(in-package :randist)

;;  This routine is based on the following article, with a couple of
;;  modifications which simplify the implementation.

;;      George Marsaglia, Wai Wan Tsang
;;      The Ziggurat Method for Generating Random Variables
;;      Journal of Statistical Software, vol. 5 (2000), no. 8
;;      http://www.jstatsoft.org/v05/i08/

;;  The modifications are:

;;  1) use 128 steps instead of 256 to decrease the amount of static
;;  data necessary.  

;;  2) use an acceptance sampling from an exponential wedge
;;  exp(-R*(x-R/2)) for the tail of the base strip to simplify the
;;  implementation.  The area of exponential wedge is used in
;;  calculating 'v' and the coefficients in ziggurat table, so the
;;  coefficients differ slightly from those in the Marsaglia and Tsang
;;  paper.

;;  See also Leong et al, "A Comment on the Implementation of the
;;  Ziggurat Method", Journal of Statistical Software, vol 5 (2005), no 7.

(declaim (optimize (speed 3) (debug 2) (safety 2) (space 0) (compilation-speed 0)))


;; position of right-most step 
(defconstant +R+ 3.44428647676d0)
(declaim (double-float +R+))

;; tabulated values for the heigt of the Ziggurat levels

(defparameter *ytab*
  (make-array 128 :element-type 'double-float :adjustable nil :fill-pointer nil
	      :initial-contents
  '(1.0d0              0.963598623011d0  0.936280813353d0  0.913041104253d0
    0.892278506696d0   0.873239356919d0  0.855496407634d0  0.838778928349d0 
    0.822902083699d0   0.807732738234d0   0.793171045519d0  0.779139726505d0 
    0.765577436082d0   0.752434456248d0   0.739669787677d0  0.727249120285d0 
    0.715143377413d0   0.703327646455d0   0.691780377035d0  0.68048276891d0 
    0.669418297233d0   0.65857233912d0    0.647931876189d0  0.637485254896d0 
    0.62722199145d0    0.617132611532d0   0.607208517467d0  0.597441877296d0 
    0.587825531465d0   0.578352913803d0   0.569017984198d0  0.559815170911d0 
    0.550739320877d0   0.541785656682d0   0.532949739145d0  0.524227434628d0 
    0.515614886373d0   0.507108489253d0   0.498704867478d0  0.490400854812d0 
    0.482193476986d0   0.47407993601d0    0.466057596125d0  0.458123971214d0 
    0.450276713467d0   0.442513603171d0   0.434832539473d0  0.427231532022d0 
    0.419708693379d0   0.41226223212d0    0.404890446548d0  0.397591718955d0 
    0.390364510382d0   0.383207355816d0   0.376118859788d0  0.369097692334d0 
    0.362142585282d0   0.355252328834d0   0.348425768415d0  0.341661801776d0 
    0.334959376311d0   0.328317486588d0   0.321735172063d0  0.31521151497d0 
    0.308745638367d0   0.302336704338d0   0.29598391232d0  0.289686497571d0 
    0.283443729739d0   0.27725491156d0    0.271119377649d0  0.265036493387d0 
    0.259005653912d0   0.253026283183d0   0.247097833139d0  0.241219782932d0 
    0.235391638239d0   0.229612930649d0   0.223883217122d0  0.218202079518d0 
    0.212569124201d0   0.206983981709d0   0.201446306496d0  0.195955776745d0 
    0.190512094256d0   0.185114984406d0   0.179764196185d0  0.174459502324d0 
    0.169200699492d0   0.1639876086d0     0.158820075195d0  0.153697969964d0 
    0.148621189348d0   0.143589656295d0   0.138603321143d0  0.133662162669d0 
    0.128766189309d0   0.123915440582d0   0.119109988745d0  0.114349940703d0 
    0.10963544023d0    0.104966670533d0   0.100343857232d0  0.0957672718266d0 
    0.0912372357329d0  0.0867541250127d0  0.082318375932d0  0.0779304915295d0 
    0.0735910494266d0  0.0693007111742d0  0.065060233529d0  0.0608704821745d0 
    0.056732448584d0   0.05264727098d0    0.0486162607163d0  0.0446409359769d0 
    0.0407230655415d0  0.0368647267386d0  0.0330683839378d0  0.0293369977411d0 
    0.0256741818288d0  0.0220844372634d0  0.0185735200577d0  0.0151490552854d0 
    0.0118216532614d0  0.00860719483079d0 0.00553245272614d0  0.00265435214565d0)))

;; tabulated values for 2^24 times x[i]/x[i+1] 
;; used to accept for U*x[i+1]<=x[i] without any floating point operations 
(defparameter *ktab*
  (make-array 128 :element-type 'fixnum :adjustable nil :fill-pointer nil
	      :initial-contents
	      '(
		0  12590644  14272653  14988939 
		15384584  15635009  15807561  15933577 
		16029594  16105155  16166147  16216399 
		16258508  16294295  16325078  16351831 
		16375291  16396026  16414479  16431002 
		16445880  16459343  16471578  16482744 
		16492970  16502368  16511031  16519039 
		16526459  16533352  16539769  16545755 
		16551348  16556584  16561493  16566101 
		16570433  16574511  16578353  16581977 
		16585398  16588629  16591685  16594575 
		16597311  16599901  16602354  16604679 
		16606881  16608968  16610945  16612818 
		16614592  16616272  16617861  16619363 
		16620782  16622121  16623383  16624570 
		16625685  16626730  16627708  16628619 
		16629465  16630248  16630969  16631628 
		16632228  16632768  16633248  16633671 
		16634034  16634340  16634586  16634774 
		16634903  16634972  16634980  16634926 
		16634810  16634628  16634381  16634066 
		16633680  16633222  16632688  16632075 
		16631380  16630598  16629726  16628757 
		16627686  16626507  16625212  16623794 
		16622243  16620548  16618698  16616679 
		16614476  16612071  16609444  16606571 
		16603425  16599973  16596178  16591995 
		16587369  16582237  16576520  16570120 
		16562917  16554758  16545450  16534739 
		16522287  16507638  16490152  16468907 
		16442518  16408804  16364095  16301683 
		16207738  16047994  15704248  15472926)))

;; tabulated values of 2^{-24}*x[i] 
(defparameter *wtab*
  (make-array 128 :element-type 'double-float :adjustable nil :fill-pointer nil
	      :initial-contents
	      '(1.62318314817d-08  2.16291505214d-08  2.54246305087d-08  2.84579525938d-08 
		3.10340022482d-08  3.33011726243d-08  3.53439060345d-08  3.72152672658d-08 
		3.8950989572d-08  4.05763964764d-08  4.21101548915d-08  4.35664624904d-08 
		4.49563968336d-08  4.62887864029d-08  4.75707945735d-08  4.88083237257d-08 
		5.00063025384d-08  5.11688950428d-08  5.22996558616d-08  5.34016475624d-08 
		5.44775307871d-08  5.55296344581d-08  5.65600111659d-08  5.75704813695d-08 
		5.85626690412d-08  5.95380306862d-08  6.04978791776d-08  6.14434034901d-08 
		6.23756851626d-08  6.32957121259d-08  6.42043903937d-08  6.51025540077d-08 
		6.59909735447d-08  6.68703634341d-08  6.77413882848d-08  6.8604668381d-08 
		6.94607844804d-08  7.03102820203d-08  7.11536748229d-08  7.1991448372d-08 
		7.2824062723d-08  7.36519550992d-08  7.44755422158d-08  7.52952223703d-08 
		7.61113773308d-08  7.69243740467d-08  7.77345662086d-08  7.85422956743d-08 
		7.93478937793d-08  8.01516825471d-08  8.09539758128d-08  8.17550802699d-08 
		8.25552964535d-08  8.33549196661d-08  8.41542408569d-08  8.49535474601d-08 
		8.57531242006d-08  8.65532538723d-08  8.73542180955d-08  8.8156298059d-08 
		8.89597752521d-08  8.97649321908d-08  9.05720531451d-08  9.138142487d-08 
		9.21933373471d-08  9.30080845407d-08  9.38259651738d-08  9.46472835298d-08 
		9.54723502847d-08  9.63014833769d-08  9.71350089201d-08  9.79732621669d-08 
		9.88165885297d-08  9.96653446693d-08  1.00519899658d-07  1.0138063623d-07 
		1.02247952126d-07  1.03122261554d-07  1.04003996769d-07  1.04893609795d-07 
		1.05791574313d-07  1.06698387725d-07  1.07614573423d-07  1.08540683296d-07 
		1.09477300508d-07  1.1042504257d-07  1.11384564771d-07  1.12356564007d-07 
		1.13341783071d-07  1.14341015475d-07  1.15355110887d-07  1.16384981291d-07 
		1.17431607977d-07  1.18496049514d-07  1.19579450872d-07  1.20683053909d-07 
		1.21808209468d-07  1.2295639141d-07  1.24129212952d-07  1.25328445797d-07 
		1.26556042658d-07  1.27814163916d-07  1.29105209375d-07  1.30431856341d-07 
		1.31797105598d-07  1.3320433736d-07  1.34657379914d-07  1.36160594606d-07 
		1.37718982103d-07  1.39338316679d-07  1.41025317971d-07  1.42787873535d-07 
		1.44635331499d-07  1.4657889173d-07  1.48632138436d-07  1.50811780719d-07 
		1.53138707402d-07  1.55639532047d-07  1.58348931426d-07  1.61313325908d-07 
		1.64596952856d-07  1.68292495203d-07  1.72541128694d-07  1.77574279496d-07 
		1.83813550477d-07  1.92166040885d-07  2.05295471952d-07  2.22600839893d-07)))


;; double
;; gsl_ran_gaussian_ziggurat (const gsl_rng * r, double sigma)
;; {
;;   unsigned long int i, j;
;;   int sign;
;;   double x, y;

;;   while (1)
;;     {
;;       i = gsl_rng_uniform_int (r, 256); /*  choose the step */
;;       j = gsl_rng_uniform_int (r, 16777216);  /* sample from 2^24 */
;;       sign = (i & 0x80) ? +1 : -1;
;;       i &= 0x7f;

;;       x = j * wtab[i];

;;       if (j < ktab[i])
;;         break;

;;       if (i < 127)
;;         {
;;           double y0, y1, U1;
;;           y0 = ytab[i];
;;           y1 = ytab[i + 1];
;;           U1 = gsl_rng_uniform (r);
;;           y = y1 + (y0 - y1) * U1;
;;         }
;;       else
;;         {
;;           double U1, U2;
;;           U1 = 1.0 - gsl_rng_uniform (r);
;;           U2 = gsl_rng_uniform (r);
;;           x = PARAM_R - log (U1) / PARAM_R;
;;           y = exp (-PARAM_R * (x - 0.5 * PARAM_R)) * U2;
;;         }

;;       if (y < exp (-0.5 * x * x))
;;         break;
;;     }

;;   return sign * sigma * x;
;;}

(declaim (ftype (function (double-float double-float) double-float) random-normal-ziggurat))

(defun random-normal-ziggurat (mean sigma)
" This routine is based on the following article, with a couple of
  modifications which simplify the implementation.

      George Marsaglia, Wai Wan Tsang
      The Ziggurat Method for Generating Random Variables
      Journal of Statistical Software, vol. 5 (2000), no. 8
      http://www.jstatsoft.org/v05/i08/

  The modifications are:

  1) use 128 steps instead of 256 to decrease the amount of static
  data necessary.  

  2) use an acceptance sampling from an exponential wedge
  exp(-R*(x-R/2)) for the tail of the base strip to simplify the
  implementation.  The area of exponential wedge is used in
  calculating 'v' and the coefficients in ziggurat table, so the
  coefficients differ slightly from those in the Marsaglia and Tsang
  paper.

  See also Leong et al, 'A Comment on the Implementation of the
  Ziggurat Method', Journal of Statistical Software, vol 5 (2005), no 7."

  (let ((i 0) (j 0) (sign 0) (x 0d0) (y 0d0))
    (declare (double-float mean sigma))
    (declare (double-float x y))
    (declare ((simple-array double-float (128)) *ytab*)) 
    (declare ((simple-array double-float (128)) *wtab*)) 
    (declare ((simple-array fixnum (128)) *ktab*))
    ;;(declare ((integer 32) i j sign))

    (tagbody
     try-again
       (setf i (random 256))
       (setf j (random 16777216))
       ;;(format t "~a  ~b~%" i  i)
       ;; Sign is the 8th bit of i
       (setf sign (if (ldb-test (byte 1 7) i) 1 -1))
       ;; Zerify the 8th bit of i
       (setf (ldb (byte 1 7) i) 0)
       ;;(print (list i j sign))
       ;;(format t "~b~%" i)
       (setf x (* j (coerce (aref *wtab* i) 'double-float)))

       ;; Accept x
       (when (< j (aref *ktab* i))
	 (go end))

       (if (< i 127)
	   (let ((y0 (aref *ytab* i))
		 (y1 (aref *ytab* (1+ i)))
		 (U1 (random-uniform)))
	     (setf y (+ y1 (* (- y0 y1) U1))))

	   (let ((U1 (random-pos))
		 (U2 (random-uniform)))
	     (setf x (/ (- +R+ (log U1)) +R+))
	     (setf y (* (exp (- (* +R+ (- x (/ +R+ 2))))) U2))))

       ;; Acccept x
       (when (< y (exp (- (/ (* x x) 2))))
	 (go end))
       
       (go try-again)
     end)

    (+ (* sign sigma x) mean)))

(declaim (ftype (function (&optional double-float double-float) double-float) random-normal))
(declaim (inline random-normal))
(defun random-normal (&optional (mean 0d0) (sigma 1d0))
  "Generate random variable with normal distribution using ziggurat method"
  (random-normal-ziggurat (coerce mean 'double-float)
			  (coerce sigma 'double-float)))
  

;;
;; Contributed by Joel J. Adamson <adamsonj@email.unc.edu>
;;

(declaim (ftype (function (double-float
                           double-float
                           &key (:rho (double-float -1d0 1d0))
                                (:error-limit (and unsigned-byte fixnum)))
                          (values double-float double-float &optional))
                random-normal-bivariate))
(defun random-normal-bivariate (sigma-x sigma-y &key (rho 0.0d0) (error-limit 500))
 "Return a pair of numbers with specific correlation coefficent rho
and with specified variances sigma-x and sigma-y; a direct port of
gsl_ran_bivariate_gaussian from the GNU Scientific Library:

void
gsl_ran_bivariate_gaussian (const gsl_rng * r,
                           double sigma_x, double sigma_y, double rho,
                           double *x, double *y)
\{
 double u, v, r2, scale;

 do
   {
     /* choose x,y in uniform square (-1,-1) to (+1,+1) */

     u = -1 + 2 * gsl_rng_uniform (r);
     v = -1 + 2 * gsl_rng_uniform (r);

     /* see if it is in the unit circle */
     r2 = u * u + v * v;
   }
 while (r2 > 1.0 || r2 == 0);

 scale = sqrt (-2.0 * log (r2) / r2);

 *x = sigma_x * u * scale;
 *y = sigma_y * (rho * u + sqrt(1 - rho*rho) * v) * scale;
}

I need a good test for this one.
"
  (let ((state *random-state*))
    (loop repeat error-limit do
      (let* ((u (- 1.0d0 (random 2.0d0 state)))
             ;; give me a pair of random numbers from the built-in
             ;; uniform random number generator
             ;;
             ;; dividing by 100.0 ensures that I get a number
             ;; between 0 and 1
             (v (- 1.0 (random 2.0d0 state)))
             ;; the next two terms test to see if the pair is
             ;; within the unit circle
             (radius (+ (* u u) (* v v))))
        ;; if this pair is within the unit circle, we're done
        (when (and (< 0d0 radius)
                   (<= radius 1d0))
          (let ((scale (sqrt (the (double-float -0d0)
                                  (/ (* -2d0 (log radius)) radius)))))
            (return
              (values (* sigma-x u scale)
                      (* sigma-y (+ (* rho u)
                                    (* v
                                       (sqrt (- 1.0d0
                                                (* rho rho)))))
                         scale))))))
          finally (error "Unsuccessful ~S after ~S tries."
                         'random-normal-bivariate
                         error-limit))))

