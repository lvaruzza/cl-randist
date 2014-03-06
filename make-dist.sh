version=0.4.4
if [ -d cl-randist-$version ]; then
  rm -Rf cl-randist-$version
fi

mkdir cl-randist-$version
cp  *.lisp cl-randist.asd cl-randist.html cl-randist-$version
tar czvf cl-randist-$version.tar.gz cl-randist-$version
rm -Rf cl-randist-$version
gpg -a -b cl-randist-$version.tar.gz
