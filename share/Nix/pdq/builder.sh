
source "$stdenv"/setup

cp --recursive "$src" ./

chmod --recursive u=rwx ./"$(basename "$src")"

cd ./"$(basename "$src")"

qmake ./ 

make


mkdir --parents "$out"/bin
cp ./pdq "$out"/bin

mkdir --parents "$out"/share/applications
cp ./pdq.desktop "$out"/share/applications



