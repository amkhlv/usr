
source "$stdenv"/setup

cp --recursive "$src" ./

chmod --recursive u=rwx ./"$(basename "$src")"

cd ./"$(basename "$src")"

qmake ./ 

make


mkdir --parents "$out"/bin
cp ./ioqml "$out"/bin




