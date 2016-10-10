#!/bin/bash

[ "$2" ] || { 
              echo '$1 should be the name of the profile, which is one of: ' ; 
              echo "  $(cd ~/a/git/amkhlv/profiles ; echo * )" ;
              echo '$2 should be the path to the directory where to initialize' ; 
              exit 1 ; }

mkdir "$2/schemas"
ln -s ~/a/git/amkhlv/bystroTeX/schemas/bystrotex.rnc "$2/schemas/"
ln -s ~/a/git/amkhlv/profiles/$1/defs.rkt "$2/"

cat > "$2/schemas.xml" <<'EOD'
<?xml version="1.0"?>
<locatingRules xmlns="http://thaiopensource.com/ns/locating-rules/1.0">
  <uri resource="bystrotex.xml" uri="schemas/bystrotex.rnc"/>
</locatingRules>
EOD

emacs "$2/bystrotex.xml" &
echo now $2/bystrotex.xml is opened in emacsclient
