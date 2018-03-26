Full path to this script
========================

From [Stack Overflow: Reliable way for a bash script to get the full path to itself?](https://stackoverflow.com/questions/4774054/reliable-way-for-a-bash-script-to-get-the-full-path-to-itself)


    SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"/$(basename "$0")

