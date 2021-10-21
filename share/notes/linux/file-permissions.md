Default ownership for newly created files
=========================================

Suppose that I have a folder `mydir` with owner `andrei` and group `www-data`.

The following command will set a default group id for all newly created
files in `mydir`:

    chmod g+s mydir

(Obviously, I need to be added to the group `www-data`)

Default permissions for newly created files
===========================================

See [this question](https://unix.stackexchange.com/questions/1314/how-to-set-default-file-permissions-for-all-folders-files-in-a-directory)

