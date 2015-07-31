
Edit file inplace from within script
====================================

To in-place edit three files `file1`, `file2` and `file3` from within the script (like `-pi.bak` but from inside the script), use this block:

    {
      local $^I='.bak'; # see perlvar(1)
      local @ARGV=("file1" "file2" "file3");
      while(<>){
        s/aaa/bbb/g;
        print;
      }
    }

Notice how we use use a bare block and the `local` keyword to limit the scope of our changes to the global `@ARGV` array to just here.
