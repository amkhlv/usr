Directory listing
=================

Recursively
-----------

    (in-directory [dir use-dir?]) → sequence?

    dir : (or/c #f path-string?) = #f
    use-dir?	 	:	 	((and/c path? complete-path?) . -> . any/c)
                     =	 	(lambda (dir-path) #t)

Only top level
--------------


    (directory-list [path #:build? build?]) → (listof path?)
        path : path-string? = (current-directory)
        build? : any/c = #f

If `build?` is `#f` only gives the last element in the path

If `build?` is `#t` gives the full path (_i.e._ combined with the `path` argument)

