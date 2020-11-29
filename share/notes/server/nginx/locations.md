# Location matches

From [Official doc](http://nginx.org/en/docs/http/ngx_http_core_module.html#location) :

## Preliminaries

1. The matching is performed against a normalized URI, after decoding the text encoded in the `%xx` form, resolving references to relative path components `.` and `..`, and possible compression of two or more adjacent slashes into a single slash. 

1. A location can either be defined by a __prefix string__, or by a __regular expression__. (I suppose `prefix string` is usual string match, not regex.)

1. nginx first checks locations defined using the prefix strings (prefix locations). Among them, the location with the longest matching prefix is selected and remembered. Then regular expressions are checked, in the order of their appearance in the configuration file. The search of regular expressions terminates on the first match, and the corresponding configuration is used. If no match with a regular expression is found then the configuration of the prefix location remembered earlier is used. 

## `~*` and `~`

Regular expressions are specified with the preceding `~*` modifier 
(for case-insensitive matching), or the `~` modifier (for case-sensitive matching). 

## `^~`

If the longest matching prefix location has the `^~` modifier then regular expressions are not checked. 


`location = /smth` means exact match.

`location /smth` means URI starting with `/smth`, but then continues searching, so regexes, if match, will be preferred...

`location ^~ /smth` will match location beginning with `/smth` (as a string, not regex)



From [this answer](https://stackoverflow.com/questions/5238377/nginx-location-priority) :

    location  = / {
      # matches the query / only.
      [ configuration A ] 
    }
    location  / {
      # matches any query, since all queries begin with /, but regular
      # expressions and any longer conventional blocks will be
      # matched first.
      [ configuration B ] 
    }
    location /documents/ {
      # matches any query beginning with /documents/ and continues searching,
      # so regular expressions will be checked. This will be matched only if
      # regular expressions don't find a match.
      [ configuration C ] 
    }
    location ^~ /images/ {
      # matches any query beginning with /images/ and halts searching,
      # so regular expressions will not be checked.
      [ configuration D ] 
    }
    location ~* \.(gif|jpg|jpeg)$ {
      # matches any request ending in gif, jpg, or jpeg. However, all
      # requests to the /images/ directory will be handled by
      # Configuration D.   
      [ configuration E ] 
    }

