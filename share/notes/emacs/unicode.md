# What character is that thing under cursor?

    (describe-char)

Sample output:

            character: • (8226, #o20042, #x2022)
    preferred charset: unicode (Unicode (ISO10646))
           code point: 0x2022
               syntax: _   which means: symbol
             category: .:Base, j:Japanese
             to input: type "&sb" with rfc1345
          buffer code: #xE2 #x80 #xA2
            file code: #xE2 #x80 #xA2 (encoded by coding system utf-8-unix)
              display: by this font (glyph code)
        xft:-xos4-Terminus-normal-normal-normal-*-20-*-*-*-c-100-iso10646-1 (#x3B)

# How to search and replace with a unicode char?

For example, we got the above output from `(describe-char)`. It says "&sb" with rfc1345. Then in the command line (after `Alt-X replace-regexp` type &sb, 
it will automatically become • . 

To replace with Carriage Return, say: `Ctrl-q Ctrl-j`

