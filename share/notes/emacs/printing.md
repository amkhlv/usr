# Using `ps-print`

Here is a setup to print two pages on an A4 paper:

    ;; 2 column landscape size 7 prints column 0-78, lines 1 to 70
    (setq ps-paper-type 'a4
          ps-font-size 7.0
          ps-print-header nil
          ps-landscape-mode t
          ps-number-of-columns 2)

Here is how to add a network printer when using Emacs on Windows:

    (setq ps-lpr-command "print"
          ps-printer-name "//FS_BSI1/HL-1270"
          printer-name "//FS_BSI1/HL-1270")
