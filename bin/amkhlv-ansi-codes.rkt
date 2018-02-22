#!/usr/bin/env racket

#lang racket
(require racket/cmdline 
         truques/terminal)

(define off "\e[0m")

(printf "To cancel use N=0, i.e. ESC[0m\n\n")

(printf "In 256-color mode (ESC[38;5;<fgcode>m and ESC[48;5;<bgcode>m), the color-codes are the following\n")
(printf "0x00-0x07:  standard colors (as in ESC [ 30–37 m)\n")
(printf "0x08-0x0F:  high intensity colors (as in ESC [ 90–97 m)\n")
(printf "0x10-0xE7:  6 × 6 × 6 = 216 colors: 16 + 36 × r + 6 × g + b (0 ≤ r, g, b ≤ 5)\n")
(printf "0xE8-0xFF:  grayscale from black to white in 24 steps\n")
(printf "\033[48;5;~am\033[38;5;~am background RGB=500 foreground RGB=050 ~a\n" 
        (+ (string->number "500" 6) 16) 
        (+ (string->number "50" 6) 16) 
        off)

(printf "For \033[1mbold~a use N=1\n" off)
(printf "To \033[4munderline~a use N=4\n" off)
(printf "To \033[5mblink~a use N=5\n" off)
(printf "To \033[7mreverse~a use N=7\n" off)
(printf "To \033[51mframe~a use N=51\n" off)
(printf "To \033[52mencircle~a use N=52\n" off)
(printf "To \033[53moverline~a use N=53\n" off)
(for ([n '(11 12 13 14 15 16 17 18 19)])
  (printf "For ~a-th \033[~amalternate font~a use ~a\n" (- n 10) n off n))

(display "color table, e.g. ESC[32mESC[41m is \033[32m\033[41mlike this\033[0m:\n")
(for ([f '(0 1 2 3 4 5 6 7)])
  (for ([b '(0 1 2 3 4 5 6 7)])
    (printf "\033[~am\033[~am 3~a:4~a ~a" (+ 30 f) (+ 40 b) f b off))
  (printf "\n"))

(display "to use bold, add ;1, e.g. ESC[32;1mESC[41;1m is \033[32;1m\033[41mlike this\033[0m:\n")
(for ([f '(0 1 2 3 4 5 6 7)])
  (for ([b '(0 1 2 3 4 5 6 7)])
    (printf "\033[~a;1m\033[~a;1m 3~a:4~a ~a" (+ 30 f) (+ 40 b) f b off))
  (printf "\n"))

(printf "To cancel use N=0, i.e. ESC[0m\n\n")

(printf "To clear screen, send ESC[2J")

(printf "\033[1m\033[48;5;~am\033[38;5;~am background ~a ,,, foreground ~a ~a\n" 220 232 220 232 off)
(printf "\033[1m\033[48;5;~am\033[38;5;~am background ~a ,,, foreground ~a ~a\n" 76 232 76 232 off)

(printf "\nNow practice entering chars. Type a char:\n")
(printf "-->~a<--" (char->integer (get-one-char)))
