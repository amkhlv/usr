;;; amkhlv-tex2scrbl --- functions for translating LaTeX to scrbl

;; Author: Andrei Mikhailov <amkhlv@gmail.com>
;; Version: 1.0
;; Package-Requires: ((amkhlv-common))

(require 'amkhlv-common)

(defmacro build-translator (name 
                            docstring
                            leftdelim-inline 
                            rightdelim-inline 
                            leftdelim-display 
                            rightdelim-display)
    `(defun ,name ()
       ,docstring
       (interactive)
       (let* ((ml (mark-marker)) 
              (mr (point-marker))) 
         (while 
             (re-search-backward
              "\\$\\([^$]+?\\)\\$" ml t)
           (replace-match 
            (concat ,leftdelim-inline "\\1" ,rightdelim-inline)
            t)
           )
         (goto-char mr)
         (while 
             (re-search-backward
              "\\\\begin{equation}" ml t)
           (replace-match ,leftdelim-display t)
           )
         (goto-char mr)
         (while 
             (re-search-backward
              "\\\\end{equation}" ml t)
           (replace-match ,rightdelim-display t)
           )
         (goto-char mr)
         )
       )
    )

(build-translator tex2mathjax "Translates to MathJax" "\\\\(" "\\\\)" "\\\\[" "\\\\]")
(build-translator tex2j "Translates to Scribble/JLaTeXMath" "@f{" "}" "@centered[@f{" "}]")

(defmacro replacer (regex replacement mk1 mk2)
  `(progn
    (while 
        (re-search-backward ,regex ,mk1 t)
      (replace-match ,replacement t)
      )
    (goto-char ,mk2)
    )
  )
(defun align2j ()
  "Translates the LaTeX align environment"
  (interactive)
  (let* ((ml (mark-marker)) 
         (mr (point-marker))
         ) 
    (replacer "\\\\begin{align}" "@tabular[@list[\n@list[@f|{" ml mr)
    (replacer "&" "}|  @f|{" ml mr)
    (replacer "\\\\end{align}" "}|\n]]]" ml mr)
    (replacer "\\\\\\\\" "}|]  @list[ @f|{" ml mr)
    (replacer "\\\\nonumber" " " ml mr)
    (replacer "\\\\label{.*}" " " ml mr)
    )
  )

(defun eqn2j ()
  "Translates LaTeX equation to Scribble/JLaTeXMath environment"
  (interactive)
  (let* ((ml (mark-marker)) 
         (mr (point-marker))
         (txt (buffer-substring ml mr))
         (lbl (progn (string-match "\\\\label{\\(.*\\)}" txt)
                     (match-string 1 txt)))
         (l (if lbl (concat "[#:label \"" lbl "\"]") ""))
         ) 
    (replacer "\\\\begin{equation}" 
              (concat "@equation" l "|{") ml mr)
    (replacer "\\\\end{equation}" "}|" ml mr)
    (replacer "\\\\label{.*}" "" ml mr)
    )
  )

(provide 'amkhlv-tex2scrbl)
