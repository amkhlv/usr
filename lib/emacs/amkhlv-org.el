(provide 'amkhlv-org)

(defun my-org-shapka ()
(interactive)
  (insert "% -*- andrei: LaTeX -*-
\\documentclass[12pt]{article}
\\input{/home/andrei/a/other/emacs/tex-outline-hat.tex}
\\begin{document} \n\n\n\n\\end{document}")
(previous-line 2)
)

(defun my-japanese-shapka ()
(interactive)
  (insert "% (set-input-method 'japanese)
\\documentclass[12pt]{article}
\\input{/home/andrei/a/other/emacs/tex-outline-hat.tex}

\\usepackage{CJK}
\\begin{document} 
\\begin{CJK*}[dnp]{JIS}{min}
\\begin{document} \n\n\n\n\\end{CJK*}
\\end{document}

% Local Variables: 
% coding: euc-jp
% End:

")
(previous-line 9)
)
