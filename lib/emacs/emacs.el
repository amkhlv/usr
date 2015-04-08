(modify-frame-parameters nil '((wait-for-wm . nil)))
(require 'color-theme)
(require 'thingatpt)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(setq line-number-mode t) 
(setq column-number-mode t)

(set-input-method "rfc1345")
(color-theme-initialize)
(color-theme-classic)
(set-frame-font "Terminus-16")
(set-face-font 'menu "Terminus-16")
(transient-mark-mode 1)
(scroll-bar-mode -1)
;;(menu-bar-mode -1)
(tool-bar-mode 0)
(add-to-list 'load-path "~/usr/lib/emacs/")
(add-to-list 'load-path "~/a/git/yasnippet")

(require 'amkhlv)
(require 'amkhlv-dired)
(require 'amkhlv-tex)
(require 'amkhlv-pod)
(require 'amkhlv-md)
(require 'amkhlv-scribble)
(require 'amkhlv-nxml)
;(require 'amkhlv-org)
;(require 'amkhlv-mail)
;(require 'amkhlv-html)

;(add-to-list 'load-path "~/usr/lib/emacs/color-theme-solarized/")
;(require 'color-theme-solarized)

(setq my-original-display-mm-alist '((t 410 . 257)))
;; (setq my-original-display-mm-alist (list (cons t (cons (display-mm-width) (display-mm-height)))))
;; --- I commented out because for some reason does not determine correctly...

;; This is to repair the runaway/too-big minibuffer:
;; (set-frame-parameter nil 'fullscreen 'fullboth)

;; Customize buffer switching:
(iswitchb-mode 1)
(defun myswitch () (interactive) 
  (if (get-buffer "*Buffer List*") 
      (bury-buffer "*Buffer List*")) 
  (iswitchb-buffer)
  )
(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\C-xb" 'myswitch)

;; Assign hippie-expand:
(global-set-key "\M- " 'hippie-expand)

(defun fake-stdin-slurp (filename)
  "Emulate stdin slurp using emacsclient hack"
  (switch-to-buffer (generate-new-buffer "*stdin*"))
  (insert-file filename)
  (end-of-buffer))

(defun ainsert-bar () (interactive) (ucs-insert #x2502))

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

(defmacro add-outline (hookname) 
  `(add-hook ,hookname '(lambda () (outline-minor-mode 1)
                          (local-set-key (kbd "C-c C-t") 'hide-body)
                          (local-set-key (kbd "C-c C-a") 'show-all)
                          (local-set-key (kbd "C-c p") 'outline-previous-heading)
                          (local-set-key (kbd "C-c n") 'outline-next-heading)
                          )))

(add-hook 'python-mode-hook '(lambda () (setq python-indent 4)))
(add-hook 'enriched-mode-hook '(lambda () (use-hard-newlines -1)))
;; this is because I do not want hard newlines in enriched mode --Andrei
(add-hook 'TeX-mode-hook '(lambda () (TeX-PDF-mode 1)))
(add-hook 'TeX-mode-hook '(lambda () (TeX-fold-mode 1)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c C-t") 'hide-body)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c C-a") 'show-all)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c n") 'outline-next-heading)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c p") 'outline-previous-heading)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c m") 'maximize-tex-window)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c d") 'tex-insert-date)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c v") 'ainsert-bar)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c j") 'amkhlv/jumplabel)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c u") 'mytex/jumphref)))
(add-hook 'dired-mode-hook 
  '(lambda () (local-set-key (kbd "C-c h") 'my-org-dired-update)))
(add-hook 'hack-local-variables-hook
          '(lambda () 
             (if (boundp 'andrei) 
                 (let ((b (buffer-name)))
                   (if (and 
                        (or (equal andrei 'LaTeX) (equal andrei 'XeLaTeX) (equal andrei 'LuaLaTeX))
                        (equal nil (string-match "_region_.*\.tex" b)))
                       (progn 
                         (switch-to-buffer b) 
                         (if (equal andrei 'XeLaTeX)
                             (progn 
                               (setq TeX-engine 'xetex)
                               )
                           (if (equal andrei 'LuaLaTeX)
                               (progn
                                 (switch-to-buffer b)
                                 (setq TeX-engine 'luatex)
                                 ;; (preview-buffer)
                                 ;; (switch-to-buffer b)
                                 ;; (delete-other-windows)
                                 )))))))))
(add-hook 'message-mode-hook 'mail-abbrevs-setup)
(add-hook 'pod-mode-hook '(lambda () (local-set-key (kbd "C-c C-c") 'mypod-compile)))
(add-hook 'pod-mode-hook '(lambda () (local-set-key (kbd "C-c C-v") 'mypod-view)))
(add-outline 'pod-mode-hook)

(add-hook 'html-mode-hook '(lambda () (outline-minor-mode 1)
			    (local-set-key (kbd "C-c m") 'myhtml-mathml)
			    )
	  )
(add-hook 'html-mode-hook '(lambda () (local-set-key (kbd "C-c s") 'myhtml-insert-svg)))
(add-hook 'html-mode-hook '(lambda () (local-set-key (kbd "C-c u") 'myhtml-uninsert-svg)))

(add-hook 'scribble-mode-hook '(lambda () (local-set-key (kbd "C-c C-c") 'amkhlv/scribble/compile)))
(add-hook 'scribble-mode-hook '(lambda () (local-set-key (kbd "C-c C-s") 'amkhlv/scribble/compile-htmls)))
(add-hook 'scribble-mode-hook '(lambda () (local-set-key (kbd "C-c C-v") 'amkhlv/scribble/view)))
(defun mylambda () (interactive) (ucs-insert #x3bb))
(add-hook 'scribble-mode-hook '(lambda () (local-set-key (kbd "C-c l") 'mylambda)))
(add-outline 'scribble-mode-hook)
(add-hook 'scribble-mode-hook '(lambda () (setq outline-regexp "@section\\|@subsection\\|@subsubsection\\|@slide")))
(defface scribble-slide-face
  '((((class color) (background dark)) (:inherit variable-pitch :family "Terminus" :foreground "khaki2" :weight bold :height 1.2)))
  "Basic face for highlighting the scribble slide title.")
(add-hook 'scribble-mode-hook '(lambda () (font-lock-add-keywords 'scribble-mode
      '(("@slide\\[\"\\(.*?\\)\".*\\]" 1 'scribble-slide-face prepend)
        ("@slide\\[@elem{\\(.*?\\)}.*\\]" 1 'scribble-slide-face prepend)
        ("@\\(after-pause\\)" 1 'font-lock-warning-face prepend)
        ("@\\(slide\\)" 1 'font-lock-warning-face prepend)))))
(add-hook 'scribble-mode-hook '(lambda () (local-set-key (kbd "C-c m") 'maximize-tex-window)))

(add-hook 'buffer-menu-mode-hook '(lambda () 
                                    (highlight-regexp ".*[^_]\.tex" "hi-green-b") 
                                    (highlight-regexp ".*\.scrbl" "hi-pink")
                                    ))

(add-hook 'scheme-mode-hook '(lambda () (local-set-key (kbd "C-c l") 'mylambda)))
(add-hook 'markdown-mode-hook '(lambda () 
                                 (local-set-key (kbd "C-c C-c") 'markdown-to-html)
                                 (local-set-key (kbd "C-c C-v") 'markdown-view-html)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(add-hook 'nxml-mode-hook
          (lambda()
            (hs-minor-mode 1)
            (define-key nxml-mode-map "\C-c\C-r" 'rng-reload-schema)
            ))



(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>" ;; regexp for start block
               "-->\\|</[^/>]*[^/]>" ;; regexp for end block
               "<!--"
               nxml-forward-element
               nil))




(setq TeX-outline-extra
      '(("\\\\section" 2)))

(defun aext (prog-name) (interactive "swith which program: ")
  (let* (
	 (browse-url-generic-args (cdr (split-string prog-name " " t)))
	 (browse-url-generic-program (car (split-string prog-name " " t)))
	 )
    (browse-url-generic (thing-at-point 'filename)))) 


(defun maximize-tex-window ()
(interactive)
(other-window 1)
(shrink-window (- (window-height) 4))
(other-window -1)
)

(defun tex-insert-date ()
(interactive)
(insert-string 
 (concat "{\\bf " 
     (car (split-string (shell-command-to-string "date +%Y-%m-%d\\ %H:%M") "\n"))
	 "} "
)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-indent-level 3)
 '(TeX-PDF-mode t)
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l --jobname=%s --synctex=1 %(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("View" "xpdf.real -remote %s %s.pdf" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-fold-macro-spec-list
   (quote
    (("[f]"
      ("footnote"))
     ("[c]"
      ("cite"))
     ("[l]"
      ("label"))
     ("[r]"
      ("ref" "pageref"))
     ("[i]"
      ("index"))
     ("*"
      ("item"))
     ("..."
      ("dots"))
     (1
      ("anote" "anth"))
     ("[F]"
      ("rem")))))
 '(TeX-fold-type-list (quote (env macro comment)))
 '(TeX-save-query nil)
 '(browse-url-browser-function (quote browse-url-firefox))
 '(calendar-mark-diary-entries-flag t)
 '(display-mm-dimensions-alist my-original-display-mm-alist)
 '(fill-column 75)
 '(font-latex-user-keyword-classes
   (quote
    (("rem"
      ("rem")
      (:foreground "green")
      command)
     ("andrei-remv"
      (("remv" "{"))
      (:underline "lightgreen" :foreground "white")
      command)
     ("andrei-theorem-header"
      (("anth" "{"))
      (:weight bold :underline t :foreground "green")
      command)
     ("andrei-marginal-note"
      (("anote" "{"))
      (:box
       (:line-width 2 :color "red" :style released-button)
       :foreground "yellow")
      command)
     ("andrei-underlined"
      (("underline" "{"))
      (:underline t)
      command)
     ("andrei-why"
      (("why" "{"))
      (:box
       (:line-width 2 :color "grey75" :style released-button)
       :foreground "orange")
      command)
     ("andrei-mark"
      ("am")
      (:weight bold :foreground "firebrick1")
      declaration)
     ("andrei-question"
      ("question")
      (:foreground "LightGreen")
      declaration)
     ("andrei-answer"
      ("answer")
      (:foreground "LightSalmon1")
      declaration)
     ("andrei-attn"
      (("attn" "{"))
      (:box
       (:line-width 2 :color "red" :style released-button)
       :background "yellow" :foreground "black")
      command))))
 '(inhibit-startup-screen t)
 '(ispell-local-dictionary-alist
   (quote
    (("brasileiro" "[A-Z\301\311\315\323\332\300\310\314\322\331\303\325\307\334\302\312\324a-z\341\351\355\363\372\340\350\354\362\371\343\365\347\374\342\352\364]" "[^A-Z\301\311\315\323\332\300\310\314\322\331\303\325\307\334\302\312\324a-z\341\351\355\363\372\340\350\354\362\371\343\365\347\374\342\352\364]" "[']" nil nil nil iso-8859-1))))
 '(nxml-heading-element-name-regexp "description\\|title\\|head")
 '(nxml-section-element-name-regexp
   "account\\|article\\|\\(sub\\)*section\\|chapter\\|div\\|appendix\\|part\\|preface\\|reference\\|simplesect\\|bibliography\\|bibliodiv\\|glossary\\|glossdiv")
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.3 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("MELPA" . "http://melpa.milkbox.net/packages/"))))
 '(preview-LaTeX-command
   (quote
    ("%`%l --jobname=%s \"\\nonstopmode\\nofiles\\PassOptionsToPackage{"
     ("," . preview-required-option-list)
     "}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined" preview-default-preamble "\\fi}\"%' %t")))
 '(preview-auto-cache-preamble t)
 '(preview-default-document-pt 12)
 '(preview-default-option-list
   (quote
    ("displaymath" "floats" "graphics" "textmath" "footnotes")))
 '(preview-gs-options
   (quote
    ("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4" "-dAlignToPixels=1")))
 '(preview-image-type (quote png))
 '(preview-scale-function 1.6)
 '(ps-font-size (quote (12 . 12)))
 '(quack-fontify-style (quote plt))
 '(quack-pretty-lambda-p t)
 '(quack-programs
   (quote
    ("racket" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme -M errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(safe-local-variable-values
   (quote
    ((andrei . XeLaTeX)
     (andrei . LaTeX)
     (andrei . LuaLaTeX))))
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-fold-folded-face ((((class color) (background dark)) (:foreground "green"))))
 '(diary ((((min-colors 88) (class color) (background dark)) (:foreground "chocolate1"))))
 '(diff-added ((t (:inherit diff-changed :foreground "goldenrod"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "SteelBlue2"))))
 '(fixed-pitch ((t (:height 0.9 :family "monospace"))))
 '(font-latex-bold-face ((((class color) (background dark)) (:inherit bold :foreground "Orange"))))
 '(font-latex-italic-face ((((class color) (background dark)) (:inherit italic :foreground "Orange"))))
 '(font-latex-sectioning-2-face ((((class color) (background dark)) (:inherit variable-pitch :family "Terminus" :foreground "gold1" :weight bold :height 1.1))))
 '(font-latex-sectioning-3-face ((((class color) (background dark)) (:inherit variable-pitch :family "Terminus" :foreground "khaki2" :weight bold :height 1.0))))
 '(font-latex-sectioning-4-face ((((class color) (background dark)) (:inherit variable-pitch :family "Terminus" :foreground "lightgreen" :weight bold :height 0.9))))
 '(font-latex-sectioning-5-face ((((class color) (background dark)) (:inherit variable-pitch :family "Terminus" :foreground "NavajoWhite1" :weight bold :height 0.8))))
 '(font-latex-verbatim-face ((((class color) (background dark)) (:inherit fixed-pitch :foreground "burlywood" :height 0.9))))
 '(font-lock-comment-face ((t (:foreground "chocolate1"))))
 '(font-lock-doc-string-face ((t (:foreground "chartreuse1"))))
 '(font-lock-string-face ((t (:foreground "LightGoldenrod2"))))
 '(fricas-algebra ((t (:background "gainsboro" :foreground "black"))))
 '(fricas-message ((t (:foreground "light salmon"))))
 '(fricas-undefined ((t (:background "light grey" :foreground "blue"))))
 '(pod-mode-command-text-face ((((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))))
 '(pod-mode-head2-face ((t (:inherit pod-mode-head-face :height 1.3))))
 '(pod-mode-head2-text-face ((t (:inherit pod-mode-command-text-face :height 1.3))))
 '(pod-mode-head3-face ((t (:inherit pod-mode-head-face :height 1.1))))
 '(pod-mode-head3-text-face ((t (:inherit pod-mode-command-text-face :height 1.1))))
 '(pod-mode-head4-face ((t (:inherit pod-mode-head-face :height 1.0))))
 '(pod-mode-head4-text-face ((t (:inherit pod-mode-command-text-face :height 1.0))))
 '(scribble-link-text-face ((t (:foreground "lightblue" :underline t)))))



(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq auto-mode-alist
  (append 
   '(("\\.rkt\\'" . scheme-mode) ("\\.md\\'" . markdown-mode))
   auto-mode-alist))


;;Changing resolution for formulas:
;;Note that display-mm-dimensions-alist uses the general ``dotted pair notation'' 
;; which is exlpained here:
;;http://www.gnu.org/software/emacs/elisp/html_node/Dotted-Pair-Notation.html#Dotted-Pair-Notation
;;In dotted pair notation, the list â€˜(1 2 3)â€™ is written as â€˜(1 . (2 . (3 . nil)))â€™

(defun zoomin ()
  (interactive)
  (let* (
         (my-display-orig (cdr (assoc t display-mm-dimensions-alist)))
         (old-x-mm (nth 0 my-display-orig))
         (old-y-mm (cdr my-display-orig))
         (new-x-mm (* 0.9 old-x-mm))
         (new-y-mm (* 0.9 old-y-mm))
         )
    (setq display-mm-dimensions-alist 
          (list (cons t (car (acons new-x-mm new-y-mm '())))))
    (format " %d x %d " new-x-mm new-y-mm)
    )
  )

(defun zoomout ()
  (interactive)
  (let* (
         (my-display-orig (cdr (assoc t display-mm-dimensions-alist)))
         (old-x-mm (nth 0 my-display-orig))
         (old-y-mm (cdr my-display-orig))
         (new-x-mm (/ old-x-mm 0.9))
         (new-y-mm (/ old-y-mm 0.9))
         )
    (setq display-mm-dimensions-alist 
          (list (cons t (car (acons new-x-mm new-y-mm '())))))
    (format " %d x %d " new-x-mm new-y-mm)
    )
  )

(defun zoom0 ()
  (interactive)
  (setq display-mm-dimensions-alist my-original-display-mm-alist)
  )


(require 'yasnippet) ;; not yasnippet-bundle
(setq yas-snippet-dirs
     '("~/usr/lib/emacs/snippets"))
(yas-global-mode 1)




(defun mycompile-yasnippet-bundle ()
  "This is to recompile the yasnippet tree; use after modify the snippets directory tree"
  (interactive)
  (yas/compile-bundle 
   "/usr/share/emacs/site-lisp/yasnippet/yasnippet.el" 
   "~/usr/lib/emacs/ys-bundle.el" 
   (list "~/usr/lib/emacs/snippets")
   nil
   "~/usr/lib/emacs/dropdown-list.el"
   )
  (require 'ys-bundle)
  )

(require 
 'scribble 
 (replace-regexp-in-string 
  "\n$" 
  "" 
  (shell-command-to-string 
   "find ~/.racket/planet/300/ -type f -path '*/cache/neil/scribble-emacs.plt/*/scribble.el' | head -n1 "
  )))

(require 'quack)
(require 'epa-file)
(epa-file-enable)

;; Create Cyrillic-CP1251 Language Environment menu item
(set-language-info-alist
 "Cyrillic-CP1251" `((charset cyrillic-iso8859-5)
		   (coding-system cp1251)
		   (coding-priority cp1251)
		   (input-method . "cyrillic-jcuken")
		   (features cyril-util)
		   (unibyte-display . cp1251)
		   (sample-text . "Russian (ÀãááÚØÙ)    ·ÔàÐÒáâÒãÙâÕ!")
		   (documentation . "Support for Cyrillic CP1251."))
 '("Cyrillic"))

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)
(setq org-agenda-files (list "~/a/org/"))

(autoload 'rnc-mode "rnc-mode")
(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))
