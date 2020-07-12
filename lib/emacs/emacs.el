
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(setq gnutls-min-prime-bits 2048
      gnutl-verify-error t)

(require 'package)
;(add-to-list 'package-archives '("melpa_local" . "/home/andrei/melpa/"))
;(add-to-list 'package-archives '("melpa_local" . "/home/andrei/melpa/"))
                                        ;(package-initialize)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/"))
               ;;'("melpa-stable" . "https://stable.melpa.org/packages/")
               t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

(add-to-list 'load-path "~/usr/lib/emacs/elisp/")
;(add-to-list 'load-path "~/usr/lib/emacs/elisp/auctex-12.1/")
(add-to-list 'load-path "~/a/git/yasnippet/")
(add-to-list 'load-path "~/a/git/rust-mode/")
;; needed for racket-mode:
;;(add-to-list 'load-path "~/melpa/s.el/")
;;(add-to-list 'load-path "~/melpa/faceup/")
;;(add-to-list 'load-path "~/melpa/racket-mode/")
;; needed for intero:
;; (add-to-list 'load-path "~/elpa/")
;; (add-to-list 'load-path "~/elpa/seq-2.20/")
;; (add-to-list 'load-path "~/melpa/dash.el/")
;; (add-to-list 'load-path "~/melpa/company-mode/")
;; (add-to-list 'load-path "~/melpa/epl/")
;; (add-to-list 'load-path "~/melpa/pkg-info.el/")
;; (add-to-list 'load-path "~/melpa/flycheck/")
;; (add-to-list 'load-path "~/melpa/haskell-mode/")
;; (add-to-list 'load-path "~/melpa/intero/elisp/")


(require 'thingatpt)
(require 'racket-mode)
(require 'yaml-mode)
(require 'yasnippet) ;; not yasnippet-bundle
(setq yas-snippet-dirs '("~/usr/lib/emacs/elisp/snippets"))
(yas-global-mode 1)
;(require 'scribble)
(require 'epa-file)
(setenv "GPG_AGENT_INFO" nil)
(autoload 'rnc-mode "rnc-mode")
(require 'markdown-mode)
(require 'bystroTeX-preview)
(require 'bystroTeX-utils)

(require 'use-package)
(use-package flycheck
  :init (global-flycheck-mode))
(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (haskell-mode . lsp)
         (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
  :config (setq lsp-prefer-flymake nil))
(use-package haskell-mode :mode "\\.hs$")
(setq lsp-haskell-process-path-hie "hie-wrapper")

;; For Scala:
;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")
;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)
;; Enable nice rendering of diagnostics like compile errors.
(use-package company-lsp)
(use-package lsp-treemacs
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t)
  )


;;------------- AUCTeX -----------------
;; (setq dpi
;;       (car (with-temp-buffer
;;              (insert-file-contents "~/.local/boi/dpi")
;;              (split-string (buffer-string) "\n" t))))

;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
;;--------------------------------------

;;------ File Backups ------------------
(defvar my-backup-directory "~/emacs-backups/")
(if (not (file-exists-p my-backup-directory))
        (make-directory my-backup-directory t))
(setq backup-directory-alist `((".*" . ,my-backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash nil
      kept-old-versions 2               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 2               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 300            ; number of keystrokes between auto-saves (default: 300)
      )


(modify-frame-parameters nil '((wait-for-wm . nil)))

(setq w32-lwindow-modifier 'super) ; Left Windows key

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq line-number-mode t)
(setq column-number-mode t)

(set-input-method "rfc1345")

(set-frame-font "Terminus-12")
(set-face-font 'menu "Terminus-12")
(transient-mark-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode 0)

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



(ido-mode 'buffers)

;; Assign hippie-expand:
(global-set-key "\M- " 'hippie-expand)

(defun toggle-fullscreen ()
  "Toggle full screen on X11."
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f8] 'amkhlv/toggle-folding)
(global-set-key [f11] 'toggle-fullscreen)


(defmacro add-outline (hookname) 
  `(add-hook ,hookname '(lambda () (outline-minor-mode 1)
                          (local-set-key (kbd "C-c C-t") 'hide-body)
                          (local-set-key (kbd "C-c C-a") 'show-all)
                          (local-set-key (kbd "C-c p") 'outline-previous-heading)
                          (local-set-key (kbd "C-c n") 'outline-next-heading)
                          )))

(defun xsel () (interactive) (insert (shell-command-to-string "xsel")))
(global-set-key (kbd "<s-mouse-2>") (quote xsel))
(define-key racket-mode-map (kbd "]") nil)
(define-key racket-mode-map (kbd ")") nil)
(define-key racket-mode-map (kbd "}") nil)
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (local-set-key (kbd "<s-mouse-2>") (quote xsel))))

(add-hook 'python-mode-hook '(lambda () (setq python-indent 4)))

(add-hook 'TeX-mode-hook '(lambda () (TeX-PDF-mode 1)))
(add-hook 'TeX-mode-hook '(lambda () (TeX-fold-mode 1)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c C-t") 'hide-body)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c C-a") 'show-all)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c n") 'outline-next-heading)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c p") 'outline-previous-heading)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c m") 'maximize-tex-window)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c d") 'tex-insert-date)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c j") 'amkhlv/jumplabel)))
(add-hook 'TeX-mode-hook '(lambda () (local-set-key (kbd "C-c u") 'mytex/jumphref)))
(setq TeX-outline-extra '(("\\\\section" 2)))
(add-hook 'message-mode-hook 'mail-abbrevs-setup)
(add-hook 'pod-mode-hook '(lambda () (local-set-key (kbd "C-c C-c") 'mypod-compile)))
(add-hook 'pod-mode-hook '(lambda () (local-set-key (kbd "C-c C-v") 'mypod-view)))
(add-outline 'pod-mode-hook)
(add-hook 'scribble-mode-hook '(lambda () (local-set-key (kbd "C-c C-c") 'amkhlv/scribble/compile)))
(add-hook 'racket-mode-hook '(lambda () (local-set-key (kbd "C-c C-c") 'amkhlv/scribble/compile)))
(add-hook 'racket-mode-hook '(lambda () (local-set-key (kbd "C-c C-v") 'amkhlv/scribble/view)))
(add-hook 'racket-mode-hook '(lambda () (local-set-key (kbd "<C-tab>") 'bystroTeX-toggle-preview-and-recenter)))
(add-hook 'racket-mode-hook '(lambda () (local-set-key (kbd "<C-M-tab>") 'bystroTeX-reveal)))
(add-hook 'racket-mode-hook '(lambda () (local-set-key (kbd "C-`") 'bystroTeX-unindent)))
(defun mylambda () (interactive) (ucs-insert #x3bb))
(add-hook 'scribble-mode-hook '(lambda () (local-set-key (kbd "C-c l") 'mylambda)))
(add-outline 'scribble-mode-hook)
(add-hook 'scribble-mode-hook '(lambda () (setq outline-regexp "@section\\|@subsection\\|@subsubsection\\|@slide\\|@page\\|@subpage")))
(add-outline 'racket-mode-hook)
(add-hook 'racket-mode-hook '(lambda () (setq outline-regexp "@section\\|@subsection\\|@subsubsection\\|@slide\\|@page\\|@subpage")))
(defface scribble-slide-face
  '((((class color) (background dark)) (:inherit variable-pitch :family "Terminus" :foreground "khaki2" :weight bold :height 1.3)))
  "Basic face for highlighting the scribble slide title.")
(add-hook 'scribble-mode-hook
          '(lambda ()
             (font-lock-add-keywords 'scribble-mode
                                     '(("@page\\[\"\\(.*?\\)\".*\\]" 1 'scribble-slide-face prepend)
                                       ("@page\\[@elem{\\(.*?\\)}.*\\]" 1 'scribble-slide-face prepend)
                                       ("@slide\\[\"\\(.*?\\)\".*\\]" 1 'scribble-slide-face prepend)
                                       ("@slide\\[@elem{\\(.*?\\)}.*\\]" 1 'scribble-slide-face prepend)
                                       ("@\\(after-pause\\)" 1 'font-lock-warning-face prepend)
                                       ("@\\(slide\\)" 1 'font-lock-warning-face prepend)
                                       ("@\\(page\\)" 1 'font-lock-warning-face prepend)
                                       ))))
(defface scribble-section-face
  '((((class color) (background dark)) (:inherit variable-pitch :family "Terminus" :foreground "khaki2" :weight bold :height 1.4)))
  "Basic face for highlighting the scribble section title.")
(defface scribble-subsection-face
  '((((class color) (background dark)) (:inherit variable-pitch :family "Terminus" :foreground "khaki2" :weight bold :height 1.2)))
  "Basic face for highlighting the scribble subsection title.")
(defface scribble-subsubsection-face
  '((((class color) (background dark)) (:inherit variable-pitch :family "Terminus" :foreground "khaki2" :weight bold )))
  "Basic face for highlighting the scribble subsection title.")
(add-hook 'racket-mode-hook
          '(lambda ()
             (font-lock-add-keywords 'racket-mode
                                     '(("@page\\[\"\\(.*?\\)\".*\\]" 1 'scribble-slide-face prepend)
                                       ("@page\\[@elem{\\(.*?\\)}.*\\]" 1 'scribble-slide-face prepend)
                                       ("@slide\\[\"\\(.*?\\)\".*\\]" 1 'scribble-slide-face prepend)
                                       ("@slide\\[@elem{\\(.*?\\)}.*\\]" 1 'scribble-slide-face prepend)
                                       ("@title\\(\\[.*\\]\\)*?{\\([^}]*?\\)}" 2 'scribble-section-face prepend)
                                       ("@section\\(\\[.*\\]\\)?{\\([^}]*?\\)}" 2 'scribble-section-face prepend)
                                       ("@subsection\\(\\[.*\\]\\)?{\\([^}]*?\\)}" 2 'scribble-subsection-face prepend)
                                       ("@subsubsection\\(\\[.*\\]\\)?{\\([^}]*?\\)}" 2 'scribble-subsubsection-face prepend)
                                       ("@\\(after-pause\\)" 1 'font-lock-warning-face prepend)
                                       ("@\\(slide\\)" 1 'font-lock-warning-face prepend)
                                       ("@\\(page\\)" 1 'font-lock-warning-face prepend)
                                       ("@subpage\\[1\s-*\"\\(.*?\\)\".*\\]"    1 'scribble-section-face prepend)
                                       ("@subpage\\[1\s-*@elem{\\(.*?\\)}.*\\]" 1 'scribble-section-face prepend)
                                       ("@subpage\\[2\s-*\"\\(.*?\\)\".*\\]"    1 'scribble-subsection-face prepend)
                                       ("@subpage\\[2\s-*@elem{\\(.*?\\)}.*\\]" 1 'scribble-subsection-face prepend)
                                       ("@subpage\\[3\s-*\"\\(.*?\\)\".*\\]"    1 'scribble-subsubsection-face prepend)
                                       ("@subpage\\[3\s-*@elem{\\(.*?\\)}.*\\]" 1 'scribble-subsubsection-face prepend)
                                       ))))
(add-hook 'scribble-mode-hook '(lambda () (local-set-key (kbd "C-c m") 'maximize-tex-window)))

(add-hook 'buffer-menu-mode-hook '(lambda () 
                                    (highlight-regexp ".*[^_]\.tex" 'hi-green-b) 
                                    (highlight-regexp ".*\.scrbl" 'hi-pink)
                                    ))

(add-hook 'markdown-mode-hook '(lambda () 
                                 (local-set-key (kbd "C-c C-c") 'markdown-to-html)
                                 (local-set-key (kbd "C-c C-v") 'markdown-view-html)))

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
 '(custom-enabled-themes (quote (deeper-blue)))
 '(initial-buffer-choice t)
 '(package-selected-packages
   (quote
    (color-theme lsp-treemacs company-lsp yasnippet yaml-mode use-package scala-mode sbt-mode racket-mode lsp-ui lsp-haskell flycheck))))

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

(set-face-attribute 'font-lock-comment-face nil :background nil :foreground "dark orange")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq auto-mode-alist
      (append 
       '(
         ("\\.rkt\\'" . racket-mode)
         ("\\.scrbl\\'" . racket-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.rnc\\'" . rnc-mode)
         ("\\.pdq\\'" . nxml-mode)
         ("\\.rs\\'" . rust-mode)
         ("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)
         )
       auto-mode-alist))

;;Changing resolution for formulas:
;;Note that display-mm-dimensions-alist uses the general ``dotted pair notation'' 
;; which is exlpained here:
;;http://www.gnu.org/software/emacs/elisp/html_node/Dotted-Pair-Notation.html#Dotted-Pair-Notation
;;In dotted pair notation, the list ‘(1 2 3)’ is written as ‘(1 . (2 . (3 . nil)))’
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



