(module defs_for-syntax racket
  (require 
   (for-syntax racket/base (planet amkhlv/bystroTeX/slides_for-syntax)))
  (provide bystro-def-formula)
   (define-syntax (bystro-def-formula stx)
     (bystro-formula-syntax 
      #:autoalign-formula-prefix "f"
      #:manual-formula-prefix    "f"
      #:display-math-prefix      "equation"
      #:size-change-notation     "fsize"
      #:size-increase-notation   "fsize+"
      #:size-restore-notation    "fsize="
      #:max-size-increase        4
      #:max-size-decrease        3
      #:max-vert-adjust          4
      stx)))
