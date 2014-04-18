(require 'amkhlv-common)

(amkhlv-interactive-replacer 
 pod2scrbl 
 '(("L<\\([^|]*\\)|http:\\([^>]*\\)>" "@hyperlink[\"http:\\2\"]{\\1}")
   ("L<\\([^|]*\\)|file://\\([^>]*\\)>" "@hyperlink[ @amkhlv/path-to-link{\\2} ]{\\1}")
   ("C<\\([^>]*\\)>" "@tt{\\1}")
   ("B<\\([^>]*\\)>" "@bold{\\1}")
   ("=head1 *\\(.*\\)" "@section{\\1}")
   ("=head2 *\\(.*\\)" "@subsection{\\1}")
   ("=head3 *\\(.*\\)" "@subsubsection{\\1}")
   )
 )


(provide 'amkhlv-pod2scrbl)