#lang racket

(require json)

(define jsn (read-json))

(define (main j)
  (cond
    [(hash? j)
     (apply
      string-append
      `(" { "
        ,@(for/list ([k (hash-keys j)])
            (format "~a = ~a, " k (main (hash-ref j k))))
        " } "
        )
       )]
    [(or (string? j) (number? j)) (jsexpr->string j)]
    [(cons? j)
     (apply
      string-append
      `(" [ "
        ,@(for/list ([x j]) (string-append (main x) ", "))
        " ] "))]
    [(null? j) "[]"]
    [(boolean? j)
     (if j "true" "false")]
    )
  )

(let-values
    ([(evl evl-stdout evl-stdin evl-stderr)
      (subprocess
       #f
       #f
       (current-error-port)
       (find-executable-path "nickel")
       "eval")]
     )
  (displayln (main jsn) evl-stdin)
  (close-output-port evl-stdin)
  (let-values
      ([(fmt fmt-stdout fmt-stdin fmt-stderr)
        (subprocess
         (current-output-port)
         evl-stdout
         (current-error-port)
         (find-executable-path "nickel")
         "format")]
       )
    (subprocess-wait evl)
    (close-input-port evl-stdout)
    (subprocess-wait fmt)))
    
         
  
      

;(displayln (main jsn))
    
