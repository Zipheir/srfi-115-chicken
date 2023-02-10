(module srfi-115
  ()

(import scheme
        (chicken base)
        (chicken module))

(import (rename (chicken irregex)
          (sre->irregex              regexp)
          (irregex?                  regexp?)
          (irregex-match             regexp-matches)
          (irregex-match?            regexp-matches?)
          (irregex-search            regexp-search)
          (irregex-match-data?       regexp-match?)
          (irregex-fold              regexp-fold)
          (irregex-extract           regexp-extract)
          (irregex-split             regexp-split)
          (irregex-replace           regexp-replace)
          (irregex-replace/all       regexp-replace-all)
          (irregex-num-submatches    regexp-match-count)
          (irregex-match-substring   regexp-match-submatch)
          (irregex-match-start-index regexp-match-submatch-start)
          (irregex-match-end-index   regexp-match-submatch-end)
          (irregex-match-num-submatches regexp-match-count)  ; ?
          ))

(export regexp regexp? regexp-matches regexp-match? regexp-search
        regexp-fold regexp-extract regexp-split regexp-replace
        regexp-replace-all regexp-match? regexp-match-count
        regexp-match-submatch regexp-match-submatch-start
        regexp-match-submatch-end regex-matches?
        )

(export rx regexp-match->list)

(define-syntax rx
  (syntax-rules ()
    ((rx sre ...)
     (regexp (quote (: sre ...))))))

(define (regexp-match->list rxm)
  (letrec*
   ((end (+ 1 (regexp-match-count rxm)))
    (build
     (lambda (k)
       (if (= k end)
           '()
           (cons (regexp-match-submatch rxm k)
                 (build (+ k 1)))))))
    (build 0)))

)
