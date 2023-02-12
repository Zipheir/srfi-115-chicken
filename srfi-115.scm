(module srfi-115
  (regexp regexp? regexp-matches regexp-match? regexp-search
   regexp-fold regexp-extract regexp-split regexp-replace
   regexp-replace-all regexp-match? regexp-match-count
   regexp-match-submatch regexp-match-submatch-start
   regexp-match-submatch-end regexp-matches?
   rx regexp-match->list translate-sre)

(import scheme
        (chicken base)
        (chicken module)
        (only (srfi 14) char-set->string)
        )

(import (only (chicken irregex) sre->irregex))

(import (rename (chicken irregex)
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

(include "pmatch.scm")

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

(define (char-set->sre cset)
  (list (char-set->string cset)))

(define unsupported '(w/nocapture w/unicode bog eog grapheme))

;; The name is a bit of a lie. This doesn't check whether sym is in the
;; SRE alphabet.
(define (validate-symbol sym)
  (if (memq sym unsupported)
      (error 'regexp "unsupported SRE form" sym)
      sym))

(define (translate-sre sre)
  (pmatch sre
    (,x (guard (or (string? x) (char? x))) x)  ; literal
    (symbol                                '("$+<=>^`|~"))  ; ASCII only
    (word                                  '(word+ any))
    (,sym (guard (symbol? sym))            (validate-symbol sym))
    ((|\|| . ,ss)                          `(or ,@(tlist ss)))
    ((optional . ,ss)                      `(? ,@(tlist ss)))
    ((zero-or-more . ,ss)                  `(* ,@(tlist ss)))
    ((one-or-more . ,ss)                   `(+ ,@(tlist ss)))
    ((exactly ,n . ,ss)                    `(= ,n ,@(tlist ss)))
    ((at-least ,n . ,ss)                   `(>= ,n ,@(tlist ss)))
    ((repeated ,m ,n . ,ss)                `(** ,m ,n ,@(tlist ss)))
    ((-> ,sym . ,ss)                       `(=> ,sym ,@(tlist ss)))
    ((char-set ,s)                         `(,s))
    ((w/ascii . ,ss)                       `(: ,@(tlist ss)))
    ((non-greedy-optional . ,ss)           `(?? ,@(tlist ss)))
    ((non-greedy-zero-or-more . ,ss)       `(*? ,@(tlist ss)))
    ((non-greedy-repeated ,m ,n . ,ss)     `(**? ,m ,n ,@(tlist ss)))
    ((,a . ,d) (guard (symbol? a))
     `(,(validate-symbol a) ,@(tlist d)))
    (else
     (error 'regexp "invalid SRE" sre))))

;; Convenient shorthand.
(define (tlist lis) (map translate-sre lis))

(define (regexp re)
  (sre->irregex (translate-sre re)))

)
