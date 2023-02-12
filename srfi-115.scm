(module srfi-115
  ()

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

(export regexp regexp? regexp-matches regexp-match? regexp-search
        regexp-fold regexp-extract regexp-split regexp-replace
        regexp-replace-all regexp-match? regexp-match-count
        regexp-match-submatch regexp-match-submatch-start
        regexp-match-submatch-end regexp-matches?
        )

(export rx regexp-match->list)

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

;; Translate things with an equivalent in (chicken irregex) and
;; raise errors for others.
(define (translate-char-set-or-boundary sym)
  (case sym
    ((symbol) '("$+<=>^`|~"))  ; ASCII only
    ((word) '(word+ any))
    ((bog eog)
     (error 'regexp "'bog' and 'eog' aren't supported"))
    ((grapheme)
     (error 'regexp "'grapheme' isn't supported"))
    (else sym)))

(define (translate-pair sre)
  (let ((tlist (lambda (lis) (map translate-sre lis)))
        (unsupported '(w/nocapture w/unicode)))

    (pmatch sre
      ((|\|| . ,ss)           `(or ,@(tlist ss)))
      ((optional . ,ss)       `(? ,@(tlist ss)))  ; SRFI 115 long names
      ((zero-or-more . ,ss)   `(* ,@(tlist ss)))
      ((one-or-more . ,ss)    `(+ ,@(tlist ss)))
      ((exactly ,n . ,ss)     `(= ,n ,@(tlist ss)))
      ((at-least ,n . ,ss)    `(>= ,n ,@(tlist ss)))
      ((repeated ,m ,n . ,ss) `(** ,m ,n ,@(tlist ss)))
      ((-> ,sym . ,ss)        `(=> ,sym ,@(tlist ss))) ; different arrow!
      ((char-set ,s)          `(,s))
      ((w/ascii . ,ss)        `(: ,@(tlist ss)))
      ((,a . ,d)
       (if (memv a unsupported)
           (error 'regexp "unsupported form" a)
           (cons a (tlist d)))))))

;; Translates a SRFI 115 SRE to an SRE compatible with CHICKEN's
;; irregex library.
(define (translate-sre sre)
  (cond ((or (string? sre) (char? sre)) sre)
        ((symbol? sre) (translate-char-set-or-boundary sre))
        ((pair? sre) (translate-pair sre))
        (else (error 'regexp "invalid SRE object" sre))))

(define (regexp re)
  (sre->irregex (translate-sre re)))

)
