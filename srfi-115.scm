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
        (only (chicken irregex) sre->irregex)
        (rename (only (chicken irregex)
                  irregex? irregex-match-data?
                  irregex-match-substring irregex-match-start-index
                  irregex-match-end-index irregex-match-num-submatches)
          (irregex?                  regexp?)
          (irregex-match-data?       regexp-match?)
          (irregex-match-substring   regexp-match-submatch)
          (irregex-match-start-index regexp-match-submatch-start)
          (irregex-match-end-index   regexp-match-submatch-end)
          (irregex-match-num-submatches regexp-match-count)) ; ?
        (only (chicken irregex)  ; shimmed procedures
          irregex-match irregex-match? irregex-search
          irregex-fold irregex-extract irregex-split irregex-replace
          irregex-replace/all
          ))

(include "pmatch.scm")

(define-syntax rx
  (syntax-rules ()
    ((rx sre ...)
     (regexp (quasiquote (: sre ...))))))

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
    ((,s) (guard (string? s))              sre)
    ((\| . ,ss)                            `(or ,@(tlist ss)))
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

;;;; Shimmed procedures

;;; These procedures can accept both SREs and compiled regexps.
;;; The SREs must be translated. We don't compile them since
;;; uncompiled regexps are preferred for some uses.

(define (translate-re re)
  (if (regexp? re)
      re
      (translate-sre re)))

(define (regexp-matches re str . opt)
  (apply irregex-match (translate-re re) str opt))

(define (regexp-matches? re str . opt)
  (apply irregex-match? (translate-re re) str opt))

(define (regexp-search re str . opt)
  (apply irregex-search (translate-re re) str opt))

(define (regexp-fold re kons knil str . opt)
  (apply irregex-fold (translate-re re) kons knil str opt))

(define (regexp-extract re str . opt)
  (apply irregex-extract (translate-re re) str opt))

(define (regexp-split re str . opt)
  (apply irregex-split (translate-re re) str opt))

(define (regexp-replace re str subst . opt)
  (apply irregex-replace (translate-re re) str subst opt))

(define (regexp-replace-all re str subst . opt)
  (apply irregex-replace/all (translate-re re) str subst opt))

)
