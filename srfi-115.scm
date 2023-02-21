(module srfi-115
  (regexp regexp? regexp-matches regexp-match? regexp-search
   regexp-fold regexp-extract regexp-split regexp-replace
   regexp-replace-all regexp-match? regexp-match-count
   regexp-match-submatch regexp-match-submatch-start
   regexp-match-submatch-end regexp-matches?
   rx regexp-match->list translate-sre valid-sre?)

(import scheme
        (chicken base)
        (chicken condition)
        (chicken module)
        (only (srfi 14) char-set->string)
        (only (srfi 152) string-concatenate string-concatenate-reverse)
        (only (chicken irregex) sre->irregex irregex-match-names)
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
(include "foof.scm")

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

;; This is a bit of a shotgun. (chicken irregex) needs to be more
;; specific with its exceptions.
(define (valid-sre? x)
  (condition-case (begin (regexp x) #t)
    (junk () #f)))

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
    ((= ,n . ,ss)                          `(= ,n ,@(tlist ss)))
    ((at-least ,n . ,ss)                   `(>= ,n ,@(tlist ss)))
    ((>= ,n . ,ss)                         `(>= ,n ,@(tlist ss)))
    ((repeated ,m ,n . ,ss)                `(** ,m ,n ,@(tlist ss)))
    ((** ,m ,n . ,ss)                      `(** ,m ,n ,@(tlist ss)))
    ((-> ,sym . ,ss)                       `(=> ,sym ,@(tlist ss)))
    ((char-set ,s)                         `(,s))
    ((w/ascii . ,ss)                       `(: ,@(tlist ss)))
    ((non-greedy-optional . ,ss)           `(?? ,@(tlist ss)))
    ((non-greedy-zero-or-more . ,ss)       `(*? ,@(tlist ss)))
    ((non-greedy-repeated ,m ,n . ,ss)     `(**? ,m ,n ,@(tlist ss)))
    ((**? ,m ,n . ,ss)                     `(**? ,m ,n ,@(tlist ss)))
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

;; regexp-fold passes str to knil and finish along with the
;; usual three.
(define (regexp-fold re kons knil str . opt)
  (let-optionals opt ((finish (lambda (i m s a) a))
                      (start 0)
                      (end (string-length str)))
    (irregex-fold (translate-re re)
                  (lambda (i m acc) (kons i m str acc))
                  knil
                  str
                  (lambda (i m acc) (finish i m str acc))
                  start
                  end)))

(define (regexp-extract re str . opt)
  (apply irregex-extract (translate-re re) str opt))

(define (regexp-split re str . opt)
  (apply irregex-split (translate-re re) str opt))

)
