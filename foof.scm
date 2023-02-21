;; Copyright (c) 2013-2016 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Edited by wcm 2023-02.

;; This file implements forms which either aren't included in
;; (chicken irregex) or differ greatly from their CHICKEN versions.

(define (regexp-partition rx str . o)
  (let-optionals o ((start 0)
                    (end (string-length str)))
    (define (kons from md str a)
      (let ((i (regexp-match-submatch-start md 0))
            (j (regexp-match-submatch-end md 0)))
        (if (eqv? i j)
            a
            (let ((left (substring str (car a) i)))
              (cons j
                    (cons (regexp-match-submatch md 0)
                          (cons left (cdr a))))))))
    (define (final from md str a)
      (if (or (< from end) (null? (cdr a)))
          (cons (substring str (car a) end) (cdr a))
          (cdr a)))
    (reverse (regexp-fold rx kons (cons start '()) str final start end))))

(define (regexp-replace rx str subst . o)
  (let-optionals o ((start 0)
                    (maybe-end (string-length str))
                    (count 0))
    ;; end may be #f
    (let ((end (or maybe-end (string-length str))))
      (let lp ((i start) (count count))
        (let ((m (regexp-search rx str i end)))
          (cond
           ((not m) str)
           ((positive? count)
            (lp (regexp-match-submatch-end m 0) (- count 1)))
           (else
            (string-concatenate
             (cons
              (substring str start (regexp-match-submatch-start m 0))
              (append
               (reverse (regexp-apply-match m str subst start end))
               (list (substring str (regexp-match-submatch-end m 0) end))))))))))))

(define (regexp-replace-all rx str subst . o)
  (let-optionals o ((start 0)
                    (maybe-end (string-length str)))
    ;; end may be #f
    (let ((end (or maybe-end (string-length str))))
      (regexp-fold
       rx
       (lambda (i m str acc)
         (let ((m-start (regexp-match-submatch-start m 0)))
           (append (regexp-apply-match m str subst start end)
                   (if (>= i m-start)
                       acc
                       (cons (substring str i m-start) acc)))))
       '()
       str
       (lambda (i m str acc)
         (let ((end (string-length str)))
           (string-concatenate-reverse
            (if (>= i end)
                acc
                (cons (substring str i end) acc)))))
       start end))))

(define (regexp-apply-match m str ls start end)
  (let lp ((ls ls) (res '()))
    (cond
     ((null? ls)
      res)
     ((not (pair? ls))
      (lp (list ls) res))
     ((integer? (car ls))
      (lp (cdr ls) (cons (or (regexp-match-submatch m (car ls)) "") res)))
     ((procedure? (car ls))
      (lp (cdr ls) (cons ((car ls) m) res)))
     ((symbol? (car ls))
      (case (car ls)
        ((pre)
         (lp (cdr ls)
             (cons (substring str start (regexp-match-submatch-start m 0))
                   res)))
        ((post)
         (lp (cdr ls)
             (cons (substring str (regexp-match-submatch-end m 0) end)
                   res)))
        (else
         (cond
          ((assq (car ls) (irregex-match-names m))
           => (lambda (x) (lp (cons (cdr x) (cdr ls)) res)))
          (else
           (error "unknown match replacement" (car ls)))))))
     (else
      (lp (cdr ls) (cons (car ls) res))))))
