#lang racket

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:               {}
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

;;(define (zip-lists a b)
;;  (let f ([acc '()] [a a] [b b])
;;    (define-syntax-rule (join-to-acc) (append acc (list `(,(car a) ,(car b)))))
;;    (cond [(or (empty? (cdr a)) (empty? (cdr b))) (join-to-acc)]
;;          [else (f (join-to-acc) (cdr a) (cdr b))])))

(define operators '(+ - * / ^))

(define (operator? x)
  (let f ([ops operators] [x x])
    (cond
      [(empty? ops) #f]
      [(eq? (car ops) x) #t]
      [else (f (cdr ops) x)])))

(define (terms pattern expr)
  (cond
    [(not (eq? (length pattern) (length expr))) #f]
    [else (let f ([acc (hash)] [p pattern] [e expr])
            (cond
              [(empty? p) acc]
              [(and (symbol? (car p)) (not (operator? (car p)))) (f
                                                                  (hash-set acc (car p) (car e))
                                                                  (cdr p)
                                                                  (cdr e))]
              [(and (number? (car p)) (eq? (car p) (car e))) (f acc (cdr p) (cdr e))]
              [(and (operator? (car p)) (eq? (car p) (car e))) (f acc (cdr p) (cdr e))]
              [else #f]
              ))]))

(define (replace-term expr key value)
  (map (lambda (x) (cond [(eq? x key) value]
                         [(list? x) (replace-term x key value)]
                         [else x])) expr))

(define (replace-terms term-hash expr)
  (hash-for-each term-hash (lambda (x y) (set! expr (replace-term expr x y))))
  expr)

(module+ test
  (check-equal? (terms '(+ a b) '(+ 1 2)) '#hash((a . 1) (b . 2)))
  (check-equal? (replace-terms (terms '(+ a b) '(+ 1 2)) '(- a b)) '(- 1 2)))

(module+ main)
