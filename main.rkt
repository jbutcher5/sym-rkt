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

(define (terms pattern term)
  (cond
    [(not (eq? (length pattern) (length term))) #f]
    [else (let f ([acc '()] [p pattern] [t term])
            (cond
              [(empty? p) acc]
              [(and (symbol? (car p)) (not (operator? (car p)))) (f
                                                                  (append acc (list `(,(car p) ,(car t))))
                                                                  (cdr p)
                                                                  (cdr t))]
              [(and (number? (car p)) (eq? (car p) (car t))) (f acc (cdr p) (cdr t))]
              [(and (operator? (car p)) (eq? (car p) (car t))) (f acc (cdr p) (cdr t))]
              [else #f]
              ))]))

(module+ test
  (check-equal? (terms '(+ a b) '(+ 1 2)) '((a 1) (b 2))))

(module+ main)
