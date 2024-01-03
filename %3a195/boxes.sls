;; Copyright (C) Marc Nieper-Wißkirchen (2020).  All Rights Reserved.
;; Copyright (C) Wolfgang Corcoran-Mathe (2024)

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Ported to R6RS by wcm 2024.

(library (srfi :195 boxes)
  (export box box? unbox set-box! box-arity unbox-value set-box-value!)
  (import (rnrs base (6))
          (rnrs records syntactic (6))
          (rnrs mutable-pairs (6)))

  ;; R7RS
  (define (list-set! list index obj)
    (let loop ([i index] [xs list])
      (cond [(null? xs)
             (assertion-violation 'list-set! "short list" xs index)]
            [(zero? i) (set-car! xs obj)]
            [else (loop (- i 1) (cdr xs))])))

  (define-record-type (<box> make-box box?)
    (nongenerative) (sealed #t) (opaque #t)
    (fields (mutable v* unbox* set-box*!)))

  (define (box . v*)
    (make-box v*))

  (define (unbox b)
    (apply values (unbox* b)))

  (define (set-box! b . v*)
    (set-box*! b v*))

  (define (box-arity b)
    (length (unbox* b)))

  (define (unbox-value b i)
    (list-ref (unbox* b) i))

  (define (set-box-value! b i obj)
    (list-set! (unbox* b) i obj))

  )
