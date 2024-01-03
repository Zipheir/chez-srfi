;; Copyright © Marc Nieper-Wißkirchen (2020).
;; Copyright © Wolfgang Corcoran-Mathe (2024)

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice (including the
;; next paragraph) shall be included in all copies or substantial
;; portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Ported to R6RS syntax-rules by wcm 2024.

;;;;;;;;;;;;
;; Syntax ;;
;;;;;;;;;;;;

(define-syntax apply/mv
  (syntax-rules ()
    ((apply/mv operator operand1 ... producer)
     (apply/mv-help operator (operand1 ...) () producer))))

;;; Pairs operands with generated temporary names & accumulates
;;; the pairs in a list, then binds all the names to values and
;;; calls the operator.
(define-syntax apply/mv-help
  (syntax-rules ()
    ((apply/mv-help operator () ((arg operand) ...) producer)
     (let-values (((proc) operator)
                  ((arg) operand) ...
                  (more-args producer))
       (apply proc arg ... more-args)))
    ((apply/mv-help operator (op1 op2 ...) (p ...) producer)
     (apply/mv-help operator (op2 ...) (p ... (arg1 op1)) producer))))

(define-syntax call/mv
  (syntax-rules ()
    ((call/mv consumer producer1 ...)
     (call/mv-help consumer (producer1 ...) ()))))

;;; Pairs producers with generated temporary names & accumulates
;;; the pairs in a list, then binds all the names to values and
;;; calls the consumer on them.
(define-syntax call/mv-help
  (syntax-rules ()
    ((call/mv-help consumer () ((args producer) ...))
     (let-values (((proc) consumer) (args producer) ...)
       (apply proc (append args ...))))
    ((call/mv-help consumer (e1 e2 ...) (p ...))
     (call/mv-help consumer (e2 ...) (p ... (args e1))))))

(define-syntax list/mv
  (syntax-rules ()
    ((list/mv element1 ... producer)
     (apply/mv list element1 ... producer))))

(define-syntax vector/mv
  (syntax-rules ()
    ((vector/mv element1 ... producer)
     (apply/mv vector element1 ... producer))))

(define-syntax box/mv
  (syntax-rules ()
    ((box/mv element1 ... producer)
     (apply/mv box element1 ... producer))))

(define-syntax value/mv
  (syntax-rules ()
    ((value/mv index operand1 ... producer)
     (apply/mv value index operand1 ... producer))))

(define-syntax coarity
  (syntax-rules ()
    ((coarity producer)
     (let-values ((res producer))
       (length res)))))

(define-syntax set!-values
  (syntax-rules ()
    ((set!-values vars producer)
     (set!-values-help vars () producer))))

;;; Pairs up the variables of the formals list with generated
;;; temporary names & accumulates the pairs in a list. Then,
;;; binds all the names to the values of the producer and
;;; sets the variables.
(define-syntax set!-values-help
  (syntax-rules ()
    ((set!-values-help () ((var temp) ...) producer)
     (let-values (((temp ... . junk) producer))
       (set! var temp) ...))
    ((set!-values-help (var1 . vars) (temp ...) producer)
     (set!-values-help vars (temp ... (var1 temp1)) producer))
    ((set!-values-help tail-var ((var temp) ...) producer)
     (let-values (((temp ... . tail-temp) producer))
       (set! var temp) ...
       (set! tail-var tail-temp)))))

(define-syntax with-values
  (syntax-rules ()
    ((with-values producer consumer)
     (apply/mv consumer producer))))

(define-syntax case-receive
  (syntax-rules ()
    ((case-receive producer clause ...)
     (with-values producer
       (case-lambda clause ...)))))

(define-syntax bind/mv
  (syntax-rules ()
    ((bind/mv producer transducer ...)
     (bind/list (list/mv producer) transducer ...))))

;;;;;;;;;;;;;;;;
;; Procedures ;;
;;;;;;;;;;;;;;;;

(define (list-values lis)
  (apply values lis))

(define (vector-values vec)
  (list-values (vector->list vec)))

(define box-values unbox)

(define (value k . objs)
  (list-ref objs k))

(define identity values)

(define compose-left
  (case-lambda
    (() identity)
    ((transducer . transducers)
     (let f ((transducer transducer) (transducers transducers))
       (if (null? transducers)
           transducer
           (let ((composition (f (car transducers) (cdr transducers))))
             (lambda args
               (apply/mv composition (apply transducer args)))))))))

(define compose-right
  (case-lambda
    (() identity)
    ((transducer . transducers)
     (let f ((transducer transducer) (transducers transducers))
       (if (null? transducers)
           transducer
           (let ((composition (f (car transducers) (cdr transducers))))
             (lambda args
               (apply/mv transducer (apply composition args)))))))))

(define (map-values proc)
  (lambda args
    (list-values (map proc args))))

(define bind/list
  (case-lambda
    ((lis) (list-values lis))
    ((lis transducer) (apply transducer lis))
    ((lis transducer . transducers)
     (apply bind/list (list/mv (apply transducer lis)) transducers))))

(define (bind/box bx . transducers)
  (apply bind/list (list/mv (unbox bx)) transducers))

(define (bind obj . transducers)
  (apply bind/list (list obj) transducers))
