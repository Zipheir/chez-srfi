;; Copyright (C) 2020 Wolfgang Corcoran-Mathe

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Ported to R6RS by wcm 2023.

(library (srfi :189)
  (export maybe? either? just nothing left right nothing? just? maybe= left?
          right? either= either-swap

          maybe-ref maybe-ref/default either-ref either-ref/default

          maybe-join maybe-bind maybe-compose either-join either-bind
          either-compose

          maybe-length maybe-filter maybe-remove either-length
          either-filter either-remove
          maybe-sequence either-sequence

          maybe->either either->maybe list->just list->right maybe->list
          either->list maybe->truth either->truth truth->maybe maybe->values
          maybe->two-values values->maybe either->values values values->either
          two-values->maybe maybe-for-each either-for-each maybe->generation
          generation->maybe list->left list->maybe list->either
          maybe->list-truth either->list-truth list-truth->maybe
          list-truth->either truth->either
          either->generation generation->either
          exception->either either-guard

          maybe-map maybe-fold maybe-unfold either-map either-fold
          either-unfold

          tri-not tri=? tri-and tri-or tri-merge

          maybe-and maybe-or maybe-let* either-and either-or either-let*
          maybe-let*-values either-let*-values
          maybe-if)
  (import (except (rnrs base (6)) error)
          (rename (only (rnrs base (6)) error)
            (error r6rs:error))
          (rnrs control (6))
          (rnrs records syntactic (6))
          (rnrs exceptions (6))
          (only (rnrs io ports (6)) eof-object eof-object?)
          (only (srfi :1 lists) find list= every list-copy)
          (srfi :145 assumptions)
          (only (chezscheme) include))

  ;;; R7RS shim
  (define (error message . irritants)
    (apply r6rs:error #f message irritants))

  (include ":189/189.scm"))
