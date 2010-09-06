(import (rnrs) (shorten) (irregex) (mosh pp) (mosh file) (srfi :48))

(define input-str (file->string "out.txt"))
(define input-port (open-string-input-port input-str))
(define input (read input-port))

(define (omit-x str omit)
  (irregex-replace/all omit str " "))

(define (string->datum str)
  (read (open-string-input-port str)))

(define (bits->number str)
  (define (output l)
    (fold-left (^[cur e]
                 (+ e (* 2 cur)))
               0 l))
  (output
    (string->datum
      (string-append "(" (omit-x str "[{},]") ")"))))

(define (conv-node n)
  (cond
    ((= (length n) 3) ; FIXME: support fields..
     (let ((type (car n))
           (name (cadr n))
           (datum (caddr n)))
       (cond
         ((or (string=? "dag" type)
              (string=? "list<dag>" type))
          (list 'dag (string->symbol name)
                (string->datum (omit-x datum ","))))
         ((and (string=? "bits<8>" type)
               (string=? "Opcode" name))
          (list 'opcode (bits->number datum)))
         ((and (string=? "AsmString" name)
               (string=? "string" type))
          (list 'asmstring (string->datum datum)))
         ((and (string=? "Format" type)
               (string=? "Form" name))
          (list 'format (string->symbol datum)))
         (else n))))
    (else
      (format (current-error-port)
              "invalid datum ~a" n)
      '())))

(define (conv x)
  (list (string->symbol (car x)) (map conv-node (cdr x))))

(define (out)
  (map conv input))

(when (file-exists? "out2.txt")
  (delete-file "out2.txt"))
(with-output-to-file
  "out2.txt"
  (^[] (pp (out))))
