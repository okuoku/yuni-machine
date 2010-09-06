(import (rnrs) (shorten) (irregex) (mosh pp) (mosh file) (srfi :48) (srfi :26))

(define input-str (file->string "out2.txt"))
(define input-port (open-string-input-port input-str))
(define input (read input-port))

(define (select l sym)
  (let ((a (assoc sym l)))
    (if a
      (cadr a)
      #f)))

(define (select- l sym)
  (let ((a (assoc sym l)))
    (if a
      (cdr a)
      #f)))

(define (conv cur x)
  (define (return r)
    (cons r cur))
  (let ((name (car x))
        (l (cadr x)))
    (define sel (cut select l <>))
    (define sel- (cut select- l <>))
    (cond
      ((select l 'asmstring)
       (return (list name (sel 'asmstring) (sel 'opcode) (sel 'format) (sel- 'dag))))
      (else cur))))

(define (out)
  (fold-left conv '() input))

(when (file-exists? "x86insts.scm")
  (delete-file "x86insts.scm"))
(with-output-to-file
  "x86insts.scm"
  (^[] (pp (out))))
