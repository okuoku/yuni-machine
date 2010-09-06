(import (rnrs) (mosh file) (mosh pp) (srfi :48) (shorten) (irregex))

(define irx-defhead
  (irregex '(: bos "def " (=> name (* any)) " {" (* any) eos)))

(define irx-defend
  (irregex '(: bos "}" eos)))

(define irx-defentry
  (irregex '(: bos (* whitespace) (=> type (* any)) " " (=> name (* any)) " = "
               (=> entry (* any)) ";" eos)))

(define source (file->list "X86/X86.txt"))
(define source-len (length source))

;; pogo stream
(define (make-pop source-list)
  (define cur source-list)
  (define line 0)
  (^[]
    (if (pair? cur)
      (let ((now (car cur))
            (next (cdr cur)))
        (set! line (+ 1 line))
        (set! cur next)
        (format (current-error-port) "line ~a/~a\n" line source-len)
        now)
      #f)))

(define pop (make-pop source))

(define (collect-def)
  (define (rip x)
    (let ((m (irregex-match irx-defentry x)))
      (if (irregex-match-data? m)
        (let ((type (irregex-match-substring m 'type))
              (name (irregex-match-substring m 'name))
              (entry (irregex-match-substring m 'entry)))
          (list type name entry))
        #f)))
  (define (itr cur)
    (let ((x (pop)))
      (let ((m (irregex-match irx-defend x)))
        (if (irregex-match-data? m)
          cur
          (let ((r (rip x)))
            (if r
              (itr (cons r cur))
              (itr cur)))))))
  (itr '()))

(define (collect-defs)
  (define (itr cur)
    (let ((x (pop)))
      (if x
        (let ((m (irregex-match irx-defhead x)))
          (if (irregex-match-data? m)
            (itr (cons (cons (irregex-match-substring m 'name) (collect-def)) cur))
            (itr cur)))
        cur)))
  (itr '()))

(when (file-exists? "out.txt")
  (delete-file "out.txt"))
(with-output-to-file
  "out.txt"
  (^[] (pp (collect-defs))))

