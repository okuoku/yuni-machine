(import (rnrs) (shorten) (irregex) (mosh pp) (mosh file) (srfi :48) (srfi :26)
        (irregex)
        (srfi :8)
        (yuni binary packunpack bit)
        )

(define input-str (file->string "out2.txt"))
(define input-port (open-string-input-port input-str))
(define input (read input-port))

(define irx-splitop
  (irregex
    '(: (=> op (* any)) "\t" (=> operand (* any)))))

(define irx-takeintel 
  (irregex
    '(: "{" (=> att (* any)) "|" (=> intel (* any)) "}")))

(define irx-taketype
  (irregex
    '(: (=> op (* any)) "{" (=> type (* any)) "}")))

(define (decode-asmstring str)
  (define (splitop str)
    (let ((m (irregex-search irx-splitop str)))
      (if (irregex-match-data? m)
        (values (irregex-match-substring m 'op)
                (irregex-match-substring m 'operand))
        (values str #f))))
  (define (takeintel str)
    (let ((m (irregex-search irx-takeintel str)))
      (if (irregex-match-data? m)
        (irregex-match-substring m 'intel)
        str)))
  (define (taketype str)
    (let ((m (irregex-search irx-taketype str)))
      (if (irregex-match-data? m)
        (let ((op (irregex-match-substring m 'op))
              (type (irregex-match-substring m 'type)))
          (list op type))
        (list str #f))))
  (receive (op operand) (splitop str)
    (append (taketype op)
            (if operand (list (takeintel operand))
              '()))))

(define-syntax isis
  (syntax-rules ()
    ((_ sym)
     (if sym '(sym) '()))))
(define (lis2 . x)
  (list x))

(define (decode-tsflags opc num)
  (define bv (make-bytevector 8))
  (define v (make-vector 17))
  (define (dec-immt c)
    (if (= c 0)
      '()
      (list
        (case c
          ((1) 'Imm8)
          ((2) 'Imm8PCRel)
          ((3) 'Imm16)
          ((4) 'Imm16PCRel)
          ((5) 'Imm32)
          ((6) 'Imm32PCRel)
          ((7) 'Imm64)))))
  (define (dec-prefix c)
    (if (= c 0)
      '()
      (list
        (case c
          ((1) 'TB)
          ((2) 'REP)
          ((3) 'D8)
          ((4) 'D9)
          ((5) 'DA)
          ((6) 'DB)
          ((7) 'DC)
          ((8) 'DD)
          ((9) 'DE)
          ((10) 'DF)
          ((11) 'XD)
          ((12) 'XS)
          ((13) 'T8)
          ((14) 'TA)
          ((15) 'TF)))))

  (define (dec-formbits c)
    (case c
      ((0) 'Pseudo)
      ((1) 'RawFrm)
      ((2) 'AddRegFrm)
      ((3) 'MRMDestReg)
      ((4) 'MRMDestMem)
      ((5) 'MRMSrcReg)
      ((6) 'MRMSrcMem)
      ;; 16 - 23 = MRM r field
      ;; 24 - 31 = MRM m field
      ((32) 'MRMInitReg)
      ;; 33 - 38 = MRM immidiates
      ((39) '(MRM #xe8))
      ((40) '(MRM #xf0))
      ((41) '(MRM #xf8))
      ((42) '(MRM #xf9))
      ((43) 'RawFrmImm8)
      ((44) 'RawFrmImm16)
      (else
        (cond
          ((<= 16 c 23)
           `(MRM-r ,(- c 16)))
          ((<= 24 c 31)
           `(MRM-m ,(- c 24)))
          ((<= 33 c 38)
           `(MRM ,(+ #xc1 (- c 33))))
          (else 'UNKNOWN)))))
  (cond 
    (num
      (bytevector-u64-set! bv 0 num (endianness big))
      (bit-unpack-word!
        bv 0 8 big v 0
        ( 
          (unsigned 6);;FormBits
          (boolean);;hasOpsizePrefix
          (boolean);;hasAdSizePrefix
          (unsigned 4);;Prefix
          (boolean);;hasRex_Wprefix
          (unsigned 3);;ImmT
          (unsigned 3);;FPForm
          (boolean);;hasLockPrefix
          (unsigned 2);;SegOvrBits
          (unsigned 2);;exedomain
          (unsigned 8);;Opcode
          (boolean);;VEXPrefix
          (boolean);;VEX_WPrefix
          (boolean);;VEX_4VPrefix
          (boolean);;VEX_i8ImmReg
          (boolean);;VEX_L
          (boolean)
          (pad 26)
          ))
      (let ((has3DNow0F0FOpcode (vector-ref v 16))
            (hasVEX_L (vector-ref v 15))
            (hasVEX_i8ImmReg (vector-ref v 14))
            (hasVEX_4VPrefix (vector-ref v 13))
            (hasVEX_WPrefix (vector-ref v 12))
            (hasVEXPrefix (vector-ref v 11))
            (opcode (vector-ref v 10))
            (ExeDomain (vector-ref v 9))
            (SegOvrBits (vector-ref v 8))
            (hasLockPrefix (vector-ref v 7))
            (FPForm (vector-ref v 6))
            (ImmT (vector-ref v 5))
            (hasREX_WPrefix (vector-ref v 4))
            (Prefix (vector-ref v 3))
            (hasAdSizePrefix (vector-ref v 2))
            (hasOpSizePrefix (vector-ref v 1))
            (FormBits (vector-ref v 0)))
        (unless (and opc (= opc opcode))
          ;(assertion-violation #f "opc unmatch!" (list v opc opcode))
          (display (list "opc unmatch!" (list v opc opcode)) (current-error-port))
          (newline (current-error-port))
          )
        (append (isis has3DNow0F0FOpcode)
                (isis hasVEX_L)
                (isis hasVEX_i8ImmReg)
                (isis hasVEX_4VPrefix)
                (isis hasVEX_WPrefix)
                (isis hasVEXPrefix)
                (lis2 'opcode opcode)
                (lis2 'ExeDomain ExeDomain)
                (lis2 'SegOvrBits SegOvrBits)
                (isis hasLockPrefix)
                (lis2 'FPForm FPForm)
                (dec-immt ImmT)
                (isis hasREX_WPrefix)
                (dec-prefix Prefix)
                (isis hasAdSizePrefix)
                (isis hasOpSizePrefix)
                (list (dec-formbits FormBits)))
        
        ))
    (else #f)))


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
       (let ((tsflags (decode-tsflags (sel 'opcode) (sel 'tsflags))))
         (cond
           ((and tsflags (eq? 'Pseudo (car (reverse tsflags))))
            cur)
           (else
             (display (list 'convert name) (current-error-port))
             (newline (current-error-port))
             (return (append (list name (sel 'opcode))
                             (decode-asmstring (sel 'asmstring) )
                             tsflags))))))
                             
      (else cur))))

(define (out)
  (fold-left conv '() input))

(when (file-exists? "x86insts.scm")
  (delete-file "x86insts.scm"))
(with-output-to-file
  "x86insts.scm"
  (^[] (pp (out))))
