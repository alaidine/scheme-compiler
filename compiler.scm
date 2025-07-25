(define fixnum-shift 2)
(define fixnum-mask 3)

(define ptr-mask 7) ; mask for pointer type tag
(define ptr-mask-inv #xfffffff8) ; mask for pointer value

(define pair-tag 1)
(define vec-tag 2)
(define str-tag 3)
(define sym-tag 5)
(define closure-tag 6)

(define char-mask 255) ; character type mask
(define char-shift 8)
(define char-tag 7)

(define bool-mask 255)
(define bool-shift 8)
(define bool-tag 15)

(define wordsize 4)

; Check whether the passed form is a primitive call (primcall) form
(define (primitive-call? form) (eq? 'primcall (car form)))

; Get the primitive operation from a passed primcall form
(define (primitive-op form) (cadr form))

; Get the Nth argument of a passed primcall form
(define (primitive-op-arg1 form) (caddr form))
(define (primitive-op-arg2 form) (cadddr form))

; Get all arguments of a passed primcall form
(define (primitive-op-args form) (cddr form))

(define emit (lambda args (apply simple-format #t args)
		     (newline)))

(define (emit-is-eax-equal-to val)
  (emit "cmpl $~a, %eax" val) ; check eax against val
  (emit "movl $0, %eax") ; zero eax, leaving equal flag in place
  (emit "sete %al") ; set low bit of eax if they are equal
  (emit "sall $~a, %eax" bool-shift) ; shift the bit up to the bool position
  (emit "orl $~a, %eax" bool-tag)) ; add boolean type tag

(define (immediate-rep x)
  (cond ((integer? x) (logand (ash x fixnum-shift) #xffffffff))
	((char? x) (logior (ash (char->integer x) char-shift) char-tag))
	((boolean? x)
	 (if x
	     (logior (ash 1 bool-shift) bool-tag)
	     bool-tag))
	))

(define (immediate? x) (or (integer? x) (char? x) (boolean? x) (null? x)))

(define (compile-expr e si)
  (cond
   ((immediate? e) (emit "movl $~a, %eax" (immediate-rep e)))
   ((primitive-call? e) (compile-primitive-call e si))
   ))

(define (compile-primitive-call form si)
  (case (primitive-op form)
    ; add1 - increment the first arg (fixnum) and return it
    ((add1)
     (compile-expr (primitive-op-arg1 form) si)
     (emit "addl $~a, %eax" (immediate-rep 1)))

    ; sub1 - decrement the first arg (fixnum) and return it
    ((sub1)
     (compile-expr (primitive-op-arg1 form) si)
     (emit "subl $~a, %eax" (immediate-rep 1)))

    ; integer? - check whether the first arg is an integer
    ((integer?)
     (compile-expr (primitive-op-arg1 form) si)
     (emit "andl $~a, %eax" fixnum-mask)
     (emit-is-eax-equal-to 0))

    ; boolean? - check whether the first arg is a boolean
    ((boolean?)
     (compile-expr (primitive-op-arg1 form) si)
     (emit "andl $~a, %eax" bool-mask)
     (emit-is-eax-equal-to bool-tag))

    ; char? - check whether the first arg is a character
    ((char?)
     (compile-expr (primitive-op-arg1 form) si)
     (emit "andl $~a, %eax" char-mask)
     (emit-is-eax-equal-to char-tag))

    ; zero? - check whether the first arg is the fixnum 0
    ((zero?)
     (compile-expr (primitive-op-arg1 form) si)
     (emit-is-eax-equal-to 0))

    ; + - addition of two fixnums
    ((+)
     (compile-expr (primitive-op-arg1 form) si)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr (primitive-op-arg2 form) (- si wordsize))
     (emit "addl ~a(%esp), %eax" si))

    ; - - substraction of two fixnums
    ((-)
     (compile-expr (primitive-op-arg2 form) si)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr (primitive-op-arg1 form) (- si wordsize))
     (emit "subl ~a(%esp), %eax" si))

    ; * = multiplication of two fixnums
    ((*)
     (compile-expr (primitive-op-arg1 form) si)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr (primitive-op-arg2 form) (- si wordsize))
     (emit "shrl $~a, %eax" fixnum-shift) ; make sure only one is shifted
     (emit "imull ~a(%esp), %eax" si))

    ; = - fixnum equality test
    ((=)
     (compile-expr (primitive-op-arg1 form) si)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr (primitive-op-arg2 form) (- si wordsize))
     (emit "cmpl %eax, ~a(%esp)" si)
     (emit "movl $0, %eax")
     (emit "sete %al")
     (emit "sall $~a, %eax" bool-shift)
     (emit "orl $~a, %eax" bool-tag))

    ; < - fixnum less-than test
    ((<)
     (compile-expr (primitive-op-arg1 form) si)
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr (primitive-op-arg2 form) (- si wordsize))
     (emit "cmpl %eax, ~a(%esp)" si)
     (emit "movl $0, %eax")
     (emit "setl %al")
     (emit "sall $~a, %eax" bool-shift)
     (emit "orl $~a, %eax" bool-tag))

    ; char=? - character equality test
    ((char=?)
     (compile-expr (primitive-op-arg1 form) si)
     (emit "shrl $~a, %eax" char-shift) ; remove tag bits
     (emit "movl %eax, ~a(%esp)" si)
     (compile-expr (primitive-op-arg2 form) (- si wordsize))
     (emit "shrl $~a, %eax" char-shift) ; remove tag bits
     (emit "cmpl %eax, ~a(%esp)" si)
     (emit "movl $0, %eax")
     (emit "sete %al")
     (emit "sall $~a, %eax" bool-shift)
     (emit "orl $~a, %eax" bool-tag))
    ))


(define (compile-and-run program)
  (begin (compile-to-binary program)
	 (system "./a.out")))

(define (compile-program program)
  (emit ".text")
  (emit ".p2align 4")
  (emit ".globl scheme_entry")
  (emit "scheme_entry:")
  (compile-expr program (- wordsize))
  (emit "ret"))

(define (compile-to-binary program)
  (begin
    (with-output-to-file "/tmp/compiled.s"
      (lambda () (compile-program program)))
    (system "gcc -fomit-frame-pointer -m32 /tmp/compiled.s rts.c")))
