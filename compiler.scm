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

(define emit (lambda args (apply simple-format #t args)
		     (newline)))

(define (immediate-rep x)
  (cond ((integer? x) (logand (ash x fixnum-shift) #xffffffff))
	((char? x) (logior (ash (char->integer x) char-shift) char-tag))
	((boolean? x)
	 (if x
	     (logior (ash 1 bool-shift) bool-tag)
	     bool-tag))
	))

(define (compile-expr e)
  (emit "movl $~a, %eax" (immediate-rep e)))

(define (compile-and-run program)
  (begin (compile-to-binary program)
	 (system "./a.out")))

(define (compile-program program)
  (emit ".text")
  (emit ".p2align 4")
  (emit ".globl scheme_entry")
  (emit "scheme_entry:")
  (compile-expr program)
  (emit "ret"))

(define (compile-to-binary program)
  (begin
    (with-output-to-file "/tmp/compiled.s"
      (lambda () (compile-program program)))
    (system "gcc -fomit-frame-pointer /tmp/compiled.s rts.c")))
