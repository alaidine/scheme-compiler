(define emit (lambda args (apply simple-format #t args)
		     (newline)))

(define (compile-expr e)
  (emit "movl $~a, %eax" e))

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
