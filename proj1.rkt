#lang racket

(define (prompt?)
  (let ([args (current-command-line-arguments)])
    (cond
      [(= (vector-length args) 0) #t]
      [(string=? (vector-ref args 0) "-b") #f]
      [(string=? (vector-ref args 0) "--batch") #f]
      [else #t])))

(define (tokenize-helper chars current-token tokens)
  (cond
    ;; End of input
    [(empty? chars)
     (let ([final-tokens (if (empty? current-token)
                             tokens
                             (cons (list->string (reverse current-token)) tokens))])
       (reverse (map string->token final-tokens)))]
    
    ;; Whitespace
    [(char-whitespace? (car chars))
     (let ([new-tokens (if (empty? current-token)
                           tokens
                           (cons (list->string (reverse current-token)) tokens))])
       (tokenize-helper (cdr chars) '() new-tokens))]
    
    [(member (car chars) '(#\+ #\- #\* #\/ #\$))
     (let* ([saved-tokens (if (empty? current-token)
                              tokens
                              (cons (list->string (reverse current-token)) tokens))]
            [op-token (string (car chars))]
            [new-tokens (cons op-token saved-tokens)])
       (tokenize-helper (cdr chars) '() new-tokens))]
    
    ;; Digit / char
    [else
     (tokenize-helper (cdr chars)
                      (cons (car chars) current-token)
                      tokens)]))

(define (tokenize str)
  (let ([chars (string->list str)])
    (tokenize-helper chars '() '())))

(define (string->token str)
  (let ([num (string->number str)])
    (if num
        num
        str)))

(define (parse-prefix tokens)
  (when (empty? tokens) ;; Error occurs when input includes an operator without enough arguments
    (error "Invalid Expression"))
  
  (let ([token (car tokens)]
        [rest (cdr tokens)])
    
    (cond
      
      [(number? token)
       (values token rest)]

      [(string=? token "-")
       (when (empty? rest)
         (error "Invalid Expression"))
       (let-values ([(result rest1) (parse-prefix rest)])
         (values (- result) rest1))]
      
      [(member token '("+" "*" "/"))
       (let*-values ([(arg1 rest1) (parse-prefix rest)] ;; recursively compute next two arguments by prefix notation
                     [(arg2 rest2) (parse-prefix rest1)])
         (let ([result (cond
                         [(string=? token "+") (+ arg1 arg2)]
                         [(string=? token "*") (* arg1 arg2)]
                         [(string=? token "/") (quotient arg1 arg2)])])
           (values result rest2)))]
      
      [else (error "Invalid Expression")]))) ;; the token is invalid

(define (eval-prefix-string str history)
  (let* ([tokens (tokenize str)]
         [tokens-with-history (replace-history-refs tokens history)])
    (when (empty? tokens-with-history)
      (error "Invalid Expression"))
    (let-values ([(result remaining) (parse-prefix tokens-with-history)])
      (unless (empty? remaining)
        (error "Invalid Expression")) ;; parse prefix tokens did not succeed
      (real->double-flonum result))))

(define (run-batch-mode)
  (let ([args (current-command-line-arguments)])
    
    (if (< (vector-length args) 2)
        (eprintf "Invalid Expression") ;; Batch mode requires 2 arguments
        (let ([expr-string (vector-ref args 1)])
          
          (with-handlers ([exn:fail? (lambda (ex)
                                       (eprintf "~a\n" (exn-message ex)))])
            (displayln (eval-prefix-string expr-string '())))))))

(define (display-history history)
  (if (empty? history)
      (printf "No history yet.\n")
      (begin
        (printf "History:\n")
        (for ([result (reverse history)]
              [i (in-naturals 1)])
          (printf "  [~a] ~a\n" i result)))))

(define (replace-history-refs tokens history)
  (cond
    [(empty? tokens) '()]
    
    ;; Check if current token is "$" and next token is a number, continue parsing through tokens
    [(and (string? (car tokens))
          (string=? (car tokens) "$")
          (not (empty? (cdr tokens)))
          (number? (cadr tokens)))
     (let* ([index (cadr tokens)]
            [reversed (reverse history)])
       (if (and (exact-integer? index) (> index 0) (<= index (length reversed)))
           (cons (list-ref reversed (- index 1))
                 (replace-history-refs (cddr tokens) history))
           (error "Invalid Expression")))]
    
    ;; "$" appears alone or with non-number
    [(and (string? (car tokens))
          (string=? (car tokens) "$"))
     (error "Invalid Expression")]
    
    ;; Continue parsing through tokens
    [else
     (cons (car tokens)
           (replace-history-refs (cdr tokens) history))]))


(define (repl-loop history)
  (printf "> ")
  (flush-output)
  (let ([input (read-line)])
    
    ;; Check for end-of-file (Ctrl+D)
    (unless (eof-object? input)
      (let ([trimmed (string-trim input)])
   
        (unless (string=? trimmed "quit")
          ;; Check for history command
          (cond
            [(string=? trimmed "history")
             (display-history history)
             (repl-loop history)]
            
            ;; Skip empty lines
            [(string=? trimmed "")
             (repl-loop history)]
            
            ;; Evaluate expression
            [else
             (with-handlers ([exn:fail? (lambda (ex)
                                          (printf "Error: Invalid Expression\n")
                                          (repl-loop history))])
               (let* ([result (eval-prefix-string trimmed history)]
                      [new-history (cons result history)])
                 (display-history new-history)
                 (repl-loop new-history)))]))))))


(define (run-interactive-mode)
  (repl-loop '())
  )


; Driver
(if (prompt?)
    (run-interactive-mode)
    (run-batch-mode))