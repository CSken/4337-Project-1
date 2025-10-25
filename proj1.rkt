#lang racket

(define (prompt?)
  (let ([args (current-command-line-arguments)])
    (cond
      [(= (vector-length args) 0) #t]
      [(string=? (vector-ref args 0) "-b") #f]
      [(string=? (vector-ref args 0) "--batch") #f]
      [else #t])))

(define (tokenize str)
  (map string->token (string-split str)))

(define (string->token str)
  (let ([num (string->number str)])
    (if num
        num
        str)))

(define (parse-prefix tokens)
  (when (empty? tokens) ;; Error occurs when input includes an operator without enough arguments
    (error "Error: Invalid Expression"))
  
  (let ([token (car tokens)]
        [rest (cdr tokens)])
    
    (cond

      [(number? token)
       (values token rest)]   
      [(member token '("+" "-" "*" "/"))
       (let*-values ([(arg1 rest1) (parse-prefix rest)] ;; recursively compmute next two arguments by prefix notation
                     [(arg2 rest2) (parse-prefix rest1)])
         (let ([result (cond
                         [(string=? token "+") (+ arg1 arg2)]
                         [(string=? token "-") (- arg1 arg2)]
                         [(string=? token "*") (* arg1 arg2)]
                         [(string=? token "/") (/ arg1 arg2)])])
           (values result rest2)))]
      
      [else (error "Error: Invalid Expression" token)]))) ;; the token is invalid

(define (eval-prefix-string str history)
  (let* ([tokens (tokenize str)]
         [tokens-with-history (replace-history-refs tokens history)])
    (when (empty? tokens-with-history)
      (error "Error: Invalid Expression"))
    (let-values ([(result remaining) (parse-prefix tokens-with-history)])
      (unless (empty? remaining)
        (error "Error: Invalid Expression")) ;; parse prefix tokens did not succeed
      (real->double-flonum result))))

(define (run-batch-mode)
  (let ([args (current-command-line-arguments)])
    
    (if (< (vector-length args) 2)
        (eprintf "Error: Invalid Expression") ;; Batch mode requires 2 arguments
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
  (map (lambda (token)
         (cond

           [(not (string? token)) token]
           
           [(<= (string-length token) 1) token]
           
           [(not (char=? (string-ref token 0) #\$)) token]
           
           [else
            (let* ([index-str (substring token 1)]
                   [index (string->number index-str)])
              (if (and (exact-integer? index) (> index 0))
                  (let ([reversed (reverse history)])
                    (if (and (>= index 1) (<= index (length reversed)))
                        (list-ref reversed (- index 1))
                        (error "Error: Invalid Expression")))
                  token))]))
       tokens))



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