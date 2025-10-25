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

(define (eval-prefix-string str)
  (let ([tokens (tokenize str)])
    (when (empty? tokens)
      (error "Error: Invalid Expression")) ;; Empty input
    (let-values ([(result remaining) (parse-prefix tokens)])
      (unless (empty? remaining)
        (error "Error: Invalid Expression:" remaining));; parse-prefix tokens did not succeed
      result)))

(define (run-batch-mode)
  (let ([args (current-command-line-arguments)])
    
    (if (< (vector-length args) 2)
        (eprintf "Error: Invalid Expression") ;; batch mode requires 2 expressions
        (let ([expr-string (vector-ref args 1)])
          
          (with-handlers ([exn:fail? (lambda (ex)
                                       (eprintf "Error: ~a\n" (exn-message ex)))])
            (displayln (eval-prefix-string expr-string)))))))

;; future implementation
(define (run-interactive-mode)
  #f)


; Driver
(if (prompt?)
    (run-interactive-mode)
    (run-batch-mode))