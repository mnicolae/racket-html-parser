#| Assignment 1 - Parsing (due Oct 11, noon)

***Write the names and CDF accounts for each of your group members below.***
Mihai Nicolae, g1mihai
David Eysman, c3eysman
|#
#lang racket
(provide parse-html-tag make-text-parser
         parse-non-special-char parse-plain-char
         either both star
         parse-html)

#|
(parse-html-tag str)
  If str starts with "<html>", returns a pair (), where
  rest is the part of str after "<html>".
  Otherwise, returns (list 'error "hi"), signifying an error.

> (parse-html-tag "<html></html>")
'("<html>" "</html>")
> (parse-html-tag "<hey><html>")
'(error "<hey><html>")
|#
(define (parse-html-tag str)
  ((make-text-parser "<html>") str))

#|
(make-text-parser t)
  Return a parser that tries to read *one* occurrence of t at the
  start of its input.

> (define parse-hi (make-text-parser "hi"))
> (parse-hi "hiya!")
'("hi" "ya!")
> (parse-hi "goodbye hi")
'(error "goodbye hi")
|#
(define (make-text-parser t)
  (lambda (str)
    (if (check-prefix str t)
        (list t (strip-prefix str t))
        (error-handler str))))

#|
(check-prefix str pre)
  Return true if str starts with pre. Otherwise return false.
|#
(define (check-prefix str pre)
  (if (< (string-length str) (string-length pre)) #f
      (if (string=? (substring str 0 (string-length pre)) pre) #t #f)))

#|
(strip-prefix str pre)
  Return substring of str with prefix pre removed.
|#
(define (strip-prefix str pre)
  (substring str (string-length pre)))

#|
(define (error-handler str)
  Error handler function that returns (list 'error str).
|#
(define (error-handler str)
  (list 'error str))

#|
(parser-error? parser str)
  Return true if parser can succesfully parse str. Otherwise return false.
|#
(define (parser-error? parser str)
  (if (equal? (parser str) (error-handler str)) #t #f))

#|
(parsed-error? parser lst)
  Return true if the first element of lst is 'error. Otherwise return false.
|#
(define (parsed-error? lst)
  (if (equal? (list-ref lst 0) 'error) #t #f))

#|
(parse-non-special-char str)
  Try to parse *one* non-special character at the start of str.

> (parse-non-special-char "hi")
'(#\h "i")
> (parse-non-special-char "<html>")
'(error "<html>")
|#
(define (parse-non-special-char str)
  ((make-char-parser '(#\< #\> #\= #\ #\/)) str))

#|
(parse-plain-char str)
  Try to parse *one* non-special, non-white character at the start of str.

> (parse-plain-char "hi")
'(#\h "i")
> (parse-plain-char " hello!")
'(error " hello!")
|#
(define (parse-plain-char str)
  ((make-char-parser '(#\space #\< #\> #\= #\ #\/)) str))

#|
(make-char-parser chr-lst)
  Return a parser that tries to read *one* occurrence of one char from 
  chr-lst at the start of its input. chr-lst is a list of characters.

> (define parse-abc (make-text-parser '(#\a #\b #\c)))
> (parse-abc "abcde")
'("#\a" "bcde")
> (parse-hi "goodbye hi")
'(error "goodbye hi")
|#
(define (make-char-parser lst)
  (lambda (str)
    (let* ([first-chr (string-ref str 0)]
          [rest-chr (substring str 1 (string-length str))])
    (if (empty? (filter (lambda (x) (equal? first-chr x)) lst))
        (list first-chr rest-chr)
        (error-handler str)))))

#| Parsing Combinators |#

#|
(either parser1 parser2)

  Return a new parser that does the following:
    - Try to apply parser 1; if success, return that result
    - Otherwise, return the result of applying parser 2

> ((either parse-plain-char parse-html-tag) "hello")
'(#\h "ello")
> ((either parse-plain-char parse-html-tag) "<html>hello")
'("<html>" "hello")
> ((either parse-plain-char parse-html-tag) "<xml>hello")
'(error "<xml>hello")
|#
(define (either parser1 parser2)
  (lambda (str)
    (if (parser-error? parser1 str)
         (parser2 str)
         (parser1 str))))

#|
(both parser1 parser2)

  Return a new parser that does the following:
    - Apply parser1; if failure, return failure
    - Otherwise, apply parser2 to the rest of the string
      not parsed by parser1
    - If failure, emit failure, together with *original* string
    - If success, return (list data rest), where data is a *LIST*
      containing the data parsed by parser1 and parser2, in that order,
      and rest is the part of the string not parsed by either
      parser1 or parser2.

> ((both parse-html-tag parse-plain-char) "<html>hello")
'(("<html>" #\h) "ello")
> ((both parse-html-tag parse-plain-char) "<xml>hello")
'(error "<xml>hello")
> ((both parse-html-tag parse-plain-char) "<html> hello")
'(error "<html> hello")
|#
(define (both parser1 parser2)
  (lambda (str)
    (let* ([parsed1 (parser1 str)]
           [parsed2 (parser2 (second parsed1))])
    (if (parsed-error? parsed1)
        (error-handler str)
        (if (parsed-error? parsed2)
            (error-handler str)
            (list (list (first parsed1) (first parsed2)) (second parsed2)))))))

#|
(star parser)

  Return a new parser that tries to parse using parser
  0 or more times, returning as its data a list of *all*
  parsed values. This new parser should be *greedy*: it
  always uses the input parser as many times as it can,
  until it reaches the end of the string or gets an error.

  Note that the new parser never returns an error; even if
  the first attempt at parsing fails, the data returned
  is simply '().

> ((star parse-plain-char) "hi")
'((#\h #\i) "")
> ((star parse-plain-char) "hi there")
'((#\h #\i) " there")
> ((star parse-plain-char) "<html>hi")
'(() "<html>hi")
|#
(define (star parser) (void))

#| HTML Parsing |#

#|
(parse-html str)

  Parse HTML content at the beginning of str, returning (list data rest),
  where data is the tree representation of the parsed HTML specified in the
  assignment handout, and rest is the rest of str that has not been parsed.

  If the string does not start with a valid html string, return
  (list 'error str) instead.

> (parse-html "<html><body class=\"hello\" >Hello, world!</body></html> Other")
'(("html"
   ()
   ("body"
    (("class" "hello"))
    "Hello, world!"))
  " Other")
> (parse-html "<blAh></blAh>")
'(("blAh"
   ()
   "")
  "")
> (parse-html "<body><p>Not good</body></p>")
'(error "<body><p>Not good</body></p>")
|#
(define (parse-html str) (void))