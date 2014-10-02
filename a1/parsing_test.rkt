#| Assignment 1 - Parsing TESTS (due Oct 11, noon) 

***Write the names and CDF accounts for each of your group members below.***
Mihai Nicolae, g1mihai
<Name>, <CDF>
|#
#lang racket
(require "parsing.rkt")
(require test-engine/racket-tests)

#|
parse-html-tag tests
|#
(check-expect (parse-html-tag "") '(error ""))
(check-expect (parse-html-tag " ") '(error " "))
(check-expect (parse-html-tag "<html></html>") '("<html>" "</html>"))
(check-expect (parse-html-tag " <html></html>") '(error " <html></html>"))
(check-expect (parse-html-tag "html></html>") '(error "html></html>"))

#|
TODO: make-text-parser tests
|#

; TODO: WRITE TESTS!!
(test)