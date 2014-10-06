#| Assignment 1 - Parsing TESTS (due Oct 11, noon) 

***Write the names and CDF accounts for each of your group members below.***
Mihai Nicolae, g1mihai
David Eysman, c3eysman
|#
#lang racket
(require "parsing.rkt")
(require test-engine/racket-tests)

#| parse-html-tag tests |#
#| error tests |#
(check-expect (parse-html-tag "") '(error ""))
(check-expect (parse-html-tag " ") '(error " "))
(check-expect (parse-html-tag " <html> ") '(error " <html> "))

(check-expect (parse-html-tag "<html></html>") '("<html>" "</html>"))
(check-expect (parse-html-tag " <html></html>") '(error " <html></html>"))
(check-expect (parse-html-tag "html></html>") '(error "html></html>"))

#|
TODO: make-text-parser tests
|#
(check-expect ((either parse-plain-char parse-html-tag) "hello") '(#\h "ello"))

#|
|#
(check-expect ((both parse-html-tag parse-plain-char) "<html>hello") '(("<html>" #\h) "ello"))
(check-expect ((both parse-html-tag parse-plain-char) "<xml>hello") '(error "<xml>hello"))
(check-expect ((both parse-html-tag parse-plain-char) "<html> hello") '(error "<html> hello"))



; TODO: WRITE TESTS!!
(test)