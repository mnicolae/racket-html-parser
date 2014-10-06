#| Assignment 1 - Parsing TESTS (due Oct 11, noon) 

***Write the names and CDF accounts for each of your group members below.***
Mihai Nicolae, g1mihai
David Eysman, c3eysman
|#
#lang racket
(require "parsing.rkt")
(require test-engine/racket-tests)

#| parse-html-tag tests |#
(check-expect (parse-html-tag "") '(error ""))
(check-expect (parse-html-tag " ") '(error " "))
(check-expect (parse-html-tag "html></html>") '(error "html></html>"))
(check-expect (parse-html-tag " <html> ") '(error " <html> "))
(check-expect (parse-html-tag "< html> ") '(error "< html> "))
(check-expect (parse-html-tag "<html > ") '(error "<html > "))
(check-expect (parse-html-tag "<hi><html>") '(error "<hi><html>"))
(check-expect (parse-html-tag "<html></html>") '("<html>" "</html>"))
(check-expect (parse-html-tag "<html><p></p></html>") '("<html>" "<p></p></html>"))
(check-expect (parse-html-tag "html></html>") '(error "html></html>"))

#| parse-non-special char tests |#
(check-expect (parse-non-special-char "hi") '(#\h "i"))
(check-expect (parse-non-special-char "<html>") '(error "<html>"))

#| parse-plain-char tests |#
(check-expect (parse-plain-char "hello") '(#\h "ello"))
(check-expect (parse-plain-char " hello!") '(error " hello!"))

#| either tests |#
(check-expect ((either parse-plain-char parse-html-tag) "hello") '(#\h "ello"))

#| both tests |#
(check-expect ((both parse-html-tag parse-plain-char) "<xml>hello") '(error "<xml>hello"))
(check-expect ((both parse-html-tag parse-plain-char) "<html> hello") '(error "<html> hello"))
(check-expect ((both parse-html-tag parse-plain-char) "<html>hello") '(("<html>" #\h) "ello"))

#| TODO: star tests |#

(test)