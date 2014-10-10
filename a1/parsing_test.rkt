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
(check-expect (parse-non-special-char "") '(error ""))
(check-expect (parse-non-special-char "hi") '(#\h "i"))
(check-expect (parse-non-special-char "<html>") '(error "<html>"))
(check-expect (parse-non-special-char ">") '(error ">"))
(check-expect (parse-non-special-char "=") '(error "="))
(check-expect (parse-non-special-char "\"") '(error "\""))
(check-expect (parse-non-special-char "/") '(error "/"))

#| parse-plain-char tests |#
(check-expect (parse-plain-char "") '(error ""))
(check-expect (parse-plain-char "hello") '(#\h "ello"))
(check-expect (parse-plain-char " hello!") '(error " hello!"))

#| either tests |#
(check-expect ((either parse-plain-char parse-html-tag) "a") '(#\a ""))
(check-expect ((either parse-plain-char parse-html-tag) "<html>hi") '("<html>" "hi"))
(check-expect ((either parse-plain-char parse-html-tag) "hello") '(#\h "ello"))
(check-expect ((either parse-plain-char parse-html-tag) "") '(error ""))
(check-expect ((either parse-html-tag parse-plain-char) "hello") '(#\h "ello"))

#| both tests |#
(check-expect ((both parse-html-tag parse-plain-char) "<xml>hello") '(error "<xml>hello"))
(check-expect ((both parse-html-tag parse-plain-char) "<html> hello") '(error "<html> hello"))
(check-expect ((both parse-html-tag parse-plain-char) "<html>hello") '(("<html>" #\h) "ello"))
(check-expect ((both parse-html-tag parse-plain-char) "") '(error ""))
(check-expect ((both parse-non-special-char parse-non-special-char) "h") '(error "h"))

#| star tests |#
(check-expect ((star parse-plain-char) "hi") '((#\h #\i) ""))
(check-expect ((star parse-plain-char) "hi there") '((#\h #\i) " there"))
(check-expect ((star parse-plain-char) "<html>hi") '(() "<html>hi"))
(check-expect ((star parse-plain-char) "") '(() ""))

#| parse-open-tag-char tests |#
(check-expect (parse-open-tag-char "") '(error ""))
(check-expect (parse-open-tag-char "<abc>") '(#\< "abc>"))
(check-expect (parse-open-tag-char "hi") '(error "hi"))

#| parse-word tests |#
(check-expect (parse-word "") '("" ""))
(check-expect (parse-word "hello world") '("hello" " world"))
(check-expect (parse-word "abc></abc>") '("abc" "></abc>"))
(check-expect (parse-word "<p>") '("" "<p>"))

#| parse-close-tag-char tests |#
(check-expect (parse-close-tag-char "") '(error ""))
(check-expect (parse-close-tag-char "><p>") '(#\> "<p>"))
(check-expect (parse-close-tag-char "<p>") '(error "<p>"))

#| parse-attribute-pair tests |#
(check-expect (parse-attribute-pair "id=\"main\"") '(("id" "main") ""))
(check-expect (parse-attribute-pair " id = \" main  \"   ") '(("id" "main") "   "))

#| parse-whitespace tests |#
(check-expect (parse-whitespace " id") '(() "id"))
(check-expect (parse-whitespace "id") '(() "id"))

#| parse-equal-char tests |#
(check-expect (parse-equal-char "=id") '(#\= "id"))
(check-expect (parse-equal-char "id") '(error "id"))

#| parse-double-quote-char tests |#
(check-expect (parse-double-quote-char "\"id") '(#\" "id"))
(check-expect (parse-double-quote-char "id") '(error "id"))

#| parse-attributes tests |#
(check-expect (parse-attributes "id=\"main\"   <") '((("id" "main")) "<"))
(check-expect (parse-attributes "id=\"main\" id2 = \"main2\"   <") '((("id" "main") ("id2" "main2")) "<"))
(check-expect (parse-attributes ">") '(() ">"))
;(check-expect (parse-attributes "id=\"main\" id2 = \"   <") '(error "id=\"main\" id2 = \"   <"))
;(check-expect (parse-attributes "id=\"main\" = \"main2\"   <") '(error "id=\"main\" = \"main2\"   <"))

#| parse-open-tag tests |#
(check-expect (parse-open-tag "<p><") '("p" () "<"))
(check-expect (parse-open-tag "<p id=\"main\"><") '("p" (("id" "main")) "<"))
(check-expect (parse-open-tag "<p id=\"main\" id2=\"main2\"><") '("p" (("id" "main") ("id2" "main2")) "<"))
; add an error test

#| parse-text tests |#
(check-expect (parse-text "Once upon a time <") '("Once upon a time " "<"))
(check-expect (parse-text "foo bar burgers =") '("foo bar burgers " "="))
(check-expect (parse-text "<mojito") '("" "<mojito"))

#| TODO: parse-element-content tests |#
(check-expect (parse-element-content "Once upon a time <") '("Once upon a time " "<"))
;(check-expect (parse-element-content "<p></p><a>hello</a></abc>") '(("p" () "")  ("a" () "hello") "</abc>"))

#| TODO: parse-element-children tests |#
;(check-expect (parse-element-content "<p></p><b><b></abc>") '(("p" () "") ("a" () "") "") "</abc>"))
;(check-expect (parse-element-content "<p><a></a></p>") '(("p" () ("a" () "")) ""))


#| parse-matching-tag tests |#
(check-expect (parse-matching-tag "</abc>" "abc") '(() ""))
(check-expect (parse-matching-tag "<abc>" "abc") '(error "<abc>"))
(check-expect (parse-matching-tag "/abc>" "abc") '(error "/abc>"))
(check-expect (parse-matching-tag "abc>" "abc") '(error "abc>"))
(check-expect (parse-matching-tag "</abc>" "p") '(error "</abc>"))
(check-expect (parse-matching-tag "</abc" "abc") '(error "</abc"))
(check-expect (parse-matching-tag "</abc>abcd" "abc") '(() "abcd"))

#| TODO: parse-element tests |#
(check-expect (parse-element "<p id=\"main\" id2=\"main2\">Once upon a time</p>") '(("p" (("id" "main") ("id2" "main2")) "Once upon a time") ""))
(check-expect (parse-element "<p id=\"main\" id2=\"main2\">Once upon a time<p>") '(error "<p id=\"main\" id2=\"main2\">Once upon a time<p>"))
(check-expect (parse-element "<p id=\"main\" id2=\"main2\">Once upon a time</x>") '(error "<p id=\"main\" id2=\"main2\">Once upon a time</x>"))
(check-expect (parse-element "< p id=\"main\" id2=\"main2\">Once upon a time</x>") '(error "< p id=\"main\" id2=\"main2\">Once upon a time</x>"))
;(check-expect (parse-element "<p id=\"main\" id2=\"main2\"><h></h></p>") '(("p" (("id" "main") ("id2" "main2")) "<h></h>")))
;(check-expect (parse-element "<p id=\"main\" id2=\"main2\"><h>hi</h></p>") '(("p" (("id" "main") ("id2" "main2")) "<h>hi</h>")))
(check-expect (parse-element "<p></p>") '(("p" () "") ""))
(check-expect (parse-element "") '(error ""))

#| TODO: parse-html tests |#
;(check-expect (parse-html "<hi>Hello</hi><bye>Bye</bye>") '(("hi" () "Hello") "<bye>Bye</bye>"))
;(check-expect (parse-html "<html><body><h1>This is a heading!</h1><div><p>This is a paragraph.</p><h2>This is a subheading.</h2><p>This is another paragraph.</p></div></body></html>") 
;              '("html"()("body"()("p"(("id" "main") ("class" "super"))"Hey"))))
;(check-expect (parse-html "<body><p>Not good</body></p>") '(error "<body><p>Not good</body></p>"))
;(check-expect (parse-html "<html><body class=\"hello\" >Hello, world!</body></html> Other")
;              '(("html"()("body"(("class" "hello"))"Hello, world!"))" Other"))

#| chr-in-chr-lst? tests |#
(check-expect (chr-in-chr-lst? #\space '(#\" #\< #\> #\/ #\space)) #t)
(check-expect (chr-in-chr-lst? #\a '(#\h #\e #\l #\o)) #f)

#| empty-str? tests |#
(check-expect (empty-str? "") #t)
(check-expect (empty-str? "halo") #f)



(test)
