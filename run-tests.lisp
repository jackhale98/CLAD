;;;; Simple test runner script
(require 'asdf)
(asdf:load-asd (truename "clad.asd"))
(asdf:load-system :clad/tests)
;; Run only the shapes-tests suite to see what happens
(fiveam:run! 'clad.tests:shapes-tests)
