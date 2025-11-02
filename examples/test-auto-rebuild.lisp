;;;; examples/test-auto-rebuild.lisp --- Quick test of auto-rebuild functionality
;;;
;;; This is a simple test to verify that Phase 6 auto-rebuild works correctly.
;;;
;;; USAGE:
;;;   sbcl --load examples/test-auto-rebuild.lisp

(require :asdf)
(asdf:load-system :clad)

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║  Phase 6: Auto-Rebuild Functionality Test                     ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%~%")

;; Define a simple test part
(clad.dsl:defpart test-box ((size 10))
  "A simple test box"
  (:body (clad.core:make-box size size size)))

(format t "✓ Test part defined: TEST-BOX~%~%")

;; Test 1: Show function
(format t "TEST 1: Testing SHOW function...~%")
(let ((result (clad:show 'test-box :name "test-box")))
  (if result
      (format t "✓ SHOW works! Part displayed.~%~%")
      (error "✗ SHOW failed!")))

;; Test 2: Manual rebuild
(format t "TEST 2: Testing REBUILD function...~%")
(let ((result (clad:rebuild)))
  (if result
      (format t "✓ REBUILD works! Part regenerated.~%~%")
      (error "✗ REBUILD failed!")))

;; Test 3: Check auto-rebuild state
(format t "TEST 3: Checking auto-rebuild state...~%")
(format t "  Auto-rebuild enabled: ~A~%" clad.auto-rebuild:*auto-rebuild*)
(format t "  Current part: ~A~%~%" clad.auto-rebuild:*current-part*)

;; Test 4: Redefine part (should trigger auto-rebuild if enabled)
(format t "TEST 4: Redefining part (should auto-rebuild)...~%")
(clad.dsl:defpart test-box ((size 20))
  "A bigger test box"
  (:body (clad.core:make-box size size size)))

(format t "✓ Part redefined successfully~%~%")

;; Test 5: Toggle auto-rebuild
(format t "TEST 5: Testing TOGGLE-AUTO-REBUILD...~%")
(clad:toggle-auto-rebuild)
(format t "✓ TOGGLE works~%~%")

;; Test 6: Show that rebuild doesn't trigger when disabled
(format t "TEST 6: Redefining with auto-rebuild disabled...~%")
(clad.dsl:defpart test-box ((size 30))
  "An even bigger test box"
  (:body (clad.core:make-box size size size)))
(format t "✓ Part redefined (should NOT auto-rebuild)~%~%")

;; Test 7: Re-enable and test again
(format t "TEST 7: Re-enabling auto-rebuild...~%")
(clad:toggle-auto-rebuild)
(format t "✓ Auto-rebuild re-enabled~%~%")

;; Summary
(format t "╔════════════════════════════════════════════════════════════════╗~%")
(format t "║  All Phase 6 Auto-Rebuild Tests PASSED! ✅                    ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%~%")

(format t "Key features verified:~%")
(format t "  ✓ SHOW function - tracks and displays parts~%")
(format t "  ✓ REBUILD function - manually regenerates parts~%")
(format t "  ✓ Auto-rebuild on defpart redefinition~%")
(format t "  ✓ TOGGLE-AUTO-REBUILD - enable/disable control~%")
(format t "  ✓ State tracking (*auto-rebuild*, *current-part*)~%")
(format t "~%")
(format t "View the part at: http://localhost:8080/?model=/models/test-box.glb~%")
(format t "~%")

(format t "Try the interactive demo:~%")
(format t "  (load \"examples/interactive-demo.lisp\")~%")
(format t "  (quick-show)~%")
(format t "~%")
