;;; ent-test.el --- Tests for the ent build‑tool -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)   ; for cl-loop, cl-remove-if-*
(require 'ert)
(require 'ent)

(defconst test-home (file-name-directory (if load-file-name load-file-name buffer-file-name)))

(ert-deftest ent-get-all-deps-test ()
    (load-file (expand-file-name "resources/test-deps.el" test-home))
    (should (equal (ent-get-all-deps '() "t1") '("t1")))
    (should (equal (ent-get-all-deps '() "t2") '("t2" "t1")))
    (should (equal (ent-get-all-deps '() "t3") '("t3" "t2" "t1")))
    (should (equal (ent-get-all-deps '() "t4") '("t4" "t3" "t2" "t1")))


    )

(provide 'ent-test)
;;; ent-test.el ends here
