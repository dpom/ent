;;; test-deps.el --- config file for testing deps -*- lexical-binding: t; -*-


;;; Commentary:
;; Test file

;;; Code:


(task "t1")

(task "t2"
      :deps "t1")

(task "t3"
      :deps "t1 t2")

(task "t4"
      :deps "t3 t2")

(provide 'test-deps.el)
;;; test-deps.el ends here
