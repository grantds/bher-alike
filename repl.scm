(load "interp.scm")
(define the-global-environment (setup-environment))

;; start the repl
(driver-loop)
