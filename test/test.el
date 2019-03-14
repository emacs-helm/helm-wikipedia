;;  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requirements

(require 'cl)
(require 'ert)

;;;; Tests

(ert-deftest helm-wikipedia--test-suggestions ()
  "Pass if Wikipedia suggestions are retrieved correctly for query \"emacs\".
This test should pass unless/until the Wikipedia suggestions for
the query change."
  (let ((helm-pattern "emacs"))
    (should
     (equal (helm-wikipedia-suggest-fetch)
            '("Emacs" "Emacs Lisp" "Emacspeak" "Emacs Speaks Statistics"
              "Emacs Lisp Package Archive" "Emacs vs vim" "Emacs schism"
              "Emacs Relay Chat" "Emac (disambiguation)" "Emacs Web Wowser")))))

(ert-deftest helm-wikipedia--test-summary ()
  "Pass if Wikipedia summary for \"Emacs\" article seems to be correct.
Passes if summary contains \"GNU Emacs\"."
  (should (string-match "GNU Emacs"
                        (helm-wikipedia--fetch-summary "Emacs"))))

(provide 'helm-wikipedia-test)
