;;; funcs.el --- kirakuiin layer funcs file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: kirakuiin <wang.zhuowei@foxmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun kirakuiin/org-agenda-skip-if-only-today (subtree conditions)
  "Like org-agenda-skip-if, but only today's entry will be display"
  (org-back-to-heading t)
  (let* ((beg (point))
         (end (if subtree (save-excursion (org-end-of-subtree t) (point))
                (org-entry-end-position)))
         (planning-end (if subtree end (line-end-position 2)))
         (dead-entry (org-entry-get nil "DEADLINE"))
         (sch-entry (org-entry-get nil "SCHEDULED"))
         (deadline-day
           (if dead-entry
             (time-to-days
               (org-time-string-to-time dead-entry))
             nil))
         (scheduled-day
           (if sch-entry
             (time-to-days
               (org-time-string-to-time sch-entry))
             nil))
         (now (time-to-days (current-time)))
         m)
    (and
      (and (or
             (setq m (memq 'nottodo conditions))
             (setq m (memq 'todo-unblocked conditions))
             (setq m (memq 'nottodo-unblocked conditions))
             (setq m (memq 'todo conditions)))
             (or
               (org-agenda-skip-if-todo m end)
               (not (or (and deadline-day (= deadline-day now))
                        (and scheduled-day (= scheduled-day now))))))
      end)))

(message "kirakuiin funcs.el loaded")
