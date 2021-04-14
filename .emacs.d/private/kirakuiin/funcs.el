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

(defun kirakuiin/org-sort-all-entries-by-todo ()
  "Sort all top level entries in agenda files"
  (interactive)
  (org-map-entries
    (lambda ()
      (org-sort-entries nil ?o))
    "+LEVEL=1+ITEM=\"Next Action\"" 'agenda)
  (org-map-entries
    (lambda ()
      (org-sort-entries nil ?o))
    "+LEVEL=1+ITEM=\"Appointment\"" 'agenda)
  (org-map-entries
    (lambda ()
      (org-sort-entries nil ?o))
    "+LEVEL=1+ITEM=\"Project\"" 'agenda)
  )

(defun kirakuiin/org-custom-varibles ()
  "custom org variables setting"
  (custom-set-variables '(org-agenda-custom-commands       ;; Org Agenda Commmand
                           '(("k" . "Kirakuiin commands")
                             ("kt" "Today's Task"
                              ((agenda ""
                                       ((org-agenda-span 'day)
                                        (org-agenda-entry-types '(:scheduled :deadline)) ;; 条目类型
                                        (org-agenda-skip-function '(kirakuiin/org-agenda-skip-if-only-today nil '(nottodo ("TODO"))))
                                        (org-deadline-warning-days 0))))) ;; 不显示警告
                             ("kd" "All Done LEVEL=2 Task"
                              ((tags "+LEVEL=2/+DONE"
                                     ((org-agenda-span 'month)))))))
                        '(org-stuck-projects '("+LEVEL=2/-DONE-CANCELED" ("TODO" "SCH") ("future") "")))
  )

(defun kirakuiin/org-pomodoro-finished-callback ()
  "Start a new pomodoro after a rest"
  (interactive)
  (org-pomodoro)
  )

(defun kirakuiin/org-pomodoro-hooks-on-winnt ()
  "Windows-nt alert is not work. attach some hooks to fix this"
  (when (equal system-type 'windows-nt)
    (progn
      (add-hook 'org-pomodoro-finished-hook
                (lambda () (org-notify "Rest for a while.")))
      (add-hook 'org-pomodoro-started-hook
                (lambda () (org-notify "Start pomodoro.")))
      (add-hook 'org-pomodoro-short-break-finished-hook
                (lambda ()
                  (org-notify "Start pomodoro.")
                  (kirakuiin/org-pomodoro-finished-callback)))
      (add-hook 'org-pomodoro-long-break-finished-hook
                (lambda ()
                  (org-notify "Start pomodoro.")
                  (kirakuiin/org-pomodoro-finished-callback)))
      (message "pomodoro attach hook over")
      )
    )
  )

(message "kirakuiin funcs.el loaded")
