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

(defvar kirakuiin/org-pomodoro-pos-info nil
  "When start a pomodoro, record buffer and point for restore context")

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
                                     ((org-agenda-span 'month)))))
                             ("ka" "All Archive LEVEL=2 Task"
                              ((tags "ARCHIVE+LEVEL=2"
                                     ((org-agenda-archives-mode t)
                                      (org-agenda-span 'month)))))))
                        '(org-stuck-projects '("+LEVEL=2/-DONE-CANCELED" ("TODO" "SCH") ("future") ""))
                        '(org-refile-targets '((org-agenda-files :level . 1))))
  )

(defun kirakuiin/org-pomodoro-restart-pomodoro ()
  "Restart a pomodoro according to recorded buffer and point"
  (when kirakuiin/org-pomodoro-pos-info
    (let ((buffer-info (car kirakuiin/org-pomodoro-pos-info))
          (point-info (cdr kirakuiin/org-pomodoro-pos-info)))
      ;; Return to pomodoro start context and restart
      (and (gnus-buffer-exists-p buffer-info)
           (with-current-buffer buffer-info
                                (goto-char point-info)
                                (interactive)
                                (org-pomodoro))))))

(defun kirakuiin/org-pomodoro-hooks-on-winnt ()
  "Some system alert is not work. attach some hooks to fix this"
  (when (or (equal system-type 'windows-nt) (equal system-type 'darwin))
    (progn
      (add-hook 'org-pomodoro-finished-hook
                (lambda () (org-notify "Rest for a while.")))
      (add-hook 'org-pomodoro-started-hook
                (lambda ()
                  (org-notify "Start pomodoro.")
                  (let ((buffer-info (current-buffer))
                        (point-info (point)))
                    ;; Save buffer and point info
                    (setq kirakuiin/org-pomodoro-pos-info (cons buffer-info point-info)))
                  ))
      (add-hook 'org-pomodoro-short-break-finished-hook
                (lambda ()
                  (org-notify "Start pomodoro after short rest.")
                  (kirakuiin/org-pomodoro-restart-pomodoro)))
      (add-hook 'org-pomodoro-long-break-finished-hook
                (lambda ()
                  (org-notify "Start pomodoro after long rest.")
                  (kirakuiin/org-pomodoro-restart-pomodoro)))
      (add-hook 'org-pomodoro-killed-hook
                (lambda ()
                  (org-notify "Kill current pomodoro.")
                  (setq kirakuiin/org-pomodoro-pos-info nil)
                  ))
      (message "pomodoro attach hook over")
      )
    )
  )

(defun kirakuiin/org-capture-templates(&optional type)
  "According to the type, return curresponding templates
   type are:
   'eureka : Record some idea with record time
   'task : A Complete task entry
   'schedule : A task with SCHEDULED property
   'deadline : A task with DEADLINE property
   'sche&dead : A task with above two property
   'project : A Task with sub task"
  (let* ((arg (or type 'eureka))
         (eureka "** %^{HEADLINE|Eureka}\n:PROPERTIES:\n:RECORD_TIME: %U\n:END:")
         (task "** %^{TODO}p%^{PRIORITY}p%^{HEADLINE|Task} %^g")
         (schedule (concat task "\nSCHEDULED: %^{SCHEDULED}T"))
         (deadline (concat task "\nDEADLINE: %^{DEADLINE}T"))
         (sche&dead (concat schedule " DEADLINE: %^{DEADLINE}T"))
         (project (concat sche&dead "\n:PROPERTIES:\n:BLOCKER: children\n:END:\
                          \n*** TODO FIRST-CHILD\n:PROPERTIES:\
                          \n:TRIGGER:  next-sibling todo!(TODO) scheduled!(\"++0h\") chain!(\"TRIGGER\")\n:END:\
                          \n*** WAIT LAST-CHILD\n:PROPERTIES:\
                          \n:TRIGGER+: parent todo!(DONE)\n:END:"))
         (templates (list (cons 'eureka eureka) (cons 'schedule schedule)
                          (cons 'deadline deadline) (cons 'sche&dead sche&dead)
                          (cons 'task task) (cons 'project project)))
         (result (cdr (assoc type templates))))
    (or result eureka)
    )
  )

(message "kirakuiin funcs.el loaded")
