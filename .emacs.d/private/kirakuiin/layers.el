;;; layers.el --- kirakuiin layer layers file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: kirakuiin <wang.zhuowei@foxmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

;;;###autoload
(defun kirakuiin/get-layer-path ()
  "Get the absolute path of this layer"
  (configuration-layer/get-layer-path 'kirakuiin))

;;;###autoload
(defun kirakuiin/get-res-path (subpath)
  "Get the absolute path of layer res path"
  (and (stringp subpath)
       (let ((base (kirakuiin/get-layer-path)))
         (if (string-match "^/" subpath)
           (concat base (substring subpath 1))
           (concat base subpath)))))

(configuration-layer/declare-layers
  '(
    markdown
    cmake
    multiple-cursors
    treemacs
    vimscript
    javascript
    (ipython-notebook :variables
                      ein-backend 'jupyter)
    (lua :variables
         lua-backend 'lsp
         lua-lsp-server 'lua-language-server
         lsp-clients-lua-language-server-bin "/Users/lambda/lua-language-server/bin/macOS/lua-language-server"
         lsp-clients-lua-language-server-main-location "/Users/lambda/lua-language-server/main.lua")
    (org :variables
         org-enable-github-support t
         org-habit-graph-column 1
         org-habit-preceding-days 6
         org-habit-following-days 2
         org-habit-show-all-today t
         org-directory "~/org"
         org-default-notes-file (concat org-directory "/eureka.org")
         org-attach-method 'ln
         org-attach-store-link-p 2 ;; 0.don't store 1.origin link, 2.attachmentlink
         org-agenda-include-diary t
         org-agenda-start-with-follow-mode nil ;; 关闭自动跟随, F开启
         org-link-search-must-match-exact-headline nil ;; org文件链接跳转不再仅匹配标题
         ;; org-enforce-todo-dependencies t ;; 已用edna代替
         org-enforce-todo-checkbox-dependencies t ;; 子选框未完成任务无法完成
         org-pomodoro-length 25
         org-pomodoro-short-break-length 5
         org-pomodoro-long-break-length 10
         org-pretty-entities t  ;; 默认显示上下标
         org-pretty-entities-include-sub-superscripts t
         org-use-sub-superscripts '{} ;; 强制上下标语法加大括号
         org-export-in-background t ;; 异步导出org文件
         org-hide-macro-markers t ;; 不显示宏的花括号
         org-edit-src-auto-save-idle-delay 30 ;; 30s自动保存编辑的源文件
         )
    (c-c++ :variables
           c-c++-default-mode-for-headers 'c++-mode
           c-c++-adopt-subprojects t
           c-c++-enable-clang-support t
           c-c++-enable-clang-format-on-save t
           c-c++-enable-google-newline t
           c-c++-enable-google-style t
           c-c++-backend 'lsp-clangd
           c-c++-lsp-enable-semantic-highlight 'rainbow
           lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
    (lsp :variables
         lsp-enable-indentation nil
         lsp-enable-file-watchers nil
         )
    (plantuml :variables
              plantuml-jar-path (kirakuiin/get-res-path "res/plantuml.jar")
              org-plantuml-jar-path (kirakuiin/get-res-path "/res/plantuml.jar")
              plantuml-indent-level 4
              )
    )
  )

(message "kirakuiin layers.el loaded")
;;; packages.el ends here
