;;; org-roam-more-utils.el --- Utility functions for org-roam-more -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; 本文件提供 org-roam-more 的通用工具函数。
;;
;; 主要功能：
;; - 链接生成和格式化
;; - 标题插入和层级管理
;; - 节点信息获取
;; - 大纲路径（OLP）操作
;; - 标题位置查找
;;
;; 函数列表：
;; * 链接工具
;;   - `org-roam-more-node-link' - 生成 org-roam 节点链接
;;   - `org-roam-more-format-link' - 格式化节点链接
;;   - `org-roam-more--link-make-string' - 底层链接构造函数
;;
;; * 标题插入
;;   - `org-roam-more-insert-subheading' - 插入子标题（自动层级）
;;   - `org-roam-more-insert-heading-with-level' - 插入指定层级标题
;;
;; * 节点信息
;;   - `org-roam-more-get-current-node-id-title' - 获取当前节点 ID 和标题
;;   - `org-roam-more-get-current-path' - 获取当前条目的大纲路径
;;
;; * 大纲路径操作
;;   - `org-roam-more-heading-to-olp' - 标题转大纲路径
;;   - `org-roam-more-subheadings-under-olp' - 获取 OLP 下的子标题
;;   - `org-roam-more-get-headline-pos-by-path' - 根据路径查找标题位置
;;   - `org-roam-more-get-original-id-at-path' - 根据路径获取 ORIGINAL-ID
;;
;; * 辅助函数
;;   - `org-roam-more--string-empty-p' - 检查字符串是否为空
;;
;;; Code:

(require 'org)
(require 'org-element)
(require 'org-roam)

;;; 辅助函数

(defsubst org-roam-more--string-empty-p (string)
  "Check whether STRING is empty."
  (string= string ""))

;;; 链接工具

(defun org-roam-more-node-link (node)
  "Return an Org link string for the given NODE."
  (format "[[id:%s][%s]]"
          (org-roam-node-id node)
          (org-roam-node-title node)))

(defun org-roam-more-format-link (node)
  "Return an Org-roam link string for NODE in the format [[id:...][title]]."
  (let ((id (org-roam-node-id node))
        (title (org-roam-node-title node)))
    (format "[[id:%s][%s]]" id title)))

(defun org-roam-more--link-make-string (link &optional description)
  "Make a bracket link, consisting of LINK and DESCRIPTION.
LINK is escaped with backslashes for inclusion in buffer."
  (let* ((zero-width-space (string ?\x200B))
	 (description
	  (and (org-string-nw-p description)
	       ;; Description cannot contain two consecutive square
	       ;; brackets, or end with a square bracket.  To prevent
	       ;; this, insert a zero width space character between
	       ;; the brackets, or at the end of the description.
	       (replace-regexp-in-string
		"\\(]\\)\\(]\\)"
		(concat "\\1" zero-width-space "\\2")
		(replace-regexp-in-string "]\\'"
					  (concat "\\&" zero-width-space)
					  (org-trim description))))))
    (if (not (org-string-nw-p link))
        (or description
            (error "Empty link"))
      (format "[[%s]%s]"
	      (org-link-escape link)
	      (if description (format "[%s]" description) "")))))

;;; 标题插入

(defun org-roam-more-insert-subheading (&optional heading)
  "Insert a subheading at current point.
In Org mode uses asterisks, in Markdown mode uses hashes.
If HEADING is not provided, prompt for it."
  (interactive)
  (unless (derived-mode-p 'org-mode 'markdown-mode)
    (user-error "Not in Org or Markdown mode"))

  (let* ((current-level
          (cond ((derived-mode-p 'org-mode)
                 (or (save-excursion
                       (when (re-search-backward "^\\(\\*+\\) " nil t)
                         (length (match-string 1))))
                     1))
                ((derived-mode-p 'markdown-mode)
                 (or (save-excursion
                       (when (re-search-backward "^\\(#+\\) " nil t)
                         (length (match-string 1))))
                     1))))
         (subheading-level (1+ current-level))
         (heading-prefix
          (cond ((derived-mode-p 'org-mode) (make-string subheading-level ?*))
                ((derived-mode-p 'markdown-mode) (make-string subheading-level ?#)))))

    (unless heading
      (setq heading (read-string "Subheading: ")))
    (insert heading-prefix " " heading "\n")))

(defun org-roam-more-insert-heading-with-level (level &optional heading)
  "Insert a subheading at current point.
In Org mode uses asterisks, in Markdown mode uses hashes.
If HEADING is not provided, prompt for it."
  (interactive)
  (unless (derived-mode-p 'org-mode 'markdown-mode)
    (user-error "Not in Org or Markdown mode"))

  (let ((heading-prefix
          (cond ((derived-mode-p 'org-mode) (make-string level ?*))
                ((derived-mode-p 'markdown-mode) (make-string level ?#)))))

    (unless heading
      (setq heading (read-string "Subheading: ")))
    (insert heading-prefix " " heading "\n")))

;;; 节点信息

(defun org-roam-more-get-current-node-id-title ()
  "在当前光标所在的 org headline 处，获取节点 ID 和标题。
如果当前 buffer 不是 org-mode，返回 nil 并显示提示。
如果没有得到 id 或标题，返回 nil。"
  (if (not (derived-mode-p 'org-mode))
      (progn
        (message "当前不在 Org 文件中")
        nil)
    (let ((title (org-get-heading t t t t)) ; 纯标题，无标签，无todo，无脚注等
          (id (org-id-get)))
      (if (and title id (not (org-roam-more--string-empty-p title)) (not (org-roam-more--string-empty-p id)))
          (cons id title)
        (progn
          (message "未能获取有效的 ID 或标题")
          nil)))))

(defun org-roam-more-get-current-path ()
  "返回当前 Org 条目的 OLP（Outline Path）.
也就是标题层级列表。包含当前标题，
从最顶层到当前条目的顺序排列。"
  (let ((olp (list (org-get-heading t t t t)))) ; 先加入当前标题
    (save-excursion
      (while (org-up-heading-safe)
        (push (org-get-heading t t t t) olp)))
    olp))

;;; 大纲路径操作

(defun org-roam-more-heading-to-olp (file title)
  "Find the first heading with TITLE in FILE and return its outline path (OLP) directly to the node."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (goto-char (point-min))
       (org-element-map (org-element-parse-buffer) 'headline
         (lambda (hl)
           (let ((raw-title (org-element-property :raw-value hl)))
             (when (string= title raw-title)
               (org-with-point-at (org-element-property :begin hl)
                 ;; Return the outline path directly to this node
                 (org-get-outline-path t)))))
         nil t)))))  ;  Returns the outline path for the heading itself.

(defun org-roam-more-subheadings-under-olp (file parent-olp)
  "Return a list of direct subheadings under PARENT-OLP in FILE."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (goto-char (point-min))
       (let (result)
         (org-element-map (org-element-parse-buffer) 'headline
           (lambda (hl)
             (let* ((path (org-with-point-at (org-element-property :begin hl)
                            (org-get-outline-path)))
                    (headline (org-element-property :raw-value hl)))
               ;; (message "Headline: %S" headline)
               ;; (message "OLP path: %S" path)
               (when (equal path parent-olp)
                 ;; (message "Matched parent OLP: %S" parent-olp)
                 (push headline result)))))
         (nreverse result))))))

(defun org-roam-more-get-headline-pos-by-path (path)
  "在当前 buffer 中，根据 PATH(标题列表)查找对应 headline 的位置。
PATH 是标题字符串列表，如 (\"一级标题\" \"二级标题\" \"目标标题\")。
返回 headline 起始位置的 point，找不到返回 nil。"
  (let ((pos (point-min))
        found-pos)
    (save-excursion
      (goto-char pos)
      (catch 'found
        (dolist (title path)
          (setq found-pos (org-find-exact-headline-in-buffer title))
          (if found-pos
              (goto-char found-pos)
            (throw 'found nil))) ;; 找不到就退出
        found-pos))))

(defun org-roam-more-get-original-id-at-path (path id-key)
  "根据 PATH 获取当前 buffer 中对应 headline 的 ORIGINAL-ID。
PATH 是标题字符串列表。找不到返回 nil。"
  (let ((pos (org-roam-more-get-headline-pos-by-path path)))
    (when pos
      (org-entry-get pos id-key))))

(provide 'org-roam-more-utils)
;;; org-roam-more-utils.el ends here

