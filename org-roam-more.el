;;; org-roam-more.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author:  <xinyu@arch>
;; Maintainer:  <xinyu@arch>
;; Created: May 07, 2025
;; Modified: May 07, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/gongshangzheng/org-roam-more
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Some complementary functions to org-roam
;;
;;; Code:
(require 'org)
(require 'org-element)
(require 'org-roam)
(defcustom org-roam-more-transclusion-insert-content nil
  "If non-nil, `org-roam-more-insert-transclude' inserts full content instead of a #+transclude link."
  :type 'boolean
  :group 'org-roam-more)
(defun org-roam-more-get-current-path ()
  "返回当前 Org 条目的 OLP（Outline Path）.
也就是标题层级列表。包含当前标题，
从最顶层到当前条目的顺序排列。"
  (let ((olp (list (org-get-heading t t t t)))) ; 先加入当前标题
    (save-excursion
      (while (org-up-heading-safe)
        (push (org-get-heading t t t t) olp)))
    olp))

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
(defun org-roam-more-capture-under-node ()
  "Prompt the user to choose an existing Org-roam node, then create a new subheading under it."
  (interactive)
  (let* ((parent-node (org-roam-node-read)) ; User picks existing node
         (file (org-roam-node-file parent-node))
         (parent-title (org-roam-node-title (org-roam-node-from-title-or-alias (org-roam-node-title parent-node))))
         (parent-olp (org-roam-more-heading-to-olp file parent-title)))
    (unless parent-olp
      (error "Impossible de trouver l'OLP pour le titre: %s" parent-title))
    (let* ((existing-subheadings (org-roam-more-subheadings-under-olp file parent-olp))
           (new-title (completing-read
                       "Entrer le titre du nouveau sous-heading: "
                       existing-subheadings nil nil)))
      (when (member new-title existing-subheadings)
        (user-error "Un heading nommé \"%s\" existe déjà sous ce noeud" new-title))
      (let ((new-olp (append parent-olp (list new-title))))
        (org-roam-capture-
         :node (org-roam-node-create :title new-title)
         :templates
         `(("n" "default" plain "%?"
            :hook (lambda ()
                    (org-id-get-create))
            :if-new (file+olp ,file ,new-olp)
            :immediate-finish nil
            :unnarrowed t)))))))
(defun org-roam-more-get-node-content (node &optional remove-properties remove-heading)
  "获取 org-roam NODE 的内容。
如果 REMOVE-PROPERTIES 为非 nil，则去除 :PROPERTIES: 区块。
如果 REMOVE-HEADING 为非 nil，则去除首行标题（保留子标题）。"
  (let* ((file (org-roam-node-file node))
         (point (org-roam-node-point node))
         (content ""))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char point)
        (let ((beg (point))
              (end (progn (org-end-of-subtree t t)
                          (point))))
          (setq content (buffer-substring-no-properties beg end))
          (when (or (null remove-properties) remove-properties)
            (setq content
                  (replace-regexp-in-string ":PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?" "" content)))
          (when (or (null remove-heading) remove-heading)
            (setq content
                  (mapconcat #'identity (cdr (split-string content "\n")) "\n"))))))
    content))
(defun org-roam-more-set-node-content (node new-content)
  "Replace the content of NODE with NEW-CONTENT while preserving heading and properties.
Does not automatically save the file."
  (let ((file (org-roam-node-file node))
        (point (org-roam-node-point node)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char point)
        (forward-line) ;; 跳过标题行
        (let ((properties-end
               (save-excursion
                 (when (looking-at-p ":PROPERTIES:")
                   (re-search-forward ":END:" nil t)
                   (forward-line))
                 (point)))
              (subtree-end (progn (org-end-of-subtree t t) (point))))
          (goto-char properties-end)
          (delete-region properties-end subtree-end)
          (insert (string-trim-right new-content) "\n")
          ;; 保存文件，否则数据库不会更新
          (save-buffer)
          ;; 同步 org-roam 数据库，仅更新当前文件
          (org-roam-db-update-file file))))))
(defun org-roam-more-get-node-body (title-or-alias &optional remove-properties remove-heading)
  "Get body content of node matching TITLE-OR-ALIAS.
When REMOVE-PROPERTIES is non-nil, strips :PROPERTIES: drawer.
When REMOVE-HEADING is non-nil, removes the heading line.
Returns content as string or nil if not found."
  (interactive "s输入标题或别名: \nP")
  (let ((node (org-roam-node-from-title-or-alias title-or-alias)))
    (if node
        (let ((content (org-roam-more-get-node-content node remove-properties remove-heading)))
          (message "找到节点：%s\n内容： %s" (org-roam-node-title node) content)
          content)
      (progn
        (message "没有找到名为 '%s' 的 node。" title-or-alias)
        nil))))
(defun org-roam-more-insert-transclude (&optional insert-content)
  "Prompt for a title or alias, and insert a heading with properties and content from the Org-roam node.
If INSERT-CONTENT is non-nil (interactively via prefix arg), include the full content; otherwise insert a #+transclude link."
  (interactive "P")
  (let* ((node (org-roam-node-read)))
    (unless node
      (user-error "未找到节点"))
    (unless (derived-mode-p 'org-mode)
      (user-error "This command only works in Org mode"))
    (unless (bolp) (insert "\n"))

    (let* ((heading (org-roam-node-title node))
           (node-id (org-roam-node-id node))
           (file-path (org-roam-node-file node))
           (insert-content (or insert-content org-roam-more-transclusion-insert-content))
           (node-content (when insert-content
                           (org-roam-more-get-node-content node t t)))) ;; remove heading and properties
      ;; Insert new heading
      (insert (concat "** " heading " :transclusion:\n"))
      ;; Insert properties
      (insert ":PROPERTIES:\n")
      (insert (format ":ORIGINAL-HEADING: %s\n" heading))
      (insert (format ":ORIGINAL-ID: %s\n" node-id))
      (insert (format ":ORIGINAL-FILE: [[file:%s]]\n" file-path))
      (insert (format ":ORIGINAL-NODE-LINK: [[id:%s][%s]]\n" node-id heading)) ;; Org-roam style link
      (insert ":END:\n")
      ;; Insert content or transclude link
      (if insert-content
          (insert node-content)
        (insert (format "#+transclude: [[id:%s]]\n" node-id))))))
(defun org-roam-more-get-transclusion-entries (&optional remove-properties remove-heading)
  "Get all transclusion entries (headlines with :transclusion: tag) in current file.
When REMOVE-PROPERTIES is non-nil, removes :PROPERTIES: blocks.
When REMOVE-HEADING is non-nil, removes heading lines.
Returns list of entry contents."
  (interactive)
  (let ((results '()))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (let ((tags (org-element-property :tags headline)))
          (when (member "transclusion" tags)
            (let* ((begin (org-element-property :begin headline))
                   (end (org-element-property :end headline))
                   (content (buffer-substring-no-properties begin end)))
              (when remove-properties
                (setq content (replace-regexp-in-string
                               ":PROPERTIES:\\(.\\|\n\\)*?:END:\n*" "" content)))
              (when remove-heading
                (setq content (mapconcat #'identity (cdr (split-string content "\n")) "\n")))
              (push content results))))))
    (nreverse results)))
(defun org-roam-more-get-outline-path-from-element (element)
  "从 headline ELEMENT（org-element）递归获取标题路径（字符串列表）。"
  (let ((title (org-element-property :raw-value element))
        (parent (org-element-property :parent element)))
    (if (and parent (eq (org-element-type parent) 'headline))
        (append (org-roam-more-get-outline-path-from-element parent) (list title))
      (list title))))

(defun org-roam-more-get-transclusion-paths ()
  "获取当前文件所有带 :transclusion: 标签 headline 的完整路径列表。
路径为字符串列表。"
  (interactive)
  (let ((paths '()))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (hl)
        (let ((tags (org-element-property :tags hl)))
          (when (member "transclusion" tags)
            (let ((path (org-roam-more-get-outline-path-from-element hl)))
              (push path paths))))))
    (nreverse paths)))
(defun org-roam-more-get-content-at-path (path &optional remove-properties remove-heading)
  "根据标题路径 PATH，获取当前 Org buffer 中对应条目的正文内容。
PATH 是一个标题组成的列表，从最外层 heading 到最内层。
当 REMOVE-PROPERTIES 为非 nil 时移除 :PROPERTIES: 区块。
当 REMOVE-HEADING 为非 nil 时移除首行标题。
返回条目的内容字符串，若未找到则返回 nil。"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((current-level 0)
          (found-pos nil))
      ;; 逐层匹配路径
      (dolist (title path)
        (let ((found nil))
          (while (and (not found)
                      (re-search-forward (format org-complex-heading-regexp-format (regexp-quote title)) nil t))
            (let ((el (org-element-at-point)))
              (when (and (eq (org-element-type el) 'headline)
                         (= (org-element-property :level el) (1+ current-level))
                         (string= (org-element-property :raw-value el) title))
                (setq current-level (org-element-property :level el))
                (setq found-pos (point))
                (setq found t))))
          (unless found
            (setq found-pos nil)
            (cl-return))))
      (when found-pos
        (goto-char found-pos)
        (let* ((el (org-element-at-point))
               (begin (org-element-property :begin el))
               (end (org-element-property :end el))
               (content (buffer-substring-no-properties begin end)))
          (when remove-properties
            (setq content (replace-regexp-in-string
                           ":PROPERTIES:\\(.\\|\n\\)*?:END:\n*" "" content)))
          (when remove-heading
            (setq content (mapconcat #'identity (cdr (split-string content "\n")) "\n")))
          content)))))
(defun org-roam-more-set-content-at-path (path new-content &optional filepath)
  "Set content at PATH (list of heading titles) to NEW-CONTENT.
FILEPATH defaults to current buffer's file.
Preserves heading and properties, only replaces body content."
  (let ((file (or filepath (buffer-file-name))))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (let ((found t))
          (dolist (title path)
            (setq found nil)
            (while (and (not found) (re-search-forward org-heading-regexp nil t))
              (let ((el (org-element-at-point)))
                (when (and (eq (org-element-type el) 'headline)
                           (string= (org-element-property :raw-value el) title))
                  (setq found t)))))
          (if (not found)
              (message "未找到路径：%s" path)
            (let* ((el (org-element-at-point))
                   (subtree-end (org-element-property :end el))
                   (properties-end
                    (save-excursion
                      (forward-line)
                      (if (looking-at-p ":PROPERTIES:")
                          (progn
                            (re-search-forward ":END:" nil t)
                            (forward-line)
                            (point))
                        (point)))))
              ;; 替换正文
              (goto-char properties-end)
              (delete-region properties-end subtree-end)
              (insert (string-trim-right new-content) "\n"))))))))
(defun org-roam-more-compare-transclusion-and-roam-content ()
  "Compare transclusion content with org-roam node content using ediff.
For all :transclusion: tagged entries, shows differences between file content
and corresponding org-roam node content. Updates both after ediff completes."
  (interactive)
  (let ((transclusion-paths (org-roam-more-get-transclusion-paths)))
    (dolist (path transclusion-paths)
      (let* ((title-or-alias (car (last path)))
             (node (org-roam-node-from-title-or-alias title-or-alias))
             (roam-content (org-roam-more-get-node-content node t))
             (current-content-list (org-roam-more-get-content-at-path path t t))) ;; 移除 properties 和 heading
        (if (and roam-content current-content-list)
            (let* ((current-content (car current-content-list))
                   (buf-a (generate-new-buffer (format "*Org: %s*" title-or-alias)))
                   (buf-b (generate-new-buffer (format "*Roam: %s*" title-or-alias)))
                   ;; 记录信息用于恢复
                   (org-path path)
                   (roam-node node))
              (with-current-buffer buf-a (insert current-content))
              (with-current-buffer buf-b (insert roam-content))
              ;; 设置 hook 在 ediff 退出后更新内容
              (let ((hook-fn
                     `(lambda ()
                        (let ((new-a (with-current-buffer ,buf-a (buffer-string)))
                              (new-b (with-current-buffer ,buf-b (buffer-string))))
                          (org-roam-more-set-content-at-path ',org-path new-a)
                          (org-roam-more-set-node-content ,roam-node new-b)
                          (kill-buffer ediff-control-buffer)
                          (kill-buffer ,buf-a)
                          (kill-buffer ,buf-b)))))
                (add-hook 'ediff-after-quit-hook-internal hook-fn))
              (ediff-buffers buf-a buf-b))
          (message "未找到路径或内容为空：%s" path))))))
(defun org-roam-more-sync-transclusion-content-to-org-roam ()
  "Sync content from transclusion entries to their org-roam nodes.
For all :transclusion: tagged entries, copies their content to the
corresponding org-roam node while preserving headings and properties."
  (interactive)
  (let ((transclusion-paths (org-roam-more-get-transclusion-paths)))
    (dolist (path transclusion-paths)
      (let* ((title-or-alias (car (last path)))
             (original-id (org-entry-get nil "ORIGINAL-ID"))
             (node (if original-id
                       (org-roam-node-from-id original-id)
                     (org-roam-node-from-title-or-alias title-or-alias)))
             (current-content (org-roam-more-get-content-at-path path t t))) ;; 移除 heading 和 properties
        (if (and node current-content)
            (let ((new-content current-content))
              (org-roam-more-set-node-content node new-content)
              (message "已同步内容到 Org-roam 节点：%s" title-or-alias))
          (message "未找到节点或内容为空：%s" title-or-alias))))))
(defun org-roam-more-sync-current-transclusion-content-to-org-roam ()
  "Sync content from the current :transclusion: entry to its org-roam node.
保留标题和属性，仅替换正文内容。"
  (interactive)
  (save-excursion
    (when (org-before-first-heading-p)
      (user-error "当前不在任何 Org 项目下"))

    ;; 确保当前 entry 有标签 :transclusion:
    (let ((tags (org-get-tags)))
      (if (not (member "transclusion" tags))
          (message "当前项没有标签 :transclusion:")
        (let* ((path (org-roam-more-get-current-path)) ; 你需要自己实现或已有这个函数
               (title-or-alias (car (last path)))
               (original-id (org-entry-get nil "ORIGINAL-ID"))
               (node (if original-id
                         (org-roam-node-from-id original-id)
                       (org-roam-node-from-title-or-alias title-or-alias)))
               (current-content (org-roam-more-get-content-at-path path t t))) ;; 移除 heading 和 properties
          (if (and node current-content)
              (progn
                (org-roam-more-set-node-content node current-content)
                (message "已同步内容到 Org-roam 节点：%s" title-or-alias))
            (message "未找到节点或内容为空：%s" title-or-alias)))))))

(defun org-roam-more-sync-org-roam-content-to-transclusion ()
  "Sync content from org-roam nodes to transclusion entries.
For all :transclusion: tagged entries, copies content from the
corresponding org-roam node while preserving headings and properties."
  (interactive)
  (let ((transclusion-paths (org-roam-more-get-transclusion-paths)))
    (dolist (path transclusion-paths)
      (let* ((title-or-alias (car (last path)))
             (original-id (org-roam-more-get-original-id-at-path path "ORIGINAL-ID"))
             (node (if original-id
                       (org-roam-node-from-id original-id)
                     (org-roam-node-from-title-or-alias title-or-alias)))) ;; 移除 properties 和 heading

        (when node  ; 只有当 node 存在时才继续
          (let ((roam-content (org-roam-more-get-node-content node t)))
            (if roam-content
                (progn
                  (org-roam-more-set-content-at-path path roam-content)
                  (message "已同步 Org-roam 节点内容到：%s" title-or-alias))
              (message "内容为空：%s" title-or-alias))))))))

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

(provide 'org-roam-more)
;;; org-roam-more.el ends here
