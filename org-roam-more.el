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

(defun org-roam-more-heading-to-olp (file title)
  "Convert a heading TITLE in FILE to its outline path (OLP).
Returns the outline path as a list of strings representing the heading hierarchy."
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
  "Get direct subheadings under PARENT-OLP in FILE.
PARENT-OLP should be a list representing the parent heading's outline path.
Returns a list of subheading titles as strings."
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
(defun org-roam-more-insert-transclude ()
  "Insert a transclusion link at point.
Creates a #+transclude: directive followed by an org-roam node link."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org mode"))
  (unless (bolp) (insert "\n"))
  (insert "#+transclude: ")
  (org-roam-node-insert))
(defun org-roam-more-capture-under-node ()
  "Capture a new node as subheading under an existing node.
Prompts for parent node, then creates new subheading with completion
against existing subheadings to prevent duplicates."
  "Prompt the user to choose an existing Org-roam node, then create a new subheading under it."
  (interactive)
  (let* ((parent-node (org-roam-node-read)) ; User picks existing node
         (file (org-roam-node-file parent-node))
         (parent-title (org-roam-node-title (org-roam-node-from-title-or-alias (org-roam-node-title parent-node))))
         (parent-olp (org-roam-more-heading-name-to-olp file parent-title)))
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
  "Get content of NODE with optional formatting.
When REMOVE-PROPERTIES is non-nil, strips :PROPERTIES: drawer.
When REMOVE-HEADING is non-nil, removes the heading line.
Returns content as string."
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
                  (replace-regexp-in-string
                   ":PROPERTIES:\\(.\\|\n\\)*?:END:\n*" "" content)))
          (when (or (null remove-heading) remove-heading)
            (setq content
                  (mapconcat #'identity (cdr (split-string content "\n")) "\n"))))))
    content))
(defun org-roam-more-set-node-content (node new-content)
  "Replace the content of NODE with NEW-CONTENT while preserving heading and properties.
Does not automatically save the file."
  "将 org-roam NODE 的正文内容替换为 NEW-CONTENT。
保留标题行和 :PROPERTIES: 区块，仅替换正文部分。
不自动保存文件。"
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
          (insert (string-trim-right new-content) "\n"))))))
(defun org-roam-more-get-node-body (title-or-alias &optional remove-properties remove-heading)
  "Get body content of node matching TITLE-OR-ALIAS.
When REMOVE-PROPERTIES is non-nil, strips :PROPERTIES: drawer.
When REMOVE-HEADING is non-nil, removes the heading line.
Returns content as string or nil if not found."
  "查找 org-roam 中标题或别名为 TITLE-OR-ALIAS 的 node，并返回其正文内容。
如果 REMOVE-PROPERTIES 为非 nil（默认），则去除 :PROPERTIES: 区块。"
  (interactive "s输入标题或别名: \nP")
  (let ((node (org-roam-node-from-title-or-alias title-or-alias)))
    (if node
        (let ((content (org-roam-more-roam-get-node-content node remove-properties remove-heading)))
          (message "找到节点：%s\n内容： %s" (org-roam-node-title node) content)
          content)
      (progn
        (message "没有找到名为 '%s' 的 node。" title-or-alias)
        nil))))
(defun org-roam-more-get-transclusion-entries (&optional remove-properties remove-heading)
  "Get all transclusion entries (headlines with :transclusion: tag) in current file.
When REMOVE-PROPERTIES is non-nil, removes :PROPERTIES: blocks.
When REMOVE-HEADING is non-nil, removes heading lines.
Returns list of entry contents."
  "获取当前 Org 文件中所有带有标签 :transclusion: 的条目的正文内容。
如果 REMOVE-PROPERTIES 非 nil，则移除 :PROPERTIES: 区块。
如果 REMOVE-HEADING 非 nil，则移除每个条目的首行标题。"
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
(defun org-roam-more-get-transclusion-paths ()
  "Get outline paths of all headlines with :transclusion: tag in current file.
Returns list of paths (each path is a list of strings)."
  "获取当前 Org 文件中所有带有标签 :transclusion: 的 headline 的路径（以嵌套列表返回）。"
  (interactive)
  (let ((paths '()))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (hl)
        (let ((tags (org-element-property :tags hl)))
          (when (member "transclusion" tags)
            (let ((path (org-get-outline-path t)))
              (push path paths))))))
    (nreverse paths)))
(defun org-roam-more-get-content-at-path (path &optional remove-properties remove-heading)
  "Get content at PATH (list of heading titles) in current file.
When REMOVE-PROPERTIES is non-nil, removes :PROPERTIES: blocks.
When REMOVE-HEADING is non-nil, removes heading line.
Returns content as string or nil if not found."
  "根据给定路径 PATH（例如标题），返回 Org 文件中该条目的正文内容。
如果 REMOVE-PROPERTIES 非 nil，则移除 :PROPERTIES: 区块。
如果 REMOVE-HEADING 非 nil，则移除每个条目的首行标题。"
  (interactive)
  (let ((results '()))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (let ((headline-title (org-element-property :title headline)))
          (when (string= (org-element-interpret-data headline-title) (car (last path)))
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
(defun org-roam-more-set-content-at-path (path new-content &optional filepath)
  "Set content at PATH (list of heading titles) to NEW-CONTENT.
FILEPATH defaults to current buffer's file.
Preserves heading and properties, only replaces body content."
  "根据嵌套路径 PATH（如 '(\"父标题\" \"子标题\")），将该条目的正文内容替换为 NEW-CONTENT。
FILEPATH 默认为当前 buffer 所对应的文件。
保留标题和 :PROPERTIES: 区块，仅替换正文部分。"
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
  "对当前文件中所有带有标签 :transclusion: 的项，分别从 Org 文件和 Org-roam 中获取其内容，并使用 ediff 进行对比。
在 ediff 退出后，将 buffer A 的内容写回 Org 文件，将 buffer B 的内容写回 org-roam 节点。"
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
                          (kill-buffer ,buf-a)
                          (kill-buffer ,buf-b)))))
                (add-hook 'ediff-after-quit-hook-internal hook-fn))
              (ediff-buffers buf-a buf-b))
          (message "未找到路径或内容为空：%s" path))))))
(defun org-roam-more-sync-transclusion-content-to-org-roam ()
  "Sync content from transclusion entries to their org-roam nodes.
For all :transclusion: tagged entries, copies their content to the
corresponding org-roam node while preserving headings and properties."
  "将当前文件中所有带有标签 :transclusion: 的项的内容直接同步到对应的 Org-roam 节点中。
保留标题和属性，仅替换正文内容。"
  (interactive)
  (let ((transclusion-paths (org-roam-more-get-transclusion-paths)))
    (dolist (path transclusion-paths)
      (let* ((title-or-alias (car (last path)))
             (node (org-roam-node-from-title-or-alias title-or-alias))
             (current-content-list (org-roam-more-get-content-at-path path t t))) ;; 移除 heading 和 properties
        (if (and node current-content-list)
            (let ((new-content (car current-content-list)))
              (org-roam-more-roam-set-node-content node new-content)
              (message "已同步内容到 Org-roam 节点：%s" title-or-alias))
          (message "未找到节点或内容为空：%s" title-or-alias))))))
(defun org-roam-more-sync-org-roam-content-to-transclusion ()
  "Sync content from org-roam nodes to transclusion entries.
For all :transclusion: tagged entries, copies content from the
corresponding org-roam node while preserving headings and properties."
  "将所有 Org-roam 节点的内容同步到当前文件中带有标签 :transclusion: 的项中。
保留标题和属性，仅替换正文内容。"
  (interactive)
  (let ((transclusion-paths (org-roam-more-get-transclusion-paths)))
    (dolist (path transclusion-paths)
      (let* ((title-or-alias (car (last path)))
             (node (org-roam-node-from-title-or-alias title-or-alias))
             (roam-content (org-roam-more-roam-get-node-content node t))) ;; 移除 properties 和 heading
        (if (and node roam-content)
            (let ((new-content roam-content))
              (org-roam-more-set-content-at-path path new-content)
              (message "已同步 Org-roam 节点内容到：%s" title-or-alias))
          (message "未找到节点或内容为空：%s" title-or-alias))))))

(provide 'org-roam-more)
;;; org-roam-more.el ends here
