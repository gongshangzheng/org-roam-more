;;; org-roam-more-node.el --- Node content operations -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; 本文件提供 org-roam 节点内容的读取、修改和管理功能。
;;
;; 主要功能：
;; - 获取和设置节点内容
;; - 根据路径获取和设置内容
;; - 在节点下创建子标题
;; - 设置节点属性
;;
;; 函数列表：
;; * 辅助函数
;;   - `org-roam-more-skip-property-drawer' - 跳过 property drawer
;;
;; * 节点内容操作
;;   - `org-roam-more-get-node-content' - 获取节点完整内容（支持选择性移除标题/属性）
;;   - `org-roam-more-set-node-content' - 设置节点内容（保留标题和属性）
;;   - `org-roam-more-get-node-body' - 获取节点正文（交互式）
;;
;; * 路径内容操作
;;   - `org-roam-more-get-content-at-path' - 根据大纲路径获取内容
;;   - `org-roam-more-set-content-at-path' - 根据大纲路径设置内容
;;
;; * 节点创建
;;   - `org-roam-more-capture-under-node' - 在已有节点下创建子标题
;;
;; * 属性设置
;;   - `org-roam-more-set-source-property-from-node' - 设置 SOURCE 属性
;;
;;; Code:

(require 'org)
(require 'org-element)
(require 'org-roam)
(require 'org-roam-more-utils)

;;; 节点内容操作

(defun org-roam-more--goto-id-property (id)
  "精确定位到包含 :ID: property 的节点标题。
与 `org-id-goto' 不同，此函数专门搜索 property drawer 中的 :ID: 属性，
而不是文件中第一次出现 ID 的位置。这样可以避免定位到 transclusion 中的 ORIGINAL-ID。

参数：
  ID - 节点的唯一标识符

返回值：
  如果找到，返回 t 并将光标移动到对应的标题行；
  如果未找到，报错。"
  (goto-char (point-min))
  (let ((search-pattern (format "^[ \t]*:ID:[ \t]+%s[ \t]*$" (regexp-quote id)))
        (found nil))
    (while (and (not found) (re-search-forward search-pattern nil t))
      ;; 确保找到的是在 property drawer 中
      (save-excursion
        (let ((prop-start (line-beginning-position)))
          ;; 向上查找 :PROPERTIES:
          (when (re-search-backward "^[ \t]*:PROPERTIES:[ \t]*$" nil t)
            (let ((drawer-start (point)))
              ;; 向下查找 :END:
              (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                (let ((drawer-end (point)))
                  ;; 确认 :ID: 在这个 drawer 中
                  (when (and (>= prop-start drawer-start)
                           (<= prop-start drawer-end))
                    ;; 找到了正确的 property drawer，定位到标题
                    (goto-char drawer-start)
                    (org-back-to-heading t)
                    (setq found t)))))))))
    (unless found
      (error "无法找到包含 :ID: %s 的节点" id))
    t))

(defun org-roam-more-skip-property-drawer ()
  "跳过当前位置的 property drawer（如果存在）。
假设光标已经在标题行后的第一行。
如果有 property drawer，移动到 :END: 后的下一行的开头；否则保持不动。"
  (when (looking-at "^[ \t]*:PROPERTIES:")
    ;; 逐行向下查找，直到找到 :END:
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (forward-line 1)
        (when (looking-at "^[ \t]*:END:[ \t]*$")
          (setq found t)))
      ;; 现在光标在 :END: 行，再向下移动一行
      (when found
        (forward-line 1)))))

(defun org-roam-more-get-node-content (node &optional remove-properties remove-heading)
  "获取 org-roam NODE 的内容。
如果 REMOVE-PROPERTIES 为非 nil，则去除 :PROPERTIES: 区块。
如果 REMOVE-HEADING 为非 nil，则去除首行标题（保留子标题）。
使用指针移动方式，避免正则表达式替换的不可靠性。"
  (let* ((file (org-roam-node-file node))
         (node-id (org-roam-node-id node)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        ;; 使用专门的函数定位到 :ID: property，避免定位到 transclusion 的 ORIGINAL-ID
        (org-roam-more--goto-id-property node-id)
        
        (cond
         ;; 同时移除标题和属性：跳过标题行，跳过 property drawer，取剩余内容
         ((and remove-properties remove-heading)
          (forward-line 1)  ; 跳过标题行
          ;; 如果下一行是 :PROPERTIES:，跳过整个 property drawer
          (when (looking-at "^[ \t]*:PROPERTIES:")
            (while (and (not (looking-at "^[ \t]*:END:[ \t]*$")) (not (eobp)))
              (forward-line 1))
            ;; 现在在 :END: 行，移动到下一行
            (when (looking-at "^[ \t]*:END:[ \t]*$")
              (forward-line 1)))
          (let ((content-start (point))
                (content-end (save-excursion
                               (org-end-of-subtree t t)
                               (point))))
            (buffer-substring-no-properties content-start content-end)))
         
         ;; 只移除标题：跳过标题行，取剩余内容（包括 property drawer）
         (remove-heading
          (forward-line 1)  ; 跳过标题行
          (let ((content-start (point))
                (content-end (save-excursion
                               (org-end-of-subtree t t)
                               (point))))
            (buffer-substring-no-properties content-start content-end)))
         
         ;; 只移除属性：获取完整内容后，跳过 property drawer
         (remove-properties
          (let ((heading-start (point)))
            (forward-line 1)  ; 移到标题下一行
            (let ((after-heading (point)))
              (org-roam-more-skip-property-drawer)
              (let ((after-properties (point))
                    (content-end (save-excursion
                                   (org-end-of-subtree t t)
                                   (point))))
                ;; 拼接：标题行 + 跳过 property drawer 后的内容
                (concat
                 (buffer-substring-no-properties heading-start after-heading)
                 (buffer-substring-no-properties after-properties content-end))))))
         
         ;; 都不移除：获取完整内容
         (t
          (let ((content-start (point))
                (content-end (save-excursion
                               (org-end-of-subtree t t)
                               (point))))
            (buffer-substring-no-properties content-start content-end))))))))

(defun org-roam-more-set-node-content (node new-content)
  "Replace the content of NODE with NEW-CONTENT while preserving heading and properties.
Does not automatically save the file."
  (let ((file (org-roam-node-file node))
        (node-id (org-roam-node-id node)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        ;; 使用专门的函数定位到 :ID: property，避免定位到 transclusion 的 ORIGINAL-ID
        (org-roam-more--goto-id-property node-id)
        (forward-line) ;; 跳过标题行
        ;; 跳过 property drawer（如果存在）
        (org-roam-more-skip-property-drawer)
        (let ((properties-end (point))
              (subtree-end (save-excursion (org-end-of-subtree t t) (point))))
          (delete-region properties-end subtree-end)
          (insert (string-trim-right new-content) "\n")
          ;; 保存文件，否则数据库不会更新
          ;; (save-buffer)
          ;; 同步 org-roam 数据库，仅更新当前文件
          ;; (org-roam-db-update-file file)
          )))))

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

;;; 路径内容操作

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
                           ":PROPERTIES:\\(.\\|\n\\)*?:END:\n?" "" content)))
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

;;; 节点创建

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

;;; 属性设置

(defun org-roam-more-set-source-property-from-node ()
  "使用 `org-roam-node-read` 选择一个节点，并将其链接写入当前 entry 的 SOURCE 属性。"
  (interactive)
  (let* ((node (org-roam-node-read))
         (source (org-roam-more-node-link node))) ;; 可自定义为 title/id/link 等
    (org-set-property "SOURCE" source)
    (message "已设置 SOURCE 属性为: %s" source)))

(provide 'org-roam-more-node)
;;; org-roam-more-node.el ends here

