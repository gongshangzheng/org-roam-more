;;; org-roam-more-transclusion.el --- Transclusion management -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; 本文件提供 org-roam 内容转写（transclusion）功能。
;;
;; Transclusion 允许你在一个位置引用另一个节点的内容，并支持双向同步。
;; 被转写的内容会被标记为 :transclusion: 标签，并在 properties 中保存原节点信息。
;;
;; 主要功能：
;; - 插入 transclusion（复制节点内容并保留层级）
;; - 判断条目是否为 transclusion
;; - 内容一致性检查
;; - 双向同步（push/pull）
;; - 可视化对比（ediff）
;;
;; 函数列表：
;; * 核心功能
;;   - `org-roam-more-insert-transclude' - 插入 transclusion
;;   - `org-roam-more-is-transclusion-p' - 判断是否为 transclusion
;;   - `org-roam-more-transclusion-content-equal-p' - 检查内容是否一致
;;
;; * 查询功能
;;   - `org-roam-more-get-transclusion-entries' - 获取所有 transclusion 条目
;;   - `org-roam-more-get-transclusion-paths' - 获取所有 transclusion 路径
;;   - `org-roam-more-get-outline-path-from-element' - 从元素获取大纲路径
;;
;; * 同步功能（Push: transclusion → 原节点）
;;   - `org-roam-more-transclusion-push' - 智能推送（自动判断）
;;   - `org-roam-more-transclusion-push-current' - 推送当前条目
;;   - `org-roam-more-transclusion-push-all' - 推送所有条目
;;
;; * 同步功能（Pull: 原节点 → transclusion）
;;   - `org-roam-more-transclusion-pull' - 智能拉取（自动判断）
;;   - `org-roam-more-transclusion-pull-current' - 拉取到当前条目
;;   - `org-roam-more-transclusion-pull-all' - 拉取到所有条目
;;
;; * 高级功能
;;   - `org-roam-more-compare-transclusion-and-roam-content' - 使用 ediff 对比
;;
;;; Code:

(require 'org)
(require 'org-element)
(require 'org-roam)
(require 'ediff)
(require 'org-roam-more-utils)
(require 'org-roam-more-node)

;;; 自定义变量

(defcustom org-roam-more-transclusion-insert-content nil
  "If non-nil, `org-roam-more-insert-transclude' inserts full content instead of a #+transclude link."
  :type 'boolean
  :group 'org-roam-more)

;;; 辅助函数

(defun org-roam-more-get-outline-path-from-element (element)
  "从 headline ELEMENT（org-element）递归获取标题路径（字符串列表）。"
  (let ((title (org-element-property :raw-value element))
        (parent (org-element-property :parent element)))
    (if (and parent (eq (org-element-type parent) 'headline))
        (append (org-roam-more-get-outline-path-from-element parent) (list title))
      (list title))))

;;; 核心功能

(defun org-roam-more-insert-transclude (&optional node-id)
  "插入一个 transclusion，复制原 node 的内容到当前位置。
提示用户选择一个 node，然后：
1. 换行（避免切断现有内容）
2. 复制 node 的完整内容（包括标题和子树，但不包括 property）
3. 保留原本的层级结构
4. 在新插入内容的顶层标题上添加 :transclusion: 标签
5. 在 property 中添加指向原 node 的信息"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "此命令只能在 Org mode 下使用"))
  
  ;; 确保从新行开始
  (unless (bolp) (insert "\n"))
  
  ;; 选择或使用提供的 node
  (let* ((node (if node-id
                   (org-roam-node-from-id node-id)
                 (org-roam-node-read)))
         (node-id (org-roam-node-id node))
         (node-title (org-roam-node-title node))
         (node-file (org-roam-node-file node))
         (node-point (org-roam-node-point node)))
    
    ;; 获取 node 的完整内容（包括标题和子树，但移除 property）
    (let ((content-with-heading 
           (with-current-buffer (find-file-noselect node-file)
             (save-excursion
               (goto-char node-point)
               (let* ((element (org-element-at-point))
                      (begin (org-element-property :begin element))
                      (end (org-element-property :end element))
                      (level (org-element-property :level element))
                      (raw-content (buffer-substring-no-properties begin end)))
                 ;; 移除 property drawer
                 (setq raw-content 
                       (replace-regexp-in-string 
                        "^\\(\\*+\\s-+.*\\)\n:PROPERTIES:\\(?:.*\n\\)*?:END:\n?"
                        "\\1\n"
                        raw-content))
                 ;; 返回内容和层级
                 (cons raw-content level))))))
      
      ;; 插入内容
      (let ((content (car content-with-heading))
            (original-level (cdr content-with-heading)))
        ;; 在第一行标题后添加 :transclusion: 标签和 property
        (if (string-match "^\\(\\*+\\s-+\\)\\(.*?\\)\\(\\s-+:\\sw+:\\)?\\s-*$" content)
            (let* ((stars (match-string 1 content))
                   (title (match-string 2 content))
                   (existing-tags (match-string 3 content))
                   (new-tags (if existing-tags
                                 (replace-regexp-in-string ":\\s-*$" ":transclusion:" existing-tags)
                               ":transclusion:"))
                   (new-first-line (concat stars title " " new-tags "\n"))
                   (property-block (concat ":PROPERTIES:\n"
                                          (format ":ORIGINAL-ID: %s\n" node-id)
                                          (format ":ORIGINAL-HEADING: %s\n" node-title)
                                          (format ":ORIGINAL-NODE-LINK: [[id:%s][%s]]\n" node-id node-title)
                                          ":END:\n"))
                   (rest-content (substring content (match-end 0))))
              (insert new-first-line)
              (insert property-block)
              (insert rest-content))
          ;; 如果匹配失败，直接插入内容
          (insert content))
        
        (message "已插入 transclusion: %s" node-title)))))

(defun org-roam-more-is-transclusion-p ()
  "判断当前 item 是否是 transclusion。
检查当前条目是否有 :transclusion: 标签。"
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (progn
        (message "当前不在 Org 文件中")
        nil)
    (let ((tags (org-get-tags)))
      (if (member "transclusion" tags)
          (progn
            (when (called-interactively-p 'any)
              (message "当前条目是 transclusion"))
            t)
        (progn
          (when (called-interactively-p 'any)
            (message "当前条目不是 transclusion"))
          nil)))))

(defun org-roam-more-transclusion-content-equal-p ()
  "判断当前 transclusion 的内容是否与原 node 的内容一致。
忽略首尾空行差异。如果当前条目不是 transclusion，返回 nil。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "当前不在 Org 文件中"))
  
  (if (not (org-roam-more-is-transclusion-p))
      (progn
        (when (called-interactively-p 'any)
          (message "当前条目不是 transclusion"))
        nil)
    
    (let* ((original-id (org-entry-get nil "ORIGINAL-ID"))
           (node (when original-id (org-roam-node-from-id original-id))))
      (if (not node)
          (progn
            (when (called-interactively-p 'any)
              (message "无法找到原始 node"))
            nil)
        
        ;; 获取当前 transclusion 的内容（不包括标题和 property）
        (let* ((current-content 
                (save-excursion
                  (let* ((element (org-element-at-point))
                         (begin (org-element-property :begin element))
                         (end (org-element-property :end element))
                         (content (buffer-substring-no-properties begin end)))
                    ;; 移除标题行和 property
                    (setq content (replace-regexp-in-string
                                   "^\\*+\\s-+.*\n" "" content))
                    (setq content (replace-regexp-in-string
                                   ":PROPERTIES:\\(?:.*\n\\)*?:END:\n?" "" content))
                    ;; 去除首尾空行
                    (string-trim content))))
               ;; 获取原始 node 的内容（不包括标题和 property）
               (original-content
                (let ((raw (org-roam-more-get-node-content node t t)))
                  ;; 去除首尾空行
                  (string-trim raw)))
               (equal-p (string= current-content original-content)))
          
          (when (called-interactively-p 'any)
            (if equal-p
                (message "内容一致")
              (message "内容不一致")))
          equal-p)))))

;;; 查询功能

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
                               ":PROPERTIES:\\(.\\|\n\\)*?:END:\n?" "" content)))
              (when remove-heading
                (setq content (mapconcat #'identity (cdr (split-string content "\n")) "\n")))
              (push content results))))))
    (nreverse results)))

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

;;; 同步功能 - Push（transclusion → 原节点）

(defun org-roam-more-transclusion-push-all ()
  "将当前文件所有 transclusion 条目的内容推送到其对应的 org-roam 节点。
对每个 :transclusion: 标签的条目：
1. 检查是否有 transclusion 标签
2. 检查内容是否与原 node 一致
3. 如果一致则跳过，不一致则更新
4. 更新时保留原 node 的标题和 property"
  (interactive)
  (let ((transclusion-paths (org-roam-more-get-transclusion-paths))
        (updated-count 0)
        (skipped-count 0))
    (dolist (path transclusion-paths)
      (save-excursion
        (let ((pos (org-roam-more-get-headline-pos-by-path path)))
          (when pos
            (goto-char pos)
            (let* ((original-id (org-entry-get nil "ORIGINAL-ID"))
                   (node (when original-id (org-roam-node-from-id original-id))))
              (if (not node)
                  (message "跳过：无法找到原始 node (路径: %s)" path)
                ;; 获取当前 transclusion 的内容（不含标题和 property）
                (let* ((current-content 
                        (let* ((element (org-element-at-point))
                               (begin (org-element-property :begin element))
                               (end (org-element-property :end element))
                               (content (buffer-substring-no-properties begin end)))
                          ;; 移除标题和 property
                          (setq content (replace-regexp-in-string
                                         "^\\*+\\s-+.*\n" "" content))
                          (setq content (replace-regexp-in-string
                                         ":PROPERTIES:\\(?:.*\n\\)*?:END:\n?" "" content))
                          (string-trim content)))
                       ;; 获取原 node 内容
                       (original-content 
                        (string-trim (org-roam-more-get-node-content node t t))))
                  
                  (if (string= current-content original-content)
                      (progn
                        (message "内容一致，跳过：%s" (car (last path)))
                        (setq skipped-count (1+ skipped-count)))
                    ;; 内容不一致，更新原 node
                    (org-roam-more-set-node-content node current-content)
                    (message "已同步到 Org-roam 节点：%s" (car (last path)))
                    (setq updated-count (1+ updated-count))))))))))
    (message "推送完成：更新 %d 个，跳过 %d 个" updated-count skipped-count)))

(defun org-roam-more-transclusion-push-current ()
  "将当前 transclusion 条目的内容推送到其对应的 org-roam 节点。
1. 检查当前条目是否有 :transclusion: 标签
2. 检查内容是否与原 node 一致
3. 如果一致则提示跳过，不一致则更新
4. 更新时保留原 node 的标题和 property"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "当前不在 Org 文件中"))
  
  (when (org-before-first-heading-p)
    (user-error "当前不在任何 Org 项目下"))

  ;; 检查是否有 :transclusion: 标签
  (unless (org-roam-more-is-transclusion-p)
    (user-error "当前条目没有 :transclusion: 标签"))

  ;; 获取原 node
  (let* ((original-id (org-entry-get nil "ORIGINAL-ID"))
         (node (when original-id (org-roam-node-from-id original-id))))
    
    (unless node
      (user-error "无法找到原始 node (ID: %s)" original-id))
    
    ;; 获取当前内容
    (let* ((current-content 
            (let* ((element (org-element-at-point))
                   (begin (org-element-property :begin element))
                   (end (org-element-property :end element))
                   (content (buffer-substring-no-properties begin end)))
              ;; 移除标题和 property
              (setq content (replace-regexp-in-string
                             "^\\*+\\s-+.*\n" "" content))
              (setq content (replace-regexp-in-string
                             ":PROPERTIES:\\(?:.*\n\\)*?:END:\n?" "" content))
              (string-trim content)))
           ;; 获取原 node 内容
           (original-content 
            (string-trim (org-roam-more-get-node-content node t t))))
      
      (if (string= current-content original-content)
          (message "内容一致，无需更新")
        ;; 内容不一致，更新原 node
        (org-roam-more-set-node-content node current-content)
        (message "已推送到 Org-roam 节点：%s" (org-roam-node-title node))))))

;;; 同步功能 - Pull（原节点 → transclusion）

(defun org-roam-more-transclusion-pull-all ()
  "将当前文件所有 transclusion 条目从对应的 org-roam 节点拉取内容。
对每个 :transclusion: 标签的条目：
1. 检查是否有 transclusion 标签
2. 检查内容是否与原 node 一致
3. 如果一致则跳过，不一致则更新
4. 更新时保留 transclusion 的标题和 property"
  (interactive)
  (let ((transclusion-paths (org-roam-more-get-transclusion-paths))
        (updated-count 0)
        (skipped-count 0))
    (dolist (path transclusion-paths)
      (save-excursion
        (let ((pos (org-roam-more-get-headline-pos-by-path path)))
          (when pos
            (goto-char pos)
            (let* ((original-id (org-entry-get nil "ORIGINAL-ID"))
                   (node (when original-id (org-roam-node-from-id original-id))))
              (if (not node)
                  (message "跳过：无法找到原始 node (路径: %s)" path)
                ;; 获取当前 transclusion 的内容
                (let* ((current-content 
                        (let* ((element (org-element-at-point))
                               (begin (org-element-property :begin element))
                               (end (org-element-property :end element))
                               (content (buffer-substring-no-properties begin end)))
                          ;; 移除标题和 property
                          (setq content (replace-regexp-in-string
                                         "^\\*+\\s-+.*\n" "" content))
                          (setq content (replace-regexp-in-string
                                         ":PROPERTIES:\\(?:.*\n\\)*?:END:\n?" "" content))
                          (string-trim content)))
                       ;; 获取原 node 内容
                       (original-content 
                        (string-trim (org-roam-more-get-node-content node t t))))
                  
                  (if (string= current-content original-content)
                      (progn
                        (message "内容一致，跳过：%s" (car (last path)))
                        (setq skipped-count (1+ skipped-count)))
                    ;; 内容不一致，用原 node 内容更新当前 transclusion
                    (org-roam-more-set-content-at-path path original-content)
                    (message "已从 Org-roam 节点拉取到：%s" (car (last path)))
                    (setq updated-count (1+ updated-count))))))))))
    (message "拉取完成：更新 %d 个，跳过 %d 个" updated-count skipped-count)))

(defun org-roam-more-transclusion-pull-current ()
  "将当前 transclusion 条目从对应的 org-roam 节点拉取内容。
1. 检查当前条目是否有 :transclusion: 标签
2. 检查内容是否与原 node 一致
3. 如果一致则提示跳过，不一致则更新
4. 更新时保留 transclusion 的标题和 property"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "当前不在 Org 文件中"))
  
  (when (org-before-first-heading-p)
    (user-error "当前不在任何 Org 项目下"))

  ;; 检查是否有 :transclusion: 标签
  (unless (org-roam-more-is-transclusion-p)
    (user-error "当前条目没有 :transclusion: 标签"))

  ;; 获取原 node
  (let* ((original-id (org-entry-get nil "ORIGINAL-ID"))
         (node (when original-id (org-roam-node-from-id original-id))))
    
    (unless node
      (user-error "无法找到原始 node (ID: %s)" original-id))
    
    ;; 获取当前内容
    (let* ((current-content 
            (let* ((element (org-element-at-point))
                   (begin (org-element-property :begin element))
                   (end (org-element-property :end element))
                   (content (buffer-substring-no-properties begin end)))
              ;; 移除标题和 property
              (setq content (replace-regexp-in-string
                             "^\\*+\\s-+.*\n" "" content))
              (setq content (replace-regexp-in-string
                             ":PROPERTIES:\\(?:.*\n\\)*?:END:\n?" "" content))
              (string-trim content)))
           ;; 获取原 node 内容
           (original-content 
            (string-trim (org-roam-more-get-node-content node t t)))
           (path (org-roam-more-get-current-path)))
      
      (if (string= current-content original-content)
          (message "内容一致，无需更新")
        ;; 内容不一致，用原 node 内容更新当前 transclusion
        (org-roam-more-set-content-at-path path original-content)
        (message "已从 Org-roam 节点拉取到当前 transclusion")))))

;;; 智能同步

(defun org-roam-more-transclusion-push ()
  "智能推送 transclusion 内容到原 org-roam 节点。
- 如果光标在 transclusion 条目内：推送当前条目到原 node
- 如果光标在条目外或非 transclusion 条目：推送当前文件所有 transclusion 到对应的原 node"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "当前不在 Org 文件中"))
  
  (if (and (not (org-before-first-heading-p))
           (org-roam-more-is-transclusion-p))
      ;; 在 transclusion 条目内
      (org-roam-more-transclusion-push-current)
    ;; 在条目外或非 transclusion 条目
    (org-roam-more-transclusion-push-all)))

(defun org-roam-more-transclusion-pull ()
  "智能从原 org-roam 节点拉取内容到 transclusion。
- 如果光标在 transclusion 条目内：从原 node 拉取到当前条目
- 如果光标在条目外或非 transclusion 条目：从对应原 node 拉取到当前文件所有 transclusion"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "当前不在 Org 文件中"))
  
  (if (and (not (org-before-first-heading-p))
           (org-roam-more-is-transclusion-p))
      ;; 在 transclusion 条目内
      (org-roam-more-transclusion-pull-current)
    ;; 在条目外或非 transclusion 条目
    (org-roam-more-transclusion-pull-all)))

;;; 高级功能

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

(provide 'org-roam-more-transclusion)
;;; org-roam-more-transclusion.el ends here

