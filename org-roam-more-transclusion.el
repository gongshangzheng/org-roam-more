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
;; - 快速跳转到原始节点
;;
;; 函数列表：
;; * 辅助函数
;;   - `org-roam-more-find-transclusion-heading' - 向上查找 transclusion 顶层标题
;;   - `org-roam-more-get-transclusion-path' - 获取 transclusion 的大纲路径
;;
;; * 核心功能
;;   - `org-roam-more-insert-transclude' - 插入 transclusion
;;   - `org-roam-more-is-transclusion-p' - 判断是否为 transclusion（支持子标题）
;;   - `org-roam-more-transclusion-content-equal-p' - 检查内容是否一致（支持子标题）
;;
;; * 内容提取
;;   - `org-roam-more-get-body-content-at-path' - 根据路径提取正文内容
;;   - `org-roam-more-get-current-transclusion-body' - 获取当前 transclusion 正文
;;
;; * 查询功能
;;   - `org-roam-more-get-transclusion-entries' - 获取所有 transclusion 条目
;;   - `org-roam-more-get-transclusion-paths' - 获取所有 transclusion 路径
;;   - `org-roam-more-get-outline-path-from-element' - 从元素获取大纲路径
;;
;; * 同步功能（Push: transclusion → 原节点）
;;   - `org-roam-more-transclusion-push-current' - 推送当前条目到原节点
;;
;; * 同步功能（Pull: 原节点 → transclusion）
;;   - `org-roam-more-transclusion-pull-current' - 从原节点拉取到当前条目
;;
;; * 高级功能
;;   - `org-roam-more-compare-transclusion-and-roam-content' - 使用 ediff 对比
;;   - `org-roam-more-transclusion-goto-original' - 跳转到原始节点
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

(defun org-roam-more-find-transclusion-heading ()
  "向上查找带有 :transclusion: 标签的祖先标题。
如果当前在 transclusion 的子标题内，返回顶层 transclusion 标题的位置。
如果当前就在 transclusion 标题上，返回当前位置。
如果没有找到，返回 nil。"
  (save-excursion
    (catch 'found
      ;; 先回到当前标题
      (org-back-to-heading t)
      ;; 检查当前标题
      (when (member "transclusion" (org-get-tags nil t))
        (throw 'found (point)))
      ;; 向上查找
      (while (org-up-heading-safe)
        (when (member "transclusion" (org-get-tags nil t))
          (throw 'found (point))))
      ;; 没找到
      nil)))

(defun org-roam-more-get-transclusion-path ()
  "获取当前光标所在 transclusion 的大纲路径。
即使光标在 transclusion 的子标题内，也会返回顶层 transclusion 的路径。
返回路径列表，如 (\"一级\" \"二级\" \"transclusion标题\")。
如果不在 transclusion 内，返回 nil。"
  (let ((trans-pos (org-roam-more-find-transclusion-heading)))
    (when trans-pos
      (save-excursion
        (goto-char trans-pos)
        (org-roam-more-get-current-path)))))

(defun org-roam-more-get-body-content-at-path (path)
  "根据路径 PATH 提取条目的正文内容。
自动去除标题行和 :PROPERTIES: 区块，只返回正文字符串。
PATH 是标题字符串列表，如 (\"一级标题\" \"二级标题\" \"目标标题\")。
使用 org-mode 官方函数来获取子树内容。
返回去除标题和属性后的正文内容字符串，找不到则返回 nil。"
  (let ((pos (org-roam-more-get-headline-pos-by-path path)))
    (when pos
      (save-excursion
        (goto-char pos)
        (org-back-to-heading t)
        ;; 跳过标题行
        (forward-line 1)
        ;; 跳过 property drawer
        (when (looking-at-p org-property-drawer-re)
          (re-search-forward ":END:" nil t)
          (forward-line 1))
        (let ((content-start (point))
              (content-end (save-excursion
                             (org-end-of-subtree t t)
                             (point))))
          ;; 去除首尾空行
          (string-trim (buffer-substring-no-properties content-start content-end)))))))

(defun org-roam-more-get-current-transclusion-body ()
  "获取当前光标所在 transclusion 条目的正文内容。
即使光标在 transclusion 的子标题内，也会找到顶层 transclusion 并获取其完整内容。
使用 org-mode 官方函数来正确处理子树和 property drawer。
返回去除标题和属性后的正文内容字符串。"
  (unless (derived-mode-p 'org-mode)
    (user-error "当前不在 Org 文件中"))
  
  (let ((trans-pos (org-roam-more-find-transclusion-heading)))
    (unless trans-pos
      (user-error "当前位置不在 transclusion 内"))
    
    (save-excursion
      (goto-char trans-pos)
      ;; 跳过标题行
      (forward-line 1)
      ;; 跳过 property drawer
      (when (looking-at-p org-property-drawer-re)
        (re-search-forward ":END:" nil t)
        (forward-line 1))
      (let ((content-start (point))
            (content-end (save-excursion
                           (org-end-of-subtree t t)
                           (point))))
        ;; 去除首尾空行
        (string-trim (buffer-substring-no-properties content-start content-end))))))

(defun org-roam-more-get-outline-path-from-element (element)
  "从 headline ELEMENT（org-element）递归获取标题路径（字符串列表）。"
  (let ((title (org-element-property :raw-value element))
        (parent (org-element-property :parent element)))
    (if (and parent (eq (org-element-type parent) 'headline))
        (append (org-roam-more-get-outline-path-from-element parent) (list title))
      (list title))))

;;; 核心功能

(defun org-roam-more--get-node-heading-and-content (node-id)
  "根据 NODE-ID 获取节点的标题和正文内容。
返回 (heading . content) 的 cons cell，其中：
- heading: 节点的标题（字符串）
- content: 节点的正文内容（不包括标题行和 property drawer）"
  (let ((node (org-roam-node-from-id node-id)))
    (unless node
      (user-error "无法找到 ID 为 %s 的节点" node-id))
    (let* ((heading (org-roam-node-title node))
           (content (org-roam-more-get-node-content node t t)))  ; 移除 properties 和 heading
      (cons heading content))))

(defun org-roam-more-insert-transclude (&optional node-id)
  "插入一个 transclusion，复制原 node 的内容到当前位置。
提示用户选择一个 node，然后：
1. 插入 2 级标题（** heading :transclusion:）
2. 插入 property drawer（包含 ORIGINAL-ID、ORIGINAL-HEADING、ORIGINAL-NODE-LINK）
3. 插入 node 的正文内容（不包括原标题和 property）"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "此命令只能在 Org mode 下使用"))
  
  ;; 选择或使用提供的 node
  (let* ((node (if node-id
                   (org-roam-node-from-id node-id)
                 (org-roam-node-read)))
         (node-id (org-roam-node-id node))
         (node-title (org-roam-node-title node)))
    
    ;; 使用辅助函数获取 heading 和 content
    (let* ((heading-and-content (org-roam-more--get-node-heading-and-content node-id))
           (heading (car heading-and-content))
           (content (cdr heading-and-content)))
      
      ;; 确保从新行开始
      ;; (unless (bolp) (insert "\n"))
      
      ;; 1. 插入 2 级标题，带 :transclusion: 标签
      (insert (format "** %s :transclusion:\n" heading))
      
      ;; 2. 插入 property drawer
      (insert ":PROPERTIES:\n")
      (insert (format ":ORIGINAL-ID: %s\n" node-id))
      (insert (format ":ORIGINAL-HEADING: %s\n" node-title))
      (insert (format ":ORIGINAL-NODE-LINK: [[id:%s][%s]]\n" node-id node-title))
      (insert ":END:\n")
      
      ;; 3. 插入正文内容
      (when (and content (not (string-empty-p (string-trim content))))
        (insert content))
      
      (message "已插入 transclusion: %s" node-title))))

(defun org-roam-more-is-transclusion-p ()
  "判断当前位置是否在 transclusion 内。
检查当前条目或其祖先条目是否有 :transclusion: 标签。
即使光标在 transclusion 的子标题内，也会返回 t。"
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (progn
        (message "当前不在 Org 文件中")
        nil)
    (let ((trans-pos (org-roam-more-find-transclusion-heading)))
      (if trans-pos
          (progn
            (when (called-interactively-p 'any)
              (message "当前位置在 transclusion 内"))
            t)
        (progn
          (when (called-interactively-p 'any)
            (message "当前位置不在 transclusion 内"))
          nil)))))

(defun org-roam-more-transclusion-content-equal-p ()
  "判断当前 transclusion 的内容是否与原 node 的内容一致。
即使光标在 transclusion 的子标题内，也会正确比较顶层 transclusion 的内容。
忽略首尾空行差异。如果当前位置不在 transclusion 内，返回 nil。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "当前不在 Org 文件中"))
  
  ;; 找到顶层 transclusion
  (let ((trans-pos (org-roam-more-find-transclusion-heading)))
    (if (not trans-pos)
        (progn
          (when (called-interactively-p 'any)
            (message "当前位置不在 transclusion 内"))
          nil)
      
      (save-excursion
        (goto-char trans-pos)
        (let* ((original-id (org-entry-get nil "ORIGINAL-ID"))
               (node (when original-id (org-roam-node-from-id original-id))))
          (if (not node)
              (progn
                (when (called-interactively-p 'any)
                  (message "无法找到原始 node"))
                nil)
            
            ;; 获取当前 transclusion 的内容（不包括标题和 property）
            (let* ((current-content (org-roam-more-get-current-transclusion-body))
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
              equal-p)))))))

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

(defun org-roam-more-transclusion-push-current ()
  "将当前 transclusion 条目的内容推送到其对应的 org-roam 节点。
即使光标在 transclusion 的子标题内，也会正确找到顶层 transclusion 并推送。
直接覆盖原 node 的内容，保留原 node 的标题和 property。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "当前不在 Org 文件中"))
  
  (when (org-before-first-heading-p)
    (user-error "当前不在任何 Org 项目下"))

  ;; 找到顶层 transclusion
  (let ((trans-pos (org-roam-more-find-transclusion-heading)))
    (unless trans-pos
      (user-error "当前位置不在 transclusion 内"))
    
    (save-excursion
      (goto-char trans-pos)
      ;; 获取原 node 和路径
      (let* ((path (org-roam-more-get-current-path))
             (original-id (org-entry-get nil "ORIGINAL-ID"))
             (node (when original-id (org-roam-node-from-id original-id))))
        
        (unless node
          (user-error "无法找到原始 node (ID: %s)" original-id))
        
        ;; 使用辅助函数获取当前 transclusion 的正文内容并直接推送
        (let ((current-content (org-roam-more-get-current-transclusion-body)))
          (org-roam-more-set-node-content node current-content)
          (message "已推送到 Org-roam 节点：%s" (org-roam-node-title node)))))))

;;; 同步功能 - Pull（原节点 → transclusion）

(defun org-roam-more-transclusion-pull-current ()
  "将当前 transclusion 条目从对应的 org-roam 节点拉取内容。
即使光标在 transclusion 的子标题内，也会正确找到顶层 transclusion 并拉取。
直接覆盖 transclusion 的内容，保留 transclusion 的标题和 property。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "当前不在 Org 文件中"))
  
  (when (org-before-first-heading-p)
    (user-error "当前不在任何 Org 项目下"))

  ;; 找到顶层 transclusion
  (let ((trans-pos (org-roam-more-find-transclusion-heading)))
    (unless trans-pos
      (user-error "当前位置不在 transclusion 内"))
    
    (save-excursion
      (goto-char trans-pos)
      ;; 获取原 node 和路径
      (let* ((path (org-roam-more-get-current-path))
             (original-id (org-entry-get nil "ORIGINAL-ID"))
             (node (when original-id (org-roam-node-from-id original-id))))
        
        (unless node
          (user-error "无法找到原始 node (ID: %s)" original-id))
        
        ;; 获取原 node 的正文内容并直接拉取
        (let ((original-content 
               (string-trim (org-roam-more-get-node-content node t t))))
          (org-roam-more-set-content-at-path path original-content)
          (message "已从 Org-roam 节点拉取到当前 transclusion"))))))

;;; 高级功能

(defun org-roam-more-compare-transclusion-and-roam-content ()
  "使用 ediff 比较当前 transclusion 与原 org-roam 节点的内容。
只比较光标当前所在的 transclusion 条目，编辑完成后更新两边的内容。
即使光标在 transclusion 的子标题内，也会正确找到顶层 transclusion 并比较。
必须在 transclusion 条目内调用此命令。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "当前不在 Org 文件中"))
  
  (when (org-before-first-heading-p)
    (user-error "当前不在任何 Org 项目下"))
  
  ;; 找到顶层 transclusion
  (let ((trans-pos (org-roam-more-find-transclusion-heading)))
    (unless trans-pos
      (user-error "当前位置不在 transclusion 内"))
    
    (save-excursion
      (goto-char trans-pos)
      ;; 获取原 node
      (let* ((path (org-roam-more-get-current-path))
             (title-or-alias (car (last path)))
             (original-id (org-entry-get nil "ORIGINAL-ID"))
             (node (when original-id (org-roam-node-from-id original-id))))
        
        (unless node
          (user-error "无法找到原始 node (ID: %s)" original-id))
        
          ;; 使用新的辅助函数获取当前 transclusion 的正文内容
          (let* ((current-content (org-roam-more-get-current-transclusion-body))
                 ;; 获取原 node 的正文内容
                 (roam-content (string-trim (org-roam-more-get-node-content node t t))))
            
            (if (and roam-content current-content)
                (let* ((buf-a (generate-new-buffer (format "*Transclusion: %s*" title-or-alias)))
                       (buf-b (generate-new-buffer (format "*Original: %s*" title-or-alias)))
                       ;; 记录信息用于恢复
                       (org-path path)
                       (roam-node node))
                  (with-current-buffer buf-a 
                    (insert current-content)
                    (org-mode))  ;; 启用 org-mode 以获得更好的编辑体验
                  (with-current-buffer buf-b 
                    (insert roam-content)
                    (org-mode))
                  ;; 设置 hook 在 ediff 退出后更新内容
                  (let ((hook-fn
                         `(lambda ()
                            (let ((new-a (with-current-buffer ,buf-a (buffer-string)))
                                  (new-b (with-current-buffer ,buf-b (buffer-string))))
                              ;; 更新 transclusion 内容
                              (org-roam-more-set-content-at-path ',org-path new-a)
                              ;; 更新原 node 内容
                              (org-roam-more-set-node-content ,roam-node new-b)
                              (message "已更新 transclusion 和原节点的内容")
                              (kill-buffer ,buf-a)
                              (kill-buffer ,buf-b)))))
                    (add-hook 'ediff-quit-hook hook-fn nil t))
                  (ediff-buffers buf-a buf-b))
              (message "未找到内容")))))))

(defun org-roam-more-transclusion-goto-original ()
  "从当前 transclusion 跳转到其原始 org-roam 节点。
即使光标在 transclusion 的子标题内，也会正确找到原始节点并跳转。
使用 ORIGINAL-ID 属性来定位并跳转到原始节点。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "当前不在 Org 文件中"))
  
  (when (org-before-first-heading-p)
    (user-error "当前不在任何 Org 项目下"))
  
  ;; 找到顶层 transclusion
  (let ((trans-pos (org-roam-more-find-transclusion-heading)))
    (unless trans-pos
      (user-error "当前位置不在 transclusion 内"))
    
    (let (original-id node)
      (save-excursion
        (goto-char trans-pos)
        ;; 获取原节点 ID
        (setq original-id (org-entry-get nil "ORIGINAL-ID"))
        (unless original-id
          (user-error "未找到 ORIGINAL-ID 属性"))
        
        ;; 获取原节点
        (setq node (org-roam-node-from-id original-id))
        (unless node
          (user-error "无法找到原始 node (ID: %s)" original-id)))
      
      ;; 使用 org-roam 的内置函数跳转（会切换窗口和移动光标）
      (org-roam-node-visit node)
      (message "已跳转到原始节点：%s" (org-roam-node-title node)))))

(provide 'org-roam-more-transclusion)  
;;; org-roam-more-transclusion.el ends here
