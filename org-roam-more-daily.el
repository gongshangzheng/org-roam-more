;;; org-roam-more-daily.el --- Daily journal integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; 本文件提供 org-roam 与日记（daily journal）的集成功能。
;;
;; 主要功能：
;; - 将新创建的节点链接自动插入到日记中
;; - 支持在 capture 时自动插入（通过 hook）
;; - 支持在创建 ID 时自动插入（通过 advice）
;; - 两种插入模式互斥，只能启用其中一个
;; - 可自定义插入模板和日记文件路径
;; - 自动添加文件链接，方便跳转到原文件
;;
;; 函数列表：
;; * 配置变量
;;   - `org-roam-more-daily-journal-file' - 日记文件路径（可自定义）
;;   - `org-roam-more-daily-insert-template' - 插入模板（可自定义）
;;   - `org-roam-more-daily-insert-on-capture' - 是否在 capture 时自动插入
;;   - `org-roam-more-daily-insert-on-create-id' - 是否在创建 ID 时自动插入
;;
;; * Hook 和 Advice 管理
;;   - `org-roam-more-daily-setup-hooks' - 根据配置设置 hooks 和 advice
;;   - `org-roam-more-toggle-insert-link-hook' - (已废弃) 启用/禁用自动插入
;;
;; * 日记插入
;;   - `org-roam-more-insert-new-node-link-into-daily' - 插入节点链接到日记
;;   - `org-roam-more-insert-current-node-link-into-daily' - 插入当前节点到日记
;;
;; * 辅助函数
;;   - `org-roam-more--get-daily-journal-file' - 获取日记文件路径
;;   - `org-roam-more--insert-after-id-creation' - ID 创建后插入的内部函数
;;
;; 使用示例：
;;   ;; 配置日记文件路径（可选，默认为 org-roam-directory/daily/journal.org）
;;   (setq org-roam-more-daily-journal-file "~/org/roam/daily/journal.org")
;;   
;;   ;; 配置插入模板（可选）
;;   (setq org-roam-more-daily-insert-template "* %s\n  %U")
;;   
;;   ;; 方式一：在 capture 时自动插入（推荐）
;;   (setq org-roam-more-daily-insert-on-capture t)
;;   (setq org-roam-more-daily-insert-on-create-id nil)
;;   
;;   ;; 方式二：在创建 ID 时自动插入（更激进）
;;   ;; (setq org-roam-more-daily-insert-on-capture nil)
;;   ;; (setq org-roam-more-daily-insert-on-create-id t)
;;   
;;   ;; 应用配置
;;   (org-roam-more-daily-setup-hooks)
;;   
;;   ;; 注意：插入的内容会自动包含节点的文件链接，格式如下：
;;   ;; * [[id:xxx][节点标题]]
;;   ;;   文件：[[file:/path/to/file.org][file.org]]
;;   ;;   [2025-01-01 Wed 10:30]
;;
;;; Code:

(require 'org)
(require 'org-roam)
(require 'org-capture)
(require 'org-roam-more-utils)

;;; 自定义变量

(defcustom org-roam-more-daily-journal-file nil
  "日记文件的路径。
如果为 nil，默认使用 org-roam-directory/daily/journal.org。
建议设置为你的日记文件路径，例如：
  \"~/org/roam/daily/journal.org\"
  \"~/Documents/notes/journal.org\""
  :type '(choice (const :tag "使用默认路径" nil)
                 (file :tag "自定义文件路径"))
  :group 'org-roam-more)

(defcustom org-roam-more-daily-insert-template "* %s\n  %U"
  "插入节点链接到日记时使用的模板。

重要：
- %s 会被替换为节点链接（[[id:xxx][标题]]），这是必需的占位符
- 其他占位符（%U, %t, %T 等）会传递给 org-capture 处理

常用的 org-capture 占位符：
- %U: 非激活的时间戳 [2025-01-01 Wed 10:30]
- %u: 非激活的日期戳 [2025-01-01 Wed]
- %T: 激活的时间戳 <2025-01-01 Wed 10:30>
- %t: 激活的日期戳 <2025-01-01 Wed>
- %%: 字面上的 % 符号

示例模板：
- \"* %s\\n  %U\" - 一级标题，带非激活时间戳（默认）
- \"** %s\" - 二级标题，无时间戳
- \"- %s (%U)\" - 列表项，时间戳在括号中
- \"* %s\\n  创建于 %U\" - 一级标题，带中文说明"
  :type 'string
  :group 'org-roam-more)

(defcustom org-roam-more-daily-insert-on-capture nil
  "是否在 org-capture 完成后自动将节点链接插入日记。
如果为 t，每次完成 capture 时都会自动插入到日记中。
如果为 nil，不会自动插入。

注意：此选项与 `org-roam-more-daily-insert-on-create-id' 互斥，
只能启用其中一个。"
  :type 'boolean
  :group 'org-roam-more
  :set (lambda (symbol value)
         (when (and value org-roam-more-daily-insert-on-create-id)
           (setq org-roam-more-daily-insert-on-create-id nil)
           (message "已禁用 insert-on-create-id，因为 insert-on-capture 已启用"))
         (set-default symbol value)))

(defcustom org-roam-more-daily-insert-on-create-id nil
  "是否在创建节点 ID 时自动将节点链接插入日记。
如果为 t，每次使用 org-id-get-create 创建 ID 时都会插入到日记中。
如果为 nil，不会自动插入。

注意：
1. 这个选项更加激进，可能会导致很多节点被插入日记。
2. 此选项与 `org-roam-more-daily-insert-on-capture' 互斥，
   只能启用其中一个。"
  :type 'boolean
  :group 'org-roam-more
  :set (lambda (symbol value)
         (when (and value org-roam-more-daily-insert-on-capture)
           (setq org-roam-more-daily-insert-on-capture nil)
           (message "已禁用 insert-on-capture，因为 insert-on-create-id 已启用"))
         (set-default symbol value)))

;;; 内部变量

(defvar org-roam-more-insert-link-hook-enabled nil
  "是否启用 org-roam-more-insert-current-node-link-into-daily 的 hook.
已废弃，请使用 `org-roam-more-daily-insert-on-capture' 代替。")

;;; Hook 和 Advice 管理

(defun org-roam-more-daily-setup-hooks ()
  "根据配置变量设置日记插入的 hooks 和 advice。
检查 `org-roam-more-daily-insert-on-capture' 和
`org-roam-more-daily-insert-on-create-id' 来决定是否启用相应的功能。

注意：两个选项互斥，只能启用其中一个。"
  (interactive)
  
  ;; 确保互斥性
  (when (and org-roam-more-daily-insert-on-capture 
             org-roam-more-daily-insert-on-create-id)
    (setq org-roam-more-daily-insert-on-create-id nil)
    (message "警告：两个选项互斥，已禁用 insert-on-create-id"))
  
  ;; Capture hook
  (if org-roam-more-daily-insert-on-capture
      (progn
        (add-hook 'org-capture-before-finalize-hook #'org-roam-more-insert-current-node-link-into-daily)
        (message "已启用 capture 后自动插入日记"))
    (remove-hook 'org-capture-before-finalize-hook #'org-roam-more-insert-current-node-link-into-daily))
  
  ;; Create ID advice (使用 advice-add 而不是 hook)
  (if org-roam-more-daily-insert-on-create-id
      (progn
        (advice-add 'org-id-get-create :after #'org-roam-more--insert-after-id-creation)
        (message "已启用创建 ID 后自动插入日记"))
    (advice-remove 'org-id-get-create #'org-roam-more--insert-after-id-creation)))

(defun org-roam-more-toggle-insert-link-hook (enable)
  "根据 ENABLE 的值添加或移除 org-roam-more-insert-current-node-link-into-daily hook。

已废弃：请使用 `org-roam-more-daily-insert-on-capture' 配置变量和
`org-roam-more-daily-setup-hooks' 函数代替。

参数：
- ENABLE: 如果为 t，启用 hook；如果为 nil，禁用 hook。"
  (setq org-roam-more-insert-link-hook-enabled enable)
  (setq org-roam-more-daily-insert-on-capture enable)
  (if enable
      (add-hook 'org-capture-before-finalize-hook #'org-roam-more-insert-current-node-link-into-daily)
    (remove-hook 'org-capture-before-finalize-hook #'org-roam-more-insert-current-node-link-into-daily)))

;;; 日记插入

(defun org-roam-more--get-daily-journal-file ()
  "获取日记文件路径。
优先使用 `org-roam-more-daily-journal-file'，如果未设置则使用默认路径。
默认路径为：org-roam-directory/daily/journal.org"
  (or org-roam-more-daily-journal-file
      (expand-file-name "daily/journal.org" org-roam-directory)))

(defun org-roam-more-insert-new-node-link-into-daily (id title)
  "使用 org-capture 将 org-roam 新节点链接插入今天的日志。

参数：
- ID: 节点的 ID
- TITLE: 节点的标题

此函数会创建一个临时的 capture 模板，将链接插入到日记文件的今天条目下。
使用 `org-roam-more-daily-insert-template' 作为插入模板。
模板中的 %s 会被替换为节点链接，其他占位符（如 %U, %t）保留给 org-capture 处理。
在节点链接下方会自动添加该节点的文件链接，方便跳转。"
  (let* ((node (org-roam-node-from-id id))
         (file-path (when node (org-roam-node-file node)))
         (id-link (org-roam-more--link-make-string (concat "id:" id) title))
         ;; 构建完整的插入内容：节点 ID 链接 + 文件链接
         (full-content (if file-path
                           (concat id-link "\n  文件：[[file:" file-path "][" (file-name-nondirectory file-path) "]]")
                         id-link))
         ;; 使用字符串替换而不是 format，以保留 org-capture 的占位符（%U, %t 等）
         (template-content (replace-regexp-in-string "%s" full-content org-roam-more-daily-insert-template t t))
         (journal-file (org-roam-more--get-daily-journal-file))
         (template `("x" "Insert org-roam node link into daily"
                     entry
                     (file+datetree ,journal-file)
                     ,template-content
                     :immediate-finish t)))
    (let ((org-capture-templates (list template)))
      (org-capture nil "x"))))

(defun org-roam-more-insert-current-node-link-into-daily ()
  "获取当前节点的 ID 和标题，2 秒后插入当天日志。

此函数会：
1. 获取当前节点的 ID 和标题
2. 延迟 2 秒后插入到日记中（避免与 capture 流程冲突）

通常配合 `org-roam-more-daily-insert-on-capture' 自动调用。"
  (interactive)
  (let* ((id-title (org-roam-more-get-current-node-id-title))
         (id (car id-title))
         (title (cdr id-title)))
    (when (and id title)
      (run-at-time
       "2 sec" nil
       (let ((captured-id id)
             (captured-title title))
         (lambda ()
           (org-roam-more-insert-new-node-link-into-daily captured-id captured-title)))))))

(defun org-roam-more--insert-after-id-creation (&optional _id)
  "在创建 ID 后将节点链接插入日记。
此函数作为 `org-id-get-create' 的 advice 使用，会在创建 ID 后自动调用。
参数 _ID 是 org-id-get-create 返回的 ID，但我们不使用它，而是重新获取。
此函数会立即获取当前节点的 ID 和标题，然后延迟插入到日记中。
配合 `org-roam-more-daily-insert-on-create-id' 自动调用。"
  (let* ((id-title (org-roam-more-get-current-node-id-title))
         (id (car id-title))
         (title (cdr id-title)))
    (when (and id title)
      (run-at-time
       "1 sec" nil
       (let ((captured-id id)
             (captured-title title))
         (lambda ()
           (org-roam-more-insert-new-node-link-into-daily captured-id captured-title)))))))

(provide 'org-roam-more-daily)
;;; org-roam-more-daily.el ends here

