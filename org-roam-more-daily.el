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
;; - 支持通过 hook 自动触发
;; - 可手动将当前节点插入日记
;;
;; 函数列表：
;; * Hook 管理
;;   - `org-roam-more-toggle-insert-link-hook' - 启用/禁用自动插入 hook
;;
;; * 日记插入
;;   - `org-roam-more-insert-new-node-link-into-daily' - 插入节点链接到日记
;;   - `org-roam-more-insert-current-node-link-into-daily' - 插入当前节点到日记
;;
;; * 内部变量
;;   - `org-roam-more-insert-link-hook-enabled' - Hook 启用状态
;;
;; 使用示例：
;;   (org-roam-more-toggle-insert-link-hook t)   ;; 启用自动插入
;;   (org-roam-more-toggle-insert-link-hook nil) ;; 禁用自动插入
;;
;;; Code:

(require 'org)
(require 'org-roam)
(require 'org-capture)
(require 'org-roam-more-utils)

;;; 变量

(defvar org-roam-more-insert-link-hook-enabled nil
  "是否启用 org-roam-more-insert-current-node-link-into-daily 的 hook.")

;;; Hook 管理

(defun org-roam-more-toggle-insert-link-hook (enable)
  "根据 ENABLE 的值添加或移除 org-roam-more-insert-current-node-link-into-daily hook。

参数：
- ENABLE: 如果为 t，启用 hook；如果为 nil，禁用 hook。

启用后，每次完成 org-capture 时都会自动将新节点的链接插入到日记中。"
  (setq org-roam-more-insert-link-hook-enabled enable)
  (if enable
      (add-hook 'org-capture-before-finalize-hook #'org-roam-more-insert-current-node-link-into-daily)
    (remove-hook 'org-capture-before-finalize-hook #'org-roam-more-insert-current-node-link-into-daily)))

;;; 日记插入

(defun org-roam-more-insert-new-node-link-into-daily (id title)
  "使用 org-capture 将 org-roam 新节点链接插入今天的日志。

参数：
- ID: 节点的 ID
- TITLE: 节点的标题

此函数会创建一个临时的 capture 模板，将链接插入到日记文件的今天条目下。"
  (let* ((link (org-roam-more--link-make-string (concat "id:" id) title))
         (template-content (format "* %s\n  %%U" link))
         (template `("x" "Insert org-roam node link into daily"
                     entry
                     (file+datetree "~/org/roam/daily/journal.org")
                     ,template-content
                     :immediate-finish t)))
    (let ((org-capture-templates (list template)))
      (org-capture nil "x"))))

(defun org-roam-more-insert-current-node-link-into-daily ()
  "获取当前节点的 ID 和标题，2 秒后插入当天日志。

此函数会：
1. 获取当前节点的 ID 和标题
2. 延迟 2 秒后插入到日记中（避免与 capture 流程冲突）

通常配合 `org-roam-more-toggle-insert-link-hook' 自动调用。"
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

(provide 'org-roam-more-daily)
;;; org-roam-more-daily.el ends here

