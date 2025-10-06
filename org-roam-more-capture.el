;;; org-roam-more-capture.el --- Node capture and insertion -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; 本文件提供增强的 org-roam 节点创建和插入功能。
;;
;; 主要功能：
;; - 创建新节点并自动插入链接
;; - 支持插入普通链接或 transclusion
;; - 使用 hook 机制捕获新创建的节点信息
;;
;; 函数列表：
;; * 主要功能
;;   - `org-roam-more-insert-new-node-with-id' - 创建或选择节点并插入链接
;;
;; * 内部变量和函数
;;   - `org-roam-more--last-captured-id' - 保存最后捕获的节点 ID
;;   - `org-roam-more--last-captured-heading' - 保存最后捕获的节点标题
;;   - `org-roam-more--update-last-captured-node' - 更新捕获的节点信息
;;   - `org-roam-more--insert-link-after-capture' - 捕获后插入链接
;;
;;; Code:

(require 'org)
(require 'org-roam)
(require 'org-capture)
(require 'org-roam-more-utils)
(require 'org-roam-more-transclusion)

;;; 内部变量

(defvar org-roam-more--last-captured-id nil
  "保存最近一次 org-roam capture 创建的节点 ID。")

(defvar org-roam-more--last-captured-heading nil
  "保存最近一次 org-roam capture 创建的节点标题。")

;;; 内部函数

(defun org-roam-more--update-last-captured-node ()
  "在 org-capture 结束前更新最近创建的 org-roam node 的信息。"
  (let* ((id-title (org-roam-more-get-current-node-id-title))
         (id (car id-title))
         (title (cdr id-title)))
    (when (and id title)
      ;; (message (format "id: %s, title: %s" id title))
      (setq org-roam-more--last-captured-id id)
      (setq org-roam-more--last-captured-heading
            title))))

(defun org-roam-more--insert-link-after-capture (&optional use-transclusion)
  "在 org-roam capture 完成后插入链接，并移除 hook。
当 USE-TRANSCLUSION 非 nil 时，使用 transclusion 插入。"
  ;; 移除所有临时 hook
  (remove-hook 'org-capture-before-finalize-hook #'org-roam-more--update-last-captured-node)
  (remove-hook 'org-capture-after-finalize-hook #'org-roam-more--insert-link-after-capture)
  ;; 插入链接
  ;; (message "=== use-transclusion: %S ====" use-transclusion)
  (when (and org-roam-more--last-captured-id org-roam-more--last-captured-heading)
    (if use-transclusion
        (org-roam-more-insert-transclude org-roam-more--last-captured-id)
      (insert (format "[[id:%s][%s]]"
                      org-roam-more--last-captured-id
                      org-roam-more--last-captured-heading))))
  ;; 清空缓存
  (setq org-roam-more--last-captured-id nil)
  (setq org-roam-more--last-captured-heading nil))

;;; 主要功能

(defun org-roam-more-insert-new-node-with-id (&optional goto keys use-transclusion &key filter-fn templates info)
  "Prompt for a title, create a new roam node via capture if needed, then insert a link to it.

选择一个已有的 org-roam 节点或创建新节点，然后插入链接。

参数：
- GOTO: 是否跳转到新创建的节点
- KEYS: org-capture 模板的 keys
- USE-TRANSCLUSION: 如果非 nil，插入 transclusion 而不是普通链接
- FILTER-FN: 过滤节点的函数
- TEMPLATES: 自定义 capture 模板
- INFO: 传递给 capture 的额外信息

如果节点已存在，直接插入链接；如果不存在，启动 capture 流程创建新节点。"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org mode"))

  ;; 让用户选择已有节点或输入新标题
  (let* ((node (org-roam-node-read nil filter-fn)))
    (if (org-roam-node-file node)
        ;; 如果已有文件路径，说明 node 存在，直接插入链接
        (if use-transclusion
            (org-roam-more-insert-transclude (org-roam-node-id node))
          (insert (org-roam-more-format-link node)))
      ;; 否则创建新节点（启动 capture 流程）
      (progn
        ;; (message "Node 未找到！")
        (add-hook 'org-capture-before-finalize-hook #'org-roam-more--update-last-captured-node)
        (add-hook 'org-capture-after-finalize-hook (lambda () (org-roam-more--insert-link-after-capture use-transclusion)))
        (org-roam-capture- :goto goto
                           :info info
                           :keys keys
                           :templates templates
                           :node node
                           :props '(:immediate-finish nil))))))

(provide 'org-roam-more-capture)
;;; org-roam-more-capture.el ends here

