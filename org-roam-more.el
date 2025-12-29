;;; org-roam-more.el --- Enhanced functionality for org-roam -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author:  <xinyu@arch>
;; Maintainer:  <xinyu@arch>
;; Created: May 07, 2025
;; Modified: May 07, 2025
;; Version: 0.0.1
;; Keywords: org-mode roam knowledge-management transclusion
;; Homepage: https://github.com/gongshangzheng/org-roam-more
;; Package-Requires: ((emacs "24.3") (org-roam "2.0.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; org-roam-more 为 org-roam 提供增强功能，包括：
;;
;; 1. **Transclusion 管理** - 内容转写和双向同步
;;    - 在文档中嵌入其他节点的内容
;;    - 支持 push/pull 双向同步
;;    - 智能内容一致性检查
;;
;; 2. **节点内容操作** - 强大的内容读写功能
;;    - 获取和设置节点内容
;;    - 保留标题和属性的内容替换
;;    - 根据大纲路径操作内容
;;
;; 3. **增强的节点创建** - 灵活的节点插入
;;    - 创建节点并自动插入链接
;;    - 支持插入 transclusion
;;    - 在已有节点下创建子节点
;;
;; 4. **日记集成** - 自动记录到日记
;;    - 新节点自动插入日记
;;    - 可配置的 hook 机制
;;
;; 5. **实用工具** - 丰富的辅助函数
;;    - 链接生成和格式化
;;    - 大纲路径（OLP）操作
;;    - 标题插入和层级管理
;;
;;; 模块结构:
;;
;; org-roam-more
;; ├── org-roam-more-utils.el         - 通用工具函数
;; ├── org-roam-more-node.el          - 节点内容操作
;; ├── org-roam-more-transclusion.el  - Transclusion 管理
;; ├── org-roam-more-capture.el       - 节点创建和插入
;; └── org-roam-more-daily.el         - 日记集成
;;
;;; 快速开始:
;;
;; 1. 安装：
;;    (use-package org-roam-more
;;      :after org-roam
;;      :load-path "/path/to/org-roam-more/")
;;
;; 2. 基本使用：
;;    ;; 插入 transclusion
;;    M-x org-roam-more-insert-transclude
;;
;;    ;; 智能推送（transclusion → 原节点）
;;    M-x org-roam-more-transclusion-push
;;
;;    ;; 智能拉取（原节点 → transclusion）
;;    M-x org-roam-more-transclusion-pull
;;
;; 3. 配置示例：
;;    ;; 启用日记自动插入
;;    (org-roam-more-toggle-insert-link-hook t)
;;
;;; Code:

(require 'org)
(require 'org-element)
(require 'org-roam)

;; 加载子模块
;;;###autoload
(require 'org-roam-more-utils)
;;;###autoload
(require 'org-roam-more-node)
;;;###autoload
(require 'org-roam-more-transclusion)
;;;###autoload
(require 'org-roam-more-capture)
;;;###autoload
(require 'org-roam-more-daily)

;;; 主要功能导出
;;
;; 以下函数是最常用的交互式命令，按功能分类列出：

;;;; Transclusion 功能

;; 插入和管理
;; - `org-roam-more-insert-transclude' - 插入 transclusion
;; - `org-roam-more-is-transclusion-p' - 判断是否为 transclusion
;; - `org-roam-more-transclusion-content-equal-p' - 检查内容一致性

;; 同步功能（推荐使用智能版本）
;; - `org-roam-more-transclusion-push' - 智能推送到原节点
;; - `org-roam-more-transclusion-pull' - 智能从原节点拉取
;; - `org-roam-more-transclusion-push-current' - 推送当前条目
;; - `org-roam-more-transclusion-push-all' - 推送所有条目
;; - `org-roam-more-transclusion-pull-current' - 拉取到当前条目
;; - `org-roam-more-transclusion-pull-all' - 拉取到所有条目

;; 高级功能
;; - `org-roam-more-compare-transclusion-and-roam-content' - 使用 ediff 对比

;;;; 节点操作功能

;; 内容读写
;; - `org-roam-more-get-node-content' - 获取节点内容
;; - `org-roam-more-set-node-content' - 设置节点内容
;; - `org-roam-more-get-node-body' - 获取节点正文（交互式）

;; 节点创建
;; - `org-roam-more-capture-under-node' - 在节点下创建子节点
;; - `org-roam-more-insert-new-node-with-id' - 创建/选择节点并插入链接

;; 属性设置
;; - `org-roam-more-set-source-property-from-node' - 设置 SOURCE 属性

;;;; 日记集成

;; - `org-roam-more-toggle-insert-link-hook' - 启用/禁用自动插入
;; - `org-roam-more-insert-current-node-link-into-daily' - 插入当前节点到日记

;;;; 工具函数

;; 链接生成
;; - `org-roam-more-node-link' - 生成节点链接
;; - `org-roam-more-format-link' - 格式化节点链接

;; 标题操作
;; - `org-roam-more-insert-subheading' - 插入子标题
;; - `org-roam-more-insert-heading-with-level' - 插入指定层级标题

;; 节点信息
;; - `org-roam-more-get-current-node-id-title' - 获取当前节点 ID 和标题
;; - `org-roam-more-get-current-path' - 获取当前大纲路径

;; 大纲路径操作
;; - `org-roam-more-heading-to-olp' - 标题转大纲路径
;; - `org-roam-more-subheadings-under-olp' - 获取子标题列表
;; - `org-roam-more-get-content-at-path' - 根据路径获取内容
;; - `org-roam-more-set-content-at-path' - 根据路径设置内容

;;; 版本信息

(defconst org-roam-more-version "0.0.1"
  "org-roam-more 的版本号。")

(defun org-roam-more-version ()
  "显示 org-roam-more 的版本信息。"
  (interactive)
  (message "org-roam-more version %s" org-roam-more-version))

(provide 'org-roam-more)
;;; org-roam-more.el ends here
