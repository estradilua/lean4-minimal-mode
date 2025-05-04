;;; lean4-mode.el --- A major mode for the Lean language -*- lexical-binding: t -*-

;; Copyright (c) 2013, 2014 Microsoft Corporation. All rights reserved.
;; Copyright (c) 2014, 2015 Soonho Kong. All rights reserved.
;; Copyright (c) 2024 Lucas Reis. All rights reserved.

;; Author: Leonardo de Moura <leonardo@microsoft.com>
;;         Soonho Kong       <soonhok@cs.cmu.edu>
;;         Gabriel Ebner     <gebner@gebner.org>
;;         Sebastian Ullrich <sebasti@nullri.ch>
;;         Lucas Reis        <lvreis@usp.br>
;; Maintainer: Lucas Reis <lvreis@usp.br>
;; Created: Jan 09, 2014
;; Keywords: languages
;; Package-Requires: ((emacs "27.1") (dash "2.18.0") (eglot "1.15"))
;; URL: https://github.com/lucasvries/lean4-eglot-mode
;; SPDX-License-Identifier: Apache-2.0

;;; License:

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at:
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Provides a major mode for the Lean programming language.

;; Provides highlighting, diagnostics, goal visualization,
;; and many other useful features for Lean users.

;; See the README.md for more advanced features and the
;; associated keybindings.

;;; Code:

(require 'eglot)
(require 'lean4-settings)
(require 'lean4-syntax)
(require 'lean4-fringe)

(defun lean4-refresh-file-dependencies ()
  "Refresh the file dependencies.

This function restarts the server subprocess for the current
file, recompiling, and reloading all imports."
  (interactive)
  (when eglot--managed-mode
    (eglot--signal-textDocument/didClose)
    (eglot--signal-textDocument/didOpen)))

(defvar lean4-mode-map (make-sparse-keymap)
  "Keymap used in Lean mode.")

(defun lean4--eglot-project (initial)
  "Find the Lean 4 Eglot project for path INITIAL.

Starting from INITIAL, repeatedly look up the
directory hierarchy for a directory containing a file
\"lean-toolchain\", and use the last such directory found, if any.
This allows us to edit files in child packages using the settings
of the parent project."
  (when eglot-lsp-context
    (let (root)
      (when-let ((file-name initial))
        (while-let ((dir (locate-dominating-file file-name "lean-toolchain")))
          ;; We found a toolchain file, but maybe it belongs to a package.
          ;; Continue looking until there are no more toolchain files.
          (setq root dir
                file-name (file-name-directory (directory-file-name dir)))))
      (when root (list 'eglot--project root)))))

;; Automode List
;;;###autoload
(define-derived-mode lean4-mode prog-mode "Lean 4"
  "Major mode for Lean.
\\{lean4-mode-map}
Invokes `lean4-mode-hook'."
  :syntax-table lean4-mode-syntax-table
  :group 'lean
  (setq-local comment-start "--")
  (setq-local comment-start-skip "[-/]-[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*\\(-/\\|\\s>\\)")
  (setq-local comment-padding 1)
  (setq-local comment-use-syntax t)
  (setq-local font-lock-defaults lean4-font-lock-defaults)
  (setq-local indent-tabs-mode nil)
  (add-to-list (make-local-variable 'project-find-functions) #'lean4--eglot-project)
  (require 'lean4-input)
  (set-input-method "Lean"))

;; Automatically use lean4-mode for .lean files.
;;;###autoload
(add-to-list '("\\.lean\'" . lean4-mode) auto-mode-alist)

;;;###autoload
(with-eval-after-load 'markdown-mode
  (add-to-list 'markdown-code-lang-modes '("lean" . lean4-mode)))

;; Use utf-8 encoding
;;;###autoload
(modify-coding-system-alist 'file "\\.lean\\'" 'utf-8)

;; Setup Eglot
(add-hook 'lean4-mode-hook #'eglot-ensure)
(push (cons 'lean4-mode '("lake" "serve")) eglot-server-programs)

(provide 'lean4-mode)
;;; lean4-mode.el ends here
