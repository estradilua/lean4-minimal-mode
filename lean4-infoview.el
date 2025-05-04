;;; lean4-infoview.el --- External infoview implementation for lean4-minimal-mode  -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Lua Reis. All rights reserved.

;; Author: Lua <me@lua.blog.br>
;; Keywords: languages 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'simple-httpd)
(require 'websocket)
(require 'eglot)

(defvar lean4-infoview-config
  '(:allErrorsOnLine t
    :autoOpenShowsGoal t
    :debounceTime 50
    :expectedTypeVisibility "Expanded by default"
    :showGoalNames t
    :emphasizeFirstGoal :json-false
    :reverseTacticState :json-false
    :hideTypeAssumptions :json-false
    :hideInstanceAssumptions :json-false
    :hideInaccessibleAssumptions :json-false
    :hideLetValues :json-false
    :showTooltipOnHover t))


;;;; Editor API implementation

(defun lean4-infoview--save-config (config)
  (message "NOT IMPLEMENTED: save-config"))

(defun lean4-infoview--send-client-request (uri method params)
  (with-current-buffer (find-file-noselect (eglot-uri-to-path uri))
    (eglot--request (eglot--current-server-or-lose) method params)))

(defun lean4-infoview--send-client-notification (uri method params)
  (with-current-buffer (find-file-noselect (eglot-uri-to-path uri))
    (jsonrpc-notify (eglot--current-server-or-lose) method params)))

(defun lean4-infoview--subscribe-server-notifications (method)
  (message "NOT IMPLEMENTED: subscribe-server-notifications"))

(defun lean4-infoview--unsubscribe-server-notifications (method)
  (message "NOT IMPLEMENTED: unsubscribe-server-notifications"))

(defun lean4-infoview--subscribe-client-notifications (method)
  (message "NOT IMPLEMENTED: subscribe-client-notifications"))

(defun lean4-infoview--unsubscribe-client-notifications (method)
  (message "NOT IMPLEMENTED: unsubscribe-client-notifications"))

(defun lean4-infoview--copy-to-clipboard (text)
  (with-temp-buffer
    (insert text)
    (clipboard-kill-ring-save (point-min) (point-max))))

(defun lean4-infoview--insert-text (text kind &optional pos)
  (save-excursion
    (when pos
      (cl-destructuring-bind (:textDocument (:uri uri) :position position) pos
        (with-current-buffer (find-file-noselect (eglot-uri-to-path uri))
          (goto-char (eglot--lsp-position-to-point position)))))
    (pcase kind ("above" (forward-line -1)))
    (insert text)))

(defun lean4-infoview--apply-edit (te)
  (eglot--apply-workspace-edit te 'lean4-infoview))

(defun lean4-infoview--show-document (show)
  (apply 'eglot-handle-request nil 'window/showDocument show))

(defun lean4-infoview--restart-file (uri)
  (with-current-buffer (find-file-noselect (eglot-uri-to-path uri))
    (eglot-reconnect (eglot-current-server))))

(defun lean4-infoview--create-rpc-session (uri)
  (message "NOT IMPLEMENTED: create-rpc-session"))

(defun lean4-infoview--close-rpc-session (session-id)
  (message "NOT IMPLEMENTED: close-rpc-session"))


;;;; Websocket

; Thanks 

; Infoview requests are a JSON blob consisting of 


(provide 'lean4-infoview)
;;; lean4-infoview.el ends here
