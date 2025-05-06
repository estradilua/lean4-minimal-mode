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

(defclass lean4-infoview--connection (jsonrpc-connection)
  ((socket :initarg :socket))
  :documentation "Represents a connection to an infoview window.")

(cl-defmethod jsonrpc-connection-send ((connection lean4-infoview--connection)
                                       &rest args
                                       &key
                                       id
                                       method
                                       _params
                                       (_result nil result-supplied-p)
                                       error)
  "Send MESSAGE, a JSON object, to CONNECTION."
  (when method
    (setq args
          (plist-put args :method
                     (cond ((keywordp method) (substring (symbol-name method) 1))
                           ((symbolp method) (symbol-name method))
                           ((stringp method) method)
                           (t (error "[jsonrpc] invalid method %s" method))))))
  (let* ((kind (cond ((or result-supplied-p error) 'reply)
                     (id 'request)
                     (method 'notification)))
         (converted (jsonrpc-convert-to-endpoint connection args kind))
         (json (jsonrpc--json-encode converted)))
    (websocket-send-text (oref socket connection) json)
    (jsonrpc--event
     connection
     'client
     :json json
     :kind kind
     :message args
     :foreign-message converted)))

(defun lean4-infoview--server (port &rest args)
  (websocket-server port
                    :host (or (plist-get :host args) 'local)
                    :on-open #'lean4-infoview--conn-open
                    :on-message #'lean4-infoview--conn-message))

(defun lean4-infoview--conn-open (socket)
  (let ((conn (lean4-infoview--connection
               :socket socket
               :request-dispatcher #'lean4-infoview--dispatcher
               :notification-dispatcher #'lean4-infoview--dispatcher)))
    (setf (websocket-client-data socket) conn)))

(defun lean4-infoview--conn-message (socket frame)
  (let ((conn (websocket-client-data socket))
        (msg (json-parse-string (websocket-frame-text frame)
                                :object-type 'plist
                                :null-object nil
                                :false-object :json-false)))
    (jsonrpc-connection-receive conn msg)))

(cl-defgeneric lean4-infoview--dispatcher (conn method params))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql saveConfig)) &key config)
  (message "NOT IMPLEMENTED: save-config"))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql sendClientRequest)) &key uri method params)
  (with-current-buffer (find-file-noselect (eglot-uri-to-path uri))
    (eglot--request (eglot--current-server-or-lose) method params)))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql sendClientNotification)) &key uri method params)
  (with-current-buffer (find-file-noselect (eglot-uri-to-path uri))
    (jsonrpc-notify (eglot--current-server-or-lose) method params)))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql subscribeServerNotifications)) &key method)
  (message "NOT IMPLEMENTED: subscribe-server-notifications"))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql unsubscribeServerNotifications)) &key method)
  (message "NOT IMPLEMENTED: unsubscribe-server-notifications"))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql subscribeClientNotifications)) &key method)
  (message "NOT IMPLEMENTED: subscribe-client-notifications"))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql unsubscribeClientNotifications)) &key method)
  (message "NOT IMPLEMENTED: unsubscribe-client-notifications"))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql copyToClipboard)) &key text)
  (with-temp-buffer
    (insert text)
    (clipboard-kill-ring-save (point-min) (point-max))))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql insertText)) &key text kind pos)
  (save-excursion
    (when pos
      (cl-destructuring-bind (:textDocument (:uri uri) :position position) pos
        (with-current-buffer (find-file-noselect (eglot-uri-to-path uri))
          (goto-char (eglot--lsp-position-to-point position)))))
    (when (equal kind "above")
      (forward-line -1))
    (insert text)))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql applyEdit)) &key te)
  (eglot--apply-workspace-edit te 'lean4-infoview))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql showDocument)) &key show)
  (apply 'eglot-handle-request nil 'window/showDocument show))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql restartFile)) &key uri)
  (with-current-buffer (find-file-noselect (eglot-uri-to-path uri))
    (eglot-reconnect (eglot-current-server))))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql createRpcSession)) &key uri)
  (message "NOT IMPLEMENTED: create-rpc-session"))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql closeRpcSession)) &key sessionId)
  (message "NOT IMPLEMENTED: close-rpc-session"))


;;;; HTTP server

(provide 'lean4-infoview)
;;; lean4-infoview.el ends here
