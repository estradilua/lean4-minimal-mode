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
(require 'lean4-server)

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

(defun lean4-infoview--server (port lsp-server &optional host)
  "Create a websocket listening at HOST:PORT for server LSP-SERVER."
  (let ((server (websocket-server
                 port
                 :host (or host 'local)
                 :on-open (lambda (socket)
                            (lean4-infoview--conn-open lsp-server socket))
                 :on-close (lambda (socket)
                             (lean4-infoview--conn-close lsp-server socket))
                 :on-message #'lean4-infoview--conn-message)))
    (oset lsp-server socket-server server)))

;;;; Editor API implementation

(defclass lean4-infoview--connection (jsonrpc-connection)
  ((socket :initarg :socket)
   (client-watchers :initform nil)
   (server-watchers :initform nil))
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
    (websocket-send-text (oref connection socket) json)
    (jsonrpc--event
     connection
     'client
     :json json
     :kind kind
     :message args
     :foreign-message converted)))

(defun lean4-infoview--conn-open (lsp-server socket)
  (let ((conn (lean4-infoview--connection
               :socket socket
               :name "Lean4 Infoview"
               :events-buffer-config '(:size 2000000 :format full)
               :request-dispatcher #'lean4-infoview--dispatcher
               :notification-dispatcher #'lean4-infoview--dispatcher)))
    (setf (websocket-client-data socket) conn)
    (push conn (oref lsp-server infoviews))))

(defun lean4-infoview--conn-close (lsp-server socket)
  (oset lsp-server infoviews
        (cl-delete socket (oref lsp-server infoviews)
                   :key (lambda (i) (oref i socket)))))

(defun lean4-infoview--conn-message (socket frame)
  (let* ((conn (websocket-client-data socket))
         (json (websocket-frame-text frame))
         (msg (json-parse-string json
                                 :object-type 'plist
                                 :null-object nil
                                 :false-object :json-false)))
    (jsonrpc-connection-receive conn (plist-put msg :jsonrpc-json json))))

(cl-defgeneric lean4-infoview--dispatcher (conn method params))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql saveConfig)) params)
  (cl-destructuring-bind (&key config) params
    (message "NOT IMPLEMENTED: save-config")))

(cl-defmethod lean4-infoview--dispatcher
  (conn (_ (eql sendClientRequest)) params)
  (cl-destructuring-bind (&key uri method params) params
    (with-current-buffer (find-file-noselect (eglot-uri-to-path uri))
      (eglot--request (eglot--current-server-or-lose) method params))))

(cl-defmethod lean4-infoview--dispatcher
  (conn (_ (eql sendClientNotification)) params)
  (cl-destructuring-bind (&key uri method params) params
    (with-current-buffer (find-file-noselect (eglot-uri-to-path uri))
      (jsonrpc-notify (eglot--current-server-or-lose) method params))))

(cl-defmethod lean4-infoview--dispatcher
  (conn (_ (eql subscribeServerNotifications)) params)
  (cl-destructuring-bind (&key method) params
    (push (intern method) (oref conn server-watchers))))

(cl-defmethod lean4-infoview--dispatcher
  (conn (_ (eql unsubscribeServerNotifications)) params)
  (cl-destructuring-bind (&key method) params
    (oset conn server-watchers
          (cl-delete (intern method) (oref conn server-watchers)
                     :count 1))))

(cl-defmethod lean4-infoview--dispatcher
  (conn (_ (eql subscribeClientNotifications)) params)
  (cl-destructuring-bind (&key method) params
    (push (intern method) (oref conn client-watchers))))

(cl-defmethod lean4-infoview--dispatcher
  (conn (_ (eql unsubscribeClientNotifications)) params)
  (cl-destructuring-bind (&key method) params
    (oset conn client-watchers
          (cl-delete (intern method) (oref conn client-watchers)
                     :count 1))))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql copyToClipboard)) params)
  (cl-destructuring-bind (&key text) params
    (with-temp-buffer
      (insert text)
      (clipboard-kill-ring-save (point-min) (point-max)))))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql insertText)) params)
  (cl-destructuring-bind (&key text kind pos) params
    (save-excursion
      (when pos
        (cl-destructuring-bind (&key textDocument position) pos
          (cl-destructuring-bind (&key uri) textDocument
            (with-current-buffer (find-file-noselect (eglot-uri-to-path uri))
              (goto-char (eglot--lsp-position-to-point position))))))
      (when (equal kind "above")
        (forward-line -1))
      (insert text))))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql applyEdit)) params)
  (cl-destructuring-bind (&key te) params
    (eglot--apply-workspace-edit te 'lean4-infoview)))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql showDocument)) params)
  (cl-destructuring-bind (&key show) params
    (apply 'eglot-handle-request nil 'window/showDocument show)))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql restartFile)) params)
  (cl-destructuring-bind (&key uri) params
    (with-current-buffer (find-file-noselect (eglot-uri-to-path uri))
      (eglot-reconnect (eglot-current-server)))))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql createRpcSession)) params)
  (cl-destructuring-bind (&key uri) params
    (message "NOT IMPLEMENTED: create-rpc-session")))

(cl-defmethod lean4-infoview--dispatcher
  (_ (_ (eql closeRpcSession)) params)
  (cl-destructuring-bind (&key sessionId) params
    (message "NOT IMPLEMENTED: close-rpc-session")))

;; Handle subscribed notifications

(cl-defmethod eglot-handle-notification :after
  ((server eglot-lean4-server) method &rest params)
  "Handle subscribed server notifications and send them to infoview."
  (dolist (conn (oref server infoviews))
    (when (memq method (oref conn server-watchers))
      (jsonrpc-notify conn :serverNotification (list :method (symbol-name method)
                                                     :params params)))))

(cl-defmethod jsonrpc-connection-send :after
  ((server eglot-lean4-server) &key _id method params _result _error)
  "Handle subscribed client notifications and send them to infoview."
  (dolist (conn (oref server infoviews))
    (when (memq method (oref conn client-watchers))
      (jsonrpc-notify conn :clientNotification (list :method (symbol-name method)
                                                     :params params)))))

;;;; HTTP server

(provide 'lean4-infoview)
;;; lean4-infoview.el ends here
