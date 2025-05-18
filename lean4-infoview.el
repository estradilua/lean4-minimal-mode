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

(defvar lean4-infoview-port 6174
  "Port for the websocket server.")

(defvar lean4-infoview-host 'local
  "Host for the websocket server.")

(defvar lean4-infoview--server nil
  "The global infoview websocket server.")

(defvar lean4-infoview--connections nil
  "Global list of infoview connections to server.")

(defun lean4-infoview--start-server ()
  "Start or restart the infoview server."
  (when lean4-infoview--server
    (websocket-server-close lean4-infoview--server))
  (setq lean4-infoview--server
        (websocket-server
         lean4-infoview-port
         :host lean4-infoview-host
         :on-open #'lean4-infoview--conn-open
         :on-close #'lean4-infoview--conn-close
         :on-message #'lean4-infoview--conn-message)))

;;;; Websocket RPC connection

(defclass lean4-infoview--connection (jsonrpc-connection)
  ((socket :initarg :socket)
   (client-watchers :initform nil)
   (server-watchers :initform nil)
   (rpc-sessions :initform nil))
  :documentation "Represents a connection to an infoview window.")

(cl-defmethod jsonrpc-connection-send ((connection lean4-infoview--connection)
                                       &rest args
                                       &key
                                       id
                                       method
                                       _params
                                       (_result nil result-supplied-p)
                                       error)
  "Send message ARGS to CONNECTION."
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

(defun lean4-infoview--location ()
  "Return a Location with the active mark or the point position."
  (cl-destructuring-bind (start . end)
      (if mark-active
          (cons (region-beginning) (region-end))
        (cons (point) (point)))
    (ignore-errors
      (apply #'list 
            :range (list :start
                         (eglot--pos-to-lsp-position start)
                         :end
                         (eglot--pos-to-lsp-position end))
            (eglot--TextDocumentIdentifier)))))

(defun lean4-infoview--conn-open (socket)
  "Open a connection for infoview using the given SOCKET."
  (let ((conn (lean4-infoview--connection
               :socket socket
               :name "Lean4 Infoview"
               :events-buffer-config '(:size 2000000 :format full)
               :request-dispatcher #'lean4-infoview--dispatcher
               :notification-dispatcher #'lean4-infoview--dispatcher)))
    (setf (websocket-client-data socket) conn)
    (push conn lean4-infoview--connections)

    ;; Initialize
    (jsonrpc-notify conn :initialize
                    (list :loc (lean4-infoview--location)))
    (lean4-infoview--send-initialize (eglot-current-server))))

(defun lean4-infoview--conn-close (socket)
  "Remove the connection of SOCKET from `lean4-infoview--connections'."
  (dolist (s (oref (websocket-client-data socket) rpc-sessions))
    (cancel-timer (cdr s)))
  (setq lean4-infoview--connections
        (cl-delete socket lean4-infoview--connections
                   :key (lambda (i) (oref i socket)))))

(defun lean4-infoview--conn-message (socket frame)
  "Receive RPC message FRAME from websocket SOCKET."
  (let* ((conn (websocket-client-data socket))
         (json (websocket-frame-text frame))
         (msg (json-parse-string json
                                 :object-type 'plist
                                 :null-object nil
                                 :false-object :json-false)))
    (jsonrpc-connection-receive conn (plist-put msg :jsonrpc-json json))))

;;;; Editor API implementation

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
  (conn (_ (eql createRpcSession)) params)
  (cl-destructuring-bind (&key uri) params
    (with-current-buffer (find-file-noselect (eglot-uri-to-path uri))
      (let* ((server (eglot-current-server))
             ;; todo: could this be async?
             (session-id (jsonrpc-request server
                                          :$/lean/rpc/connect
                                          (list :uri uri)))
             (keepalive (run-with-timer
                         10 10
                         (jsonrpc-notify server
                                         :$/lean/rpc/keepAlive
                                         (list :uri uri
                                               :sessionId session-id)))))
        (push (cons session-id keepalive) (oref conn rpc-sessions))
        session-id))))

(cl-defmethod lean4-infoview--dispatcher
  (conn (_ (eql closeRpcSession)) params)
  (cl-destructuring-bind (&key sessionId) params
    (oset conn rpc-sessions
          (delq nil (map-apply
                     (lambda (id timer)
                       (if (string= id sessionId)
                           (cancel-timer timer)
                         (cons id timer)))
                     (oref conn rpc-sessions))))))

(defun lean4-infoview--send-location ()
  "Send current location to all connections."
  (dolist (conn lean4-infoview--connections)
    (jsonrpc-notify conn :changedCursorLocation
                    (list :loc (lean4-infoview--location)))))

(defun lean4-infoview--send-initialize (server)
  "Send initialization info of SERVER to all connections."
  (when (object-of-class-p server 'eglot-lean4-server)
    (with-slots (capabilities server-info) server
      (dolist (conn lean4-infoview--connections)
        (jsonrpc-notify conn :serverRestarted
                        (list :result
                              (list :capabilities capabilities
                                    :serverInfo server-info)))))))

(add-hook 'eglot-connect-hook #'lean4-infoview--send-initialize)

;;;; HTTP server

(provide 'lean4-infoview)
;;; lean4-infoview.el ends here
