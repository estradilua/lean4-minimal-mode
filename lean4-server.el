;;; lean4-server.el --- Eglot server class definition for lean4-mode  -*- lexical-binding: t; -*-

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

(require 'eglot)
(require 'websocket)
(require 'lean4-infoview)

;; Eglot subclass definition
(defclass eglot-lean4-server (eglot-lsp-server) ()
  :documentation "Eglot Lean4 server.")

(cl-defmethod eglot-handle-notification :after
  ((_ eglot-lean4-server) method &rest params)
  "Send server notifications to the subscribed infoviews."
  (dolist (conn lean4-infoview--connections)
    (when (memq method (oref conn server-watchers))
      (jsonrpc-notify conn :serverNotification
                      (list :method (symbol-name method)
                            :params params)))))

(cl-defmethod jsonrpc-connection-send :after
  ((_ eglot-lean4-server) &key _id method params _result _error)
  "Send client notifications to the subscribed infoviews."
  (dolist (conn lean4-infoview--connections)
    (when (memq method (oref conn client-watchers))
      (jsonrpc-notify conn :clientNotification
                      (list :method (symbol-name method)
                            :params params)))))

;; Setup Eglot
(add-hook 'lean4-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs '(lean4-mode eglot-lean4-server "lake" "serve"))

;; Commands (requests)
(defun lean4-restart-file ()
  "Refresh the file dependencies.

This function restarts the server subprocess for the current
file, recompiling, and reloading all imports."
  (interactive)
  (when eglot--managed-mode
    (eglot--signal-textDocument/didClose)
    (eglot--signal-textDocument/didOpen)))


(provide 'lean4-server)
;;; lean4-server.el ends here
