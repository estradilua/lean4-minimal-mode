;;; lean4-settings.el --- Custom variables for lean4-mode -*- lexical-binding: t -*-

;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Author: Soonho Kong
;; SPDX-License-Identifier: Apache-2.0
;;

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

;; This library defines custom variables for `lean4-mode'.

;;; Code:

(require 'cl-lib)

(defgroup lean4 nil
  "Lean 4 programming language and theorem prover."
  :prefix "lean4-"
  :group 'languages
  :link '(url-link :tag "Website" "http://leanprover.github.io")
  :link '(url-link :tag "Github"  "https://github.com/leanprover/lean4"))

(defconst lean4-default-executable-name
  (cl-case system-type
    (windows-nt "lean.exe")
    (t          "lean"))
  "Default executable name of Lean.")

(defconst lean4-default-lake-name
  (cl-case system-type
    (windows-nt "lake.exe")
    (t          "lake"))
  "Default executable name of Lake.")

(defcustom lean4-rootdir nil
  "Full pathname of lean root directory.  It should be defined by user."
  :group 'lean
  :type 'string)

(defcustom lean4-executable-name lean4-default-executable-name
  "Name of lean executable."
  :group 'lean
  :type 'string)

(defcustom lean4-lake-name lean4-default-lake-name
  "Name of lake executable."
  :group 'lake
  :type 'string)

(defcustom lean4-show-file-progress t
  "Highlight file progress in the current buffer."
  :group 'lean
  :type 'boolean)

(provide 'lean4-settings)
;;; lean4-settings.el ends here
