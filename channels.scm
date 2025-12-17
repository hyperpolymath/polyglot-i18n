;;; channels.scm --- Guix channel configuration for polyglot-i18n
;;;
;;; Copyright (C) 2024-2025 polyglot-i18n contributors
;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;
;;; This file configures the Guix channels required for building and
;;; developing polyglot-i18n.

(list
 ;; Main GNU Guix channel
 (channel
  (name 'guix)
  (url "https://git.savannah.gnu.org/git/guix.git")
  (branch "master")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))

 ;; NonGNU packages (additional Node.js packages, Rust toolchain)
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (branch "master")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))

 ;; Guix Science channel (additional development tools)
 (channel
  (name 'guix-science)
  (url "https://github.com/guix-science/guix-science.git")
  (branch "master"))

 ;; polyglot-i18n channel (this project)
 (channel
  (name 'polyglot-i18n)
  (url "https://github.com/hyperpolymath/polyglot-i18n")
  (branch "main")))

;;; Usage:
;;;
;;; 1. Copy to ~/.config/guix/channels.scm
;;; 2. Run: guix pull
;;; 3. Install: guix install polyglot-i18n
;;;
;;; For development:
;;;   guix shell -D polyglot-i18n
;;;
;;; Or with manifest:
;;;   guix shell -m manifest.scm
