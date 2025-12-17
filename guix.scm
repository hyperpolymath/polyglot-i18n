;;; guix.scm --- GNU Guix package definition for polyglot-i18n
;;;
;;; Copyright (C) 2024-2025 polyglot-i18n contributors
;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;
;;; This file defines the GNU Guix package for polyglot-i18n,
;;; enabling reproducible builds and supply-chain integrity.

(define-module (polyglot-i18n)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system node)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system dune)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages node)
  #:use-module (gnu packages node-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages wasm)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages compression))

;;; Commentary:
;;;
;;; polyglot-i18n: A ReScript-first, WASM-accelerated translation module
;;; with dynamic JSON storage for polyglot applications. This package provides:
;;;
;;; - Core JavaScript/ReScript library
;;; - WebAssembly performance modules
;;; - Deno runtime support
;;; - CLI tools for locale management
;;; - Nickel configuration support
;;;
;;; Code:

;;;
;;; Utility Procedures
;;;

(define %source-dir
  (dirname (current-filename)))

(define (i18n-version)
  "Return the current version of polyglot-i18n."
  "2.0.0-alpha")

;;;
;;; WebAssembly Core Package (Rust)
;;;

(define-public i18n-wasm
  (package
    (name "i18n-wasm")
    (version (i18n-version))
    (source
     (local-file %source-dir
                 #:recursive? #t
                 #:select? (lambda (file stat)
                            (or (string-suffix? ".rs" file)
                                (string-suffix? "Cargo.toml" file)
                                (string-suffix? "Cargo.lock" file)))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-console-error-panic-hook" ,rust-console-error-panic-hook-0.1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-to-wasm
           (lambda _
             (chdir "wasm")))
         (add-after 'build 'build-wasm
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "wasm-pack" "build" "--target" "web"
                     "--out-dir" (string-append (assoc-ref outputs "out")
                                               "/lib/wasm")))))))
    (native-inputs
     (list wasm-pack rust-analyzer))
    (home-page "https://github.com/hyperpolymath/polyglot-i18n")
    (synopsis "WebAssembly performance core for polyglot-i18n")
    (description
     "High-performance WebAssembly modules for i18n operations including
CLDR plural rule evaluation, hash-based translation lookup, and string
interpolation.")
    (license license:expat)))

;;;
;;; ReScript Bindings Package
;;;

(define-public i18n-rescript
  (package
    (name "i18n-rescript")
    (version (i18n-version))
    (source
     (local-file %source-dir
                 #:recursive? #t
                 #:select? (lambda (file stat)
                            (or (string-suffix? ".res" file)
                                (string-suffix? ".resi" file)
                                (string=? "bsconfig.json" (basename file))))))
    (build-system node-build-system)
    (arguments
     `(#:tests? #f ; Tests run separately
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-to-rescript
           (lambda _
             (chdir "bindings/rescript")))
         (add-before 'build 'install-rescript
           (lambda _
             (invoke "npm" "install" "rescript@11")))
         (replace 'build
           (lambda _
             (invoke "npx" "rescript" "build"))))))
    (inputs
     (list node-lts))
    (home-page "https://github.com/hyperpolymath/polyglot-i18n")
    (synopsis "ReScript bindings for polyglot-i18n")
    (description
     "Type-safe ReScript bindings for the polyglot-i18n library,
providing ML-style syntax and compile-time type checking.")
    (license license:expat)))

;;;
;;; Main i18n Package
;;;

(define-public polyglot-i18n
  (package
    (name "polyglot-i18n")
    (version (i18n-version))
    (source
     (local-file %source-dir
                 #:recursive? #t
                 #:select? (lambda (file stat)
                            (not (or (string-prefix? "." (basename file))
                                    (string=? "node_modules" (basename file))
                                    (string-suffix? ".log" file))))))
    (build-system node-build-system)
    (arguments
     `(#:tests? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-wasm
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((wasm-dir (string-append (assoc-ref outputs "out")
                                            "/lib/node_modules/i18n/wasm")))
               (mkdir-p wasm-dir)
               (copy-recursively (string-append (assoc-ref inputs "i18n-wasm")
                                               "/lib/wasm")
                                wasm-dir))))
         (add-after 'install 'create-wrapper
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib/node_modules/i18n")))
               (mkdir-p bin)
               (call-with-output-file (string-append bin "/i18n")
                 (lambda (port)
                   (format port "#!~a~%exec ~a ~a/cli/main.js \"$@\"~%"
                           (which "bash")
                           (which "node")
                           lib)))
               (chmod (string-append bin "/i18n") #o755)))))))
    (inputs
     (list node-lts i18n-wasm))
    (native-inputs
     (list node-lts))
    (propagated-inputs
     (list i18n-rescript))
    (home-page "https://github.com/hyperpolymath/polyglot-i18n")
    (synopsis "ReScript-first translation module with dynamic JSON storage")
    (description
     "polyglot-i18n is a ReScript-first internationalization library featuring:
@itemize
@item Dynamic JSON storage for translations
@item Common __('...') syntax
@item Plural forms handling with CLDR rules
@item MessageFormat support for advanced formatting
@item Mustache template support
@item WebAssembly performance core
@item ReScript type-safe bindings
@item Deno runtime support
@end itemize")
    (license license:expat)))

;;;
;;; CLI Tools Package
;;;

(define-public i18n-cli
  (package
    (name "i18n-cli")
    (version (i18n-version))
    (source (package-source polyglot-i18n))
    (build-system node-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-man-pages
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((man1 (string-append (assoc-ref outputs "out")
                                        "/share/man/man1")))
               (mkdir-p man1)
               (for-each
                (lambda (cmd)
                  (install-file (string-append "man/man1/i18n-" cmd ".1")
                               man1))
                '("init" "translate" "validate" "sync" "extract"
                  "compile" "serve" "migrate" "benchmark" "config" "doctor")))))
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bash (string-append (assoc-ref outputs "out")
                                        "/share/bash-completion/completions"))
                   (zsh (string-append (assoc-ref outputs "out")
                                       "/share/zsh/site-functions"))
                   (fish (string-append (assoc-ref outputs "out")
                                        "/share/fish/vendor_completions.d")))
               (mkdir-p bash)
               (mkdir-p zsh)
               (mkdir-p fish)
               (install-file "completions/i18n.bash" bash)
               (install-file "completions/_i18n" zsh)
               (install-file "completions/i18n.fish" fish)))))))
    (inputs
     (list polyglot-i18n))
    (home-page "https://github.com/hyperpolymath/polyglot-i18n")
    (synopsis "Command-line tools for polyglot-i18n")
    (description
     "Comprehensive CLI for managing internationalization with commands for
validation, synchronization, extraction, and more.")
    (license license:expat)))

;;;
;;; Development Package (for hacking)
;;;

(define-public polyglot-i18n-dev
  (package
    (inherit polyglot-i18n)
    (name "polyglot-i18n-dev")
    (native-inputs
     (modify-inputs (package-native-inputs polyglot-i18n)
       (prepend rust
                rust-analyzer
                wasm-pack
                deno
                just
                nickel)))
    (synopsis "Development environment for polyglot-i18n")))

;;; guix.scm ends here
