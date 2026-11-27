;;; Installed trees and the distributions made from them: `capy install`, `capy uninstall` and
;;; `capy dist`.

(library (capy pkg dist)
  (export write-toolchain-libmap!
    layout-toolchain!
    layout-package!
    make-dist-info
    dist-info?
    dist-info-name
    dist-info-version
    toolchain-dist-info
    package-dist-info
    target-label
    write-portable!
    write-deb!
    write-rpm!
    install-records-file
    read-install-records
    file-owner
    record-install!
    uninstall!)
  (import (rnrs)
    (only (capy) file-exists? file-directory? file-symbolic-link? directory-list create-directory*
      create-symbolic-link change-file-mode current-directory delete-directory*)
    (capy libmap)
    (capy path)
    (capy process)
    (capy pkg manifest)
    (capy pkg ui))

  ;;; Files

  (define (run! what program args . env)
    (guard (e [(process-failure? e)
               (fail (string-append what ": " (condition-message e)))])
      (run program args #:env (if (null? env) '() (car env)))))

  ;; Removes a file, a link or a directory tree; a path that is not there is not an error.
  (define (remove! path)
    (if (and (file-exists? path) (file-directory? path))
      (delete-directory* path)
      (guard (e [#t (when (file-exists? path) (raise e))])
        (delete-file path))))
  (define (copy! from to)
    (run! (string-append "copying " from " to " to) "cp" (list "-R" from to)))
  (define (link! target path) (create-symbolic-link target path))

  ;; FILE copied to TO. The file there is removed first: overwriting an executable in place keeps
  ;; macOS's cached code signature, and the kernel kills the new bytes.
  (define (replace! from to)
    (remove! to)
    (copy! from to))

  (define (directory? path) (and (file-exists? path) (file-directory? path)))

  ;; The files and links under DIR, relative to it, sorted.
  (define (tree-files dir)
    (let walk ([rel ""] [out '()])
      (fold-left
        (lambda (out name)
          (let* ([r (if (string=? rel "") name (path-join rel name))]
                 [full (path-join dir r)])
            (if (and (file-directory? full) (not (file-symbolic-link? full)))
              (walk r out)
              (cons r out))))
        out
        (list-sort string<? (directory-list (path-join dir rel))))))

  (define (string-ends? s suffix)
    (let ([n (string-length s)] [m (string-length suffix)])
      (and (>= n m) (string=? (substring s (- n m) n) suffix))))

  (define (join strings separator)
    (if (null? strings)
      ""
      (fold-left (lambda (out s) (string-append out separator s)) (car strings) (cdr strings))))

  ;;; The libmap of a toolchain directory

  ;; The id the libraries of a toolchain package have in their keys: the packages of the
  ;; toolchain share the plain keys of `core` and `std` (a library of `common` has the key
  ;; `(common par)`, as the library of `std` has `(rnrs base)`). See "Library keys of packages,
  ;; and views" in boot/linker.scm.
  (define (key-id manifest) (manifest-stage-key manifest))

  (define (manifest-view manifest manifests)
    (let* ([deps (manifest-path-dependencies manifest manifests)]
           [core (find manifest-boot-package? manifests)]
           [entries (map (lambda (pair)
                          (list (key-id (cdr pair)) (manifest-provides (cdr pair))
                            (dependency-renames (car pair))))
                      deps)]
           ;; core is in every view
           [entries (if (or (eq? manifest core) (assq 'core entries) (not core))
                      entries
                      (append entries (list (list 'core (manifest-provides core) '()))))])
      (make-package-view (manifest-name manifest) (manifest-provides manifest)
        entries (manifest-no-std? manifest) (key-id manifest))))

  ;; Writes DIR/capy.libmap, the libmap of the toolchain directory DIR, whose packages are the ones
  ;; of PKGS-DIR (`PKGS-DIR/*/package.scm`). It has the view of each package and an entry for every
  ;; library of every package, from the headers of its sources (`PKGS-DIR/P/src` and the libraries
  ;; the package generated, DIR/P/out), with the compiled file (DIR/P/fasl) of the ones that have a
  ;; current one, and the native extensions in DIR/P/native. The paths are relative to DIR. For an
  ;; install DIR holds the packages and PKGS-DIR is DIR.
  (define (write-toolchain-libmap! dir pkgs-dir)
    (let* ([manifests (read-manifest-tree pkgs-dir)]
           [sub (lambda (m name) (path-join dir (symbol->string (manifest-name m)) name))])
      (write-libmap! (path-join dir "capy.libmap")
        (filter directory? (map (lambda (m) (sub m "fasl")) manifests))
        (append
          (map (lambda (m) (list (manifest-source-directory m) (key-id m) (manifest-edition m))) manifests)
          (filter values
            (map (lambda (m) (and (directory? (sub m "out")) (cons (sub m "out") (key-id m)))) manifests)))
        (map (lambda (m) (manifest-view m manifests)) manifests)
        (map (lambda (m) (sub m "native")) manifests))))

  ;;; Layouts

  ;; The name libcapy has in DIR (libcapy.dylib or libcapy.so), or #f when it is not there.
  (define (libcapy-in dir)
    (find (lambda (name) (file-exists? (path-join dir name))) '("libcapy.dylib" "libcapy.so")))

  (define (layout-toolchain! ui stage pkgs-dir tree dest version)
    (let* ([runtime (path-join stage "fhs")]
           [lib (or (libcapy-in runtime) "libcapy.so")]
           [libroot (path-join dest "lib" "capy" version)]
           [bin (path-join dest "bin")])
      (unless (file-exists? (path-join runtime "capy-rt"))
        (fail (string-append runtime "/capy-rt is missing: the installed runtime is built by ./bootstrap.sh"
                " (without --no-fhs)")))
      (unless (file-exists? (path-join stage "capy.libmap"))
        (fail (string-append stage " is not a toolchain directory (it has no capy.libmap)")))
      (for-each create-directory* (list bin (path-join dest "lib") (path-join dest "include") libroot))
      (ui-status! ui "Installing" (string-append "the runtime into " dest))
      (replace! (path-join runtime "capy-rt") (path-join bin "capy-rt"))
      (remove! (path-join bin "capyc"))
      (link! "capy-rt" (path-join bin "capyc"))
      (replace! (path-join runtime lib) (path-join dest "lib" lib))
      (remove! (path-join bin lib))
      (remove! (path-join bin "lib"))
      (for-each
        (lambda (name) (replace! (path-join runtime name) (path-join dest "lib" name)))
        '("libcapy_static.a" "libcapy_static.libs"))
      (replace! (path-join tree "c" "capy.h") (path-join dest "include" "capy.h"))
      ;; the packages: sources from the tree, compiled files and native extensions from the stage
      (for-each
        (lambda (name)
          (when (and (directory? (path-join stage name))
                     (file-exists? (path-join pkgs-dir name "package.scm")))
            (let ([to (path-join libroot name)])
              (ui-status! ui "Installing" (string-append "the package " name))
              (remove! to)
              (create-directory* to)
              (copy! (path-join pkgs-dir name "src") (path-join to "src"))
              (copy! (path-join pkgs-dir name "package.scm") (path-join to "package.scm"))
              (for-each
                (lambda (sub)
                  (when (directory? (path-join stage name sub))
                    (copy! (path-join stage name sub) (path-join to sub))))
                '("fasl" "out" "native")))))
        (list-sort string<? (directory-list stage)))
      (let ([cache (path-join dest ".capy-libmap-cache")]
            [quoted (lambda (s) (call-with-string-output-port (lambda (p) (write s p))))])
        (run! "writing the libmap" (path-join stage "capy-rt")
          (list "-c" (string-append "(import (capy pkg dist)) (write-toolchain-libmap! "
                       (quoted libroot) " " (quoted libroot) ")"))
          (list (cons "XDG_CACHE_HOME" cache)))
        (remove! cache))
      (ui-status! ui "Linking" "the driver, bin/capy")
      (let ([cache (path-join dest ".capy-link-cache")])
        (guard (e [#t (remove! cache) (raise e)])
          (run! "linking the driver" (path-join bin "capy-rt")
            (list "--compile" "--static" "--exe" "-o" (path-join bin "capy.new")
              (path-join libroot "capy-pkg" "src" "capy" "pkg" "main.scm"))
            (list (cons "XDG_CACHE_HOME" cache)))
          (replace! (path-join bin "capy.new") (path-join bin "capy"))
          (remove! (path-join bin "capy.new"))
          (remove! cache)))
      (remove! (path-join bin lib))     ; the copy --exe made; the executables find ../lib
      (remove! (path-join bin "lib"))))  ; and the extensions it copied, which are in the toolchain

  ;; The programs PROGRAMS of a build (pairs of a name and the path of an executable), laid out
  ;; under DEST. Returns the files written, relative to DEST.
  (define (layout-package! ui programs dest)
    (let ([bin (path-join dest "bin")])
      (create-directory* bin)
      (fold-left
        (lambda (written program)
          (let* ([name (car program)]
                 [exe (cdr program)]
                 [dir (path-directory exe)]
                 [lib (libcapy-in dir)]
                 [extensions (path-join dir "lib")])
            (ui-status! ui "Installing" (string-append name " into " bin))
            (replace! exe (path-join bin name))
            (append written
              (list (path-join "bin" name))
              (if lib
                (begin
                  (create-directory* (path-join dest "lib"))
                  (replace! (path-join dir lib) (path-join dest "lib" lib))
                  (list (path-join "lib" lib)))
                '())
              (if (directory? extensions)
                (begin
                  (for-each
                    (lambda (package)
                      (create-directory* (path-join bin "lib"))
                      (replace! (path-join extensions package) (path-join bin "lib" package)))
                    (directory-list extensions))
                  (map (lambda (f) (path-join "bin" "lib" f)) (tree-files extensions)))
                '()))))
        '()
        programs)))

  ;;; Metadata

  ;; DEPENDS and REQUIRES are the Depends of a deb and the Requires of an rpm, strings; "" for none.
  (define-record-type dist-info
    (fields name version summary description license maintainer homepage depends requires))

  (define (toolchain-dist-info version)
    (make-dist-info "capyscheme" version
      "CapyScheme (runtime + compiler)"
      "A Scheme implementation with a native runtime and compiler."
      "LGPL3+"
      "Adel Prokurov <adel.prokurov@gmail.com>"
      "https://github.com/playX18/capyscheme"
      "libc6 (>= 2.39), libgcc-s1 (>= 12)"
      ""))

  (define (package-dist-info manifest version)
    (let* ([meta (lambda (key default) (manifest-metadata-ref manifest key default))]
           [name (symbol->string (manifest-name manifest))]
           [description (meta 'description (string-append name ", a CapyScheme package"))]
           [authors (meta 'authors '())]
           [dist (meta 'dist '())]
           [dist-field (lambda (format key)
                         (let* ([section (assq format dist)]
                                [entry (and section (assq key (cdr section)))])
                           (if entry (join (cdr entry) ", ") "")))])
      (make-dist-info name (or version (manifest-version manifest))
        description description
        (meta 'license "unknown")
        (if (pair? authors) (car authors) "unknown")
        (meta 'homepage "")
        (dist-field 'deb 'depends)
        (dist-field 'rpm 'requires))))

  ;; The label of the platform in the name of a portable archive, `x86_64-unknown-linux-gnu`
  ;; and the like: what `uname` says, in the words of a Rust target.
  (define (target-label)
    (let* ([word (lambda (flag)
                   (let ([s (run/string "uname" (list flag))])
                     (if (string-ends? s "\n") (substring s 0 (- (string-length s) 1)) s)))]
           [machine (word "-m")]
           [machine (if (string=? machine "arm64") "aarch64" machine)]
           [system (word "-s")])
      (cond
        [(string=? system "Darwin") (string-append machine "-apple-darwin")]
        [(string=? system "Linux") (string-append machine "-unknown-linux-gnu")]
        [(string=? system "FreeBSD") (string-append machine "-unknown-freebsd")]
        [else (fail (string-append "cannot name the target of this system (uname -s says " system
                     ")"))])))

  (define (deb-architecture)
    (guard (e [#t (let ([m (run/string "uname" '("-m"))])
                    (cond [(string-ends? m "x86_64\n") "amd64"]
                      [(or (string-ends? m "aarch64\n") (string-ends? m "arm64\n")) "arm64"]
                      [else (substring m 0 (- (string-length m) 1))]))])
      (let ([a (run/string "dpkg" '("--print-architecture"))])
        (substring a 0 (- (string-length a) 1)))))

  (define (have? program)
    (guard (e [#t #f])
      (run "sh" (list "-c" "command -v \"$1\" >/dev/null" "sh" program))
      #t))

  ;;; Distributions
  ;;
  ;; LAYOUT! takes the directory that stands for the prefix and lays the tree out in it. EXTRAS
  ;; are files copied into the top of a portable archive (LICENSE, CHANGELOG.md). WORK is a
  ;; scratch directory, OUT the directory the result goes to; each returns the path it wrote.

  (define (write-portable! ui info layout! extras work out archive-name)
    (let* ([top (string-append (dist-info-name info) "-" (dist-info-version info))]
           [root (path-join work top)]
           [archive (path-join out (or archive-name
                                     (string-append top "-" (target-label) ".tar.gz")))])
      (remove! work)
      (create-directory* root)
      (create-directory* out)
      (layout! root)
      (for-each (lambda (file) (when (file-exists? file) (copy! file root))) extras)
      (ui-status! ui "Packing" archive)
      (run! "making the archive" "tar" (list "-C" work "-czf" archive top))
      archive))

  (define (write-deb! ui info layout! work out)
    (let* ([root (path-join work "deb")]
           [usr (path-join root "usr")]
           [version (let ([v (dist-info-version info)])
                      (if (and (> (string-length v) 0) (char=? (string-ref v 0) #\v))
                        (substring v 1 (string-length v))
                        v))]
           [arch (deb-architecture)]
           [deb (path-join out (string-append (dist-info-name info) "_" version "_" arch ".deb"))])
      (remove! work)
      (create-directory* (path-join root "DEBIAN"))
      (create-directory* out)
      (layout! usr)
      (let ([size (let ([s (run/string "du" (list "-sk" usr))])
                    (let loop ([i 0])
                      (if (or (= i (string-length s)) (not (char-numeric? (string-ref s i))))
                        (substring s 0 i)
                        (loop (+ i 1)))))])
        (call-with-output-file (path-join root "DEBIAN" "control")
          (lambda (port)
            (for-each
              (lambda (line) (unless (string=? (cdr line) "") (put-string port (string-append (car line) ": " (cdr line) "\n"))))
              (list (cons "Package" (dist-info-name info))
                (cons "Version" version)
                (cons "Section" "devel")
                (cons "Priority" "optional")
                (cons "Architecture" arch)
                (cons "Maintainer" (dist-info-maintainer info))
                (cons "Depends" (dist-info-depends info))
                (cons "Homepage" (dist-info-homepage info))
                (cons "Installed-Size" size)
                (cons "Description" (string-append (dist-info-summary info) "\n "
                                      (dist-info-description info))))))))
      (change-file-mode (path-join root "DEBIAN") #o755)
      (ui-status! ui "Packing" deb)
      (if (have? "fakeroot")
        (run! "making the deb" "fakeroot" (list "dpkg-deb" "--build" root deb))
        (run! "making the deb" "dpkg-deb" (list "--build" root deb)))
      deb))

  ;; The rpm owns the entries two levels below usr/ (usr/bin/capy, usr/lib/capy, ...): the
  ;; directories among them with everything in them.
  (define (write-rpm! ui info layout! work out)
    (let* ([work (path-join (current-directory) work)]
           [out (path-join (current-directory) out)]
           [tree (path-join work "root")]
           [usr (path-join tree "usr")]
           [top (path-join work "rpmbuild")]
           [spec (path-join top "SPECS" (string-append (dist-info-name info) ".spec"))]
           [version (let ([v (dist-info-version info)])
                      ;; an rpm version has no `-`; `~` sorts a pre-release below the release
                      (list->string (map (lambda (c) (if (char=? c #\-) #\~ c)) (string->list v))))])
      (remove! work)
      (for-each (lambda (d) (create-directory* (path-join top d))) '("SPECS" "BUILD" "BUILDROOT" "RPMS" "SRPMS" "RPMDB"))
      (create-directory* out)
      (layout! usr)
      (let ([owned (fold-left
                     (lambda (out d)
                       (append out (map (lambda (name) (string-append "/usr/" d "/" name))
                                     (list-sort string<? (directory-list (path-join usr d))))))
                     '()
                     (list-sort string<? (filter (lambda (d) (directory? (path-join usr d))) (directory-list usr))))])
        (call-with-output-file spec
          (lambda (port)
            (for-each (lambda (line) (put-string port line) (newline port))
              (append
                (list (string-append "Name:           " (dist-info-name info))
                  (string-append "Version:        " version)
                  "Release:        1%{?dist}"
                  (string-append "Summary:        " (dist-info-summary info))
                  (string-append "License:        " (dist-info-license info)))
                (if (string=? (dist-info-homepage info) "") '()
                  (list (string-append "URL:            " (dist-info-homepage info))))
                (if (string=? (dist-info-requires info) "") '()
                  (list (string-append "Requires:       " (dist-info-requires info))))
                (list ""
                  "%description" (dist-info-description info) ""
                  "%install"
                  "mkdir -p %{buildroot}"
                  (string-append "cp -a " usr " %{buildroot}/")
                  ""
                  "%files")
                owned)))))
      (ui-status! ui "Packing" (string-append "the rpm into " out))
      (run! "making the rpm" "rpmbuild"
        (list "-bb" spec
          "--define" (string-append "_topdir " top)
          "--define" (string-append "_dbpath " (path-join top "RPMDB"))
          "--define" "debug_package %{nil}"))
      (let ([rpms (filter (lambda (f) (string-ends? f ".rpm")) (tree-files (path-join top "RPMS")))])
        (when (null? rpms) (fail "rpmbuild wrote no rpm"))
        (map (lambda (f)
               (let ([to (path-join out (path-file-name f))])
                 (replace! (path-join top "RPMS" f) to)
                 to))
          rpms))))

  ;;; Install records
  ;;
  ;; PREFIX/.capy-installs.scm holds one datum, a list of (NAME (version V) (source S) (files F ...)),
  ;; with the files relative to PREFIX.

  (define (install-records-file prefix) (path-join prefix ".capy-installs.scm"))

  (define (read-install-records prefix)
    (let ([file (install-records-file prefix)])
      (if (file-exists? file)
        (call-with-input-file file
          (lambda (port) (let ([d (read port)]) (if (eof-object? d) '() d))))
        '())))

  (define (write-install-records! prefix records)
    (create-directory* prefix)
    (let ([file (install-records-file prefix)])
      (when (file-exists? file) (delete-file file))
      (call-with-output-file file
        (lambda (port)
          (put-string port ";; The packages `capy install` installed here, and their files.\n(")
          (for-each (lambda (r) (newline port) (put-string port "  ") (write r port)) records)
          (put-string port ")\n")))))

  (define (record-field record key) (cdr (assq key (cdr record))))

  ;; Records that the package NAME is installed under PREFIX with the files FILES. The files of
  ;; an earlier install of NAME that the new one did not write are removed.
  (define (record-install! prefix name version source files)
    (let* ([records (read-install-records prefix)]
           [old (assoc name records)]
           [others (remp (lambda (r) (equal? (car r) name)) records)])
      (when old
        (for-each
          (lambda (f) (unless (member f files) (remove! (path-join prefix f))))
          (record-field old 'files)))
      (write-install-records! prefix
        (append others (list (list name (list 'version version) (list 'source source) (cons 'files files)))))))

  ;; The owner, among the records of PREFIX, of the file FILE (relative to PREFIX), or #f.
  (define (file-owner records file)
    (find (lambda (r) (member file (record-field r 'files))) records))

  ;; Removes the files of the package NAME and its record. Returns the files it removed.
  (define (uninstall! prefix name)
    (let* ([records (read-install-records prefix)]
           [record (assoc name records)])
      (unless record
        (fail (string-append "the package " name " is not installed in " prefix)))
      (for-each (lambda (f) (remove! (path-join prefix f))) (record-field record 'files))
      (write-install-records! prefix (remp (lambda (r) (equal? (car r) name)) records))
      (record-field record 'files)))
  )
