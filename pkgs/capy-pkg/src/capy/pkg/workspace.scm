;;; Workspaces

(library (capy pkg workspace)
  (export find-project
    find-project-or-false
    project?
    project-root
    project-workspace
    project-members
    project-current
    project-member-manifests
    select-members
    single-member)
  (import (rnrs)
    (only (capy) file-exists? canonicalize-path-string)
    (capy path)
    (capy pkg manifest)
    (capy pkg ui))

  ;; ROOT is the directory of target/ and the lock, WORKSPACE the workspace or #f, MEMBERS the
  ;; canonical directories of the members (the package alone outside a workspace), CURRENT the
  ;; member the command was started in, or #f at the root of a virtual workspace.
  (define-record-type project
    (fields root workspace members current))

  (define (nearest-manifest-directory start)
    (let loop ([dir start])
      (cond
        [(file-exists? (path-join dir "package.scm")) dir]
        [(string=? (path-directory dir) dir) #f]
        [else (loop (path-directory dir))])))

  (define (find-project-or-false start)
    (let ([dir (nearest-manifest-directory (canonicalize-path-string start))])
      (and dir
        (let* ([dir (canonicalize-path-string dir)]
               [own (read-workspace-file (path-join dir "package.scm"))])
          (cond
            [own
              (make-project dir own (map canonicalize-path-string (workspace-member-directories own))
                (and (workspace-root-package? own) dir))]
            [(find-enclosing-workspace dir)
              => (lambda (ws)
                   (make-project (workspace-directory ws) ws
                     (map canonicalize-path-string (workspace-member-directories ws)) dir))]
            [else (make-project dir #f (list dir) dir)])))))

  (define (find-project start)
    (or (find-project-or-false start)
      (fail "could not find package.scm in the current directory or any parent")))

  ;; (directory . manifest) of every member.
  (define (project-member-manifests project)
    (map (lambda (dir) (cons dir (read-manifest (path-join dir "package.scm"))))
      (project-members project)))

  (define (member-names project)
    (let loop ([ms (project-member-manifests project)] [out ""])
      (if (null? ms)
        out
        (loop (cdr ms)
          (string-append out (if (string=? out "") "" ", ") (symbol->string (manifest-name (cdr (car ms)))))))))

  ;; The directories of the members to work on. NAMES are the names given with -p, ALL? is
  ;; --workspace.
  (define (select-members project names all?)
    (cond
      [all? (project-members project)]
      [(pair? names)
        (let ([manifests (project-member-manifests project)])
          (map (lambda (name)
                 (cond
                   [(find (lambda (m) (string=? (symbol->string (manifest-name (cdr m))) name)) manifests)
                     => car]
                   [else
                     (fail (string-append "the " (if (project-workspace project) "workspace" "package")
                             " has no member " name " (its members: " (member-names project) ")"))]))
            names))]
      [(project-current project) => list]
      [else (project-members project)]))

  ;; The one member to work on, for the commands that take one (repl, add, remove). WHAT names
  ;; the command in the error.
  (define (single-member project names all? what)
    (let ([dirs (select-members project names all?)])
      (unless (and (pair? dirs) (null? (cdr dirs)))
        (fail (string-append what " works on one package; name it with -p (the members: "
                (member-names project) ")")))
      (car dirs))))
