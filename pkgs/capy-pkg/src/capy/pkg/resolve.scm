;;; Dependency resolution
(library (capy pkg resolve)
  (export make-resolve-options
    resolve-options-offline?
    resolve-options-locked?
    resolve-options-update?
    resolve-dependencies
    resolution-roots
    empty-resolution
    resolution-nodes
    resolution-lookup
    resolution-node-by-id
    node-id
    node-url
    node-version
    node-rev
    node-commit
    node-tree
    node-directory
    parse-version
    version->string
    version<?
    latest-version
    lock-file-name)
  (import (rnrs)
    (rnrs hashtables)
    (only (capy) canonicalize-path-string)
    (capy path)
    (capy pkg git)
    (capy pkg manifest)
    (capy pkg ui)
    (capy pkg util))

  (define-record-type resolve-options
    (fields offline? locked? update?))

  ;; A package fetched from a repository. VERSION is a string `X.Y.Z`: the tag, or the version of
  ;; the manifest for a revision. REV is the revision as the manifest wrote it, or #f.
  (define-record-type node
    (fields id url version rev commit tree directory manifest))

  (define-record-type resolution
    (fields selected      ; id -> node
      specs               ; (repository-id . rev) -> node
      nodes))             ; the selected nodes, in order of their ids

  (define (empty-resolution)
    (make-resolution (make-hashtable equal-hash equal?) (make-hashtable equal-hash equal?) '()))

  (define lock-file-name "package.lock")

  (define (resolution-node-by-id resolution id)
    (hashtable-ref (resolution-selected resolution) id #f))

  ;; The node a git dependency of a manifest stands for.
  (define (resolution-lookup resolution dependency)
    (let ([url (dependency-git dependency)])
      (cond
        [(dependency-rev dependency)
          => (lambda (rev)
              (let ([node (hashtable-ref (resolution-specs resolution) (cons (git-repository-id url) rev) #f)])
                (and node (resolution-node-by-id resolution (node-id node)))))]
        [else
          (resolution-node-by-id resolution
            (repository-package-id url (car (parse-version (dependency-version dependency)))))])))

  ;;; Versions

  ;; "1.2.3" => (1 2 3); "1.2" => (1 2 0); "1" => (1 0 0); anything else: #f
  (define (parse-version text)
    (let loop ([i 0] [start 0] [parts '()])
      (define (part)
        (and (> i start)
          (for-all char-numeric? (string->list (substring text start i)))
          (string->number (substring text start i))))
      (cond
        [(= i (string-length text))
          (let ([p (part)])
            (and p (<= (+ 1 (length parts)) 3)
              (let pad ([v (reverse (cons p parts))])
                (if (< (length v) 3) (pad (append v '(0))) v))))]
        [(char=? (string-ref text i) #\.)
          (let ([p (part)])
            (and p (loop (+ i 1) (+ i 1) (cons p parts))))]
        [else (loop (+ i 1) start parts)])))

  (define (version->string v)
    (let loop ([v v] [out ""])
      (if (null? v) out
        (loop (cdr v) (string-append out (if (string=? out "") "" ".") (number->string (car v)))))))

  ;; A missing component counts as 0, so (1 2) and (1 2 0) are equal.
  (define (version<? a b)
    (if (and (null? a) (null? b))
      #f
      (let ([x (if (null? a) 0 (car a))]
            [y (if (null? b) 0 (car b))]
            [rest-a (if (null? a) '() (cdr a))]
            [rest-b (if (null? b) '() (cdr b))])
        (cond [(< x y) #t]
          [(> x y) #f]
          [else (version<? rest-a rest-b)]))))

  (define (version-max a b) (if (version<? a b) b a))

  ;; A tag `vX.Y.Z` as a version, or #f.
  (define (tag-version tag)
    (and (> (string-length tag) 1) (char=? (string-ref tag 0) #\v)
      (let ([text (substring tag 1 (string-length tag))])
        (let ([v (parse-version text)])
          (and v (= (length (string-split-dots text)) 3) v)))))

  (define (string-split-dots text)
    (let loop ([i 0] [start 0] [out '()])
      (cond [(= i (string-length text)) (reverse (cons (substring text start i) out))]
        [(char=? (string-ref text i) #\.) (loop (+ i 1) (+ i 1) (cons (substring text start i) out))]
        [else (loop (+ i 1) start out)])))

  (define (repository-package-id url major)
    (string->symbol (string-append (git-repository-id url) "@" (number->string major))))

  ;; The highest version tag `vX.Y.Z` of the repository (below MAJOR, when it is given), as a
  ;; string, or #f. Asks the server.
  (define (latest-version url major)
    (let loop ([tags (git-list-tags url)] [best #f])
      (cond
        [(null? tags) (and best (version->string best))]
        [else
          (let ([v (tag-version (car (car tags)))])
            (loop (cdr tags)
              (if (and v (or (not major) (= major (car v))) (or (not best) (version<? best v))) v best)))])))

  ;;; The lock

  ;; An entry is (id url version rev commit tree selected?).
  (define (entry-id e) (list-ref e 0))
  (define (entry-url e) (list-ref e 1))
  (define (entry-version e) (list-ref e 2))
  (define (entry-rev e) (list-ref e 3))
  (define (entry-commit e) (list-ref e 4))
  (define (entry-tree e) (list-ref e 5))
  (define (entry-selected? e) (list-ref e 6))

  (define (lock-error path message . irritants)
    (apply fail (string-append path ": " message) irritants))

  (define (read-lock path)
    (if (not (file-exists? path))
      '()
      (let ([datum (guard (e [(message-condition? e) (lock-error path (condition-message e))])
                     (call-with-input-file path read))])
        (unless (and (pair? datum) (eq? (car datum) 'lock) (list? datum)
                  (pair? (cdr datum)) (eqv? (cadr datum) 1))
          (lock-error path "not a package.lock of this version"))
        (map
          (lambda (form)
            (define (field key)
              (let ([f (and (list? form) (assq key (cddr form)))])
                (and f (pair? (cdr f)) (cadr f))))
            (unless (and (pair? form) (eq? (car form) 'package) (list? form) (pair? (cdr form))
                      (symbol? (cadr form)) (string? (field 'git)) (string? (field 'version))
                      (string? (field 'commit)) (string? (field 'tree-sha256)))
              (lock-error path "malformed entry" form))
            (unless (and (= (string-length (field 'commit)) 40) (hex-string? (field 'commit))
                      (= (string-length (field 'tree-sha256)) 64) (hex-string? (field 'tree-sha256)))
              (lock-error path "an entry has a commit that is not 40 hexadecimal digits or a tree hash that is not 64" form))
            (list (cadr form) (field 'git) (field 'version) (field 'rev) (field 'commit)
              (field 'tree-sha256) (and (field 'selected) #t)))
          (cddr datum)))))

  (define (write-lock! path entries)
    (write-file-atomically! path
      (lambda (port)
        (put-string port ";; This file is written by capy. Do not edit it.\n")
        (put-string port "(lock 1")
        (for-each
          (lambda (e)
            (put-string port "\n  (package ")
            (write (entry-id e) port)
            (put-string port " (git ") (write (entry-url e) port) (put-string port ")")
            (put-string port " (version ") (write (entry-version e) port) (put-string port ")")
            (when (entry-rev e)
              (put-string port " (rev ") (write (entry-rev e) port) (put-string port ")"))
            (put-string port "\n    (commit ") (write (entry-commit e) port) (put-string port ")")
            (put-string port " (tree-sha256 ") (write (entry-tree e) port) (put-string port ")")
            (put-string port " (selected ") (put-string port (if (entry-selected? e) "#t" "#f"))
            (put-string port "))"))
          entries)
        (put-string port ")\n"))))

  (define (entry<? a b)
    (let ([ia (symbol->string (entry-id a))] [ib (symbol->string (entry-id b))])
      (cond
        [(string<? ia ib) #t]
        [(string<? ib ia) #f]
        [else
          (let ([va (parse-version (entry-version a))] [vb (parse-version (entry-version b))])
            (cond [(version<? va vb) #t]
              [(version<? vb va) #f]
              [else (string<? (entry-commit a) (entry-commit b))]))])))

  (define (entry-text e)
    (string-append (symbol->string (entry-id e)) " "
      (if (entry-rev e) (string-append "rev " (entry-rev e) " (version ") "version ")
      (entry-version e) (if (entry-rev e) ")" "")))

  ;;; The search

  (define (plural n one many) (string-append (number->string n) " " (if (= n 1) one many)))

  ;; The directory of the lock and the directories of the packages that are resolved together for
  ;; the package (or workspace) in DIR: the root of its workspace and every member, or DIR alone.
  (define (resolution-roots dir)
    (let* ([own (read-workspace-file (path-join dir "package.scm"))]
           [ws (or own (find-enclosing-workspace dir))])
      (if ws
        (values (workspace-directory ws) (workspace-member-directories ws))
        (values dir (list dir)))))

  ;; Resolves the git dependencies of the package in DIR, see the top of this file, and those of
  ;; the other members of its workspace when it is in one. Reads and writes the package.lock of
  ;; the root (of the workspace, or DIR). Returns a resolution.
  (define (resolve-dependencies dir options ui)
    (let-values ([(root members) (resolution-roots dir)])
      (resolve-roots root members options ui)))

  (define (resolve-roots root members options ui)
    (let* ([lock-path (path-join root lock-file-name)]
           [old (read-lock lock-path)]
           [lookup-entries (if (resolve-options-update? options) '() old)]
           [offline? (resolve-options-offline? options)]
           [locked? (resolve-options-locked? options)]
           [visited (make-hashtable equal-hash equal?)]     ; (id version) or (id rev commit) -> node
           [visited-list '()]
           [specs (make-hashtable equal-hash equal?)]
           [seen-dirs '()]
           [required (make-hashtable equal-hash equal?)]    ; id -> the highest version asked for
           [tags (make-hashtable equal-hash equal?)]        ; url -> tags
           [trees (make-hashtable equal-hash equal?)])      ; directory -> hash

      (define (find-entry url version rev)
        (let ([repo (git-repository-id url)])
          (find (lambda (e)
                 (and (string=? (git-repository-id (entry-url e)) repo)
                   (if rev
                     (equal? (entry-rev e) rev)
                     (and (not (entry-rev e)) (string=? (entry-version e) version)))))
            lookup-entries)))

      (define (cannot-ask url what)
        (cond
          [locked?
            (fail (string-append "package.lock needs to be updated: it has no entry for " what " of " url
                   "; run `capy fetch` to update it"))]
          [else
            (fail (string-append "cannot look up " what " of " url " with --offline: it is not in package.lock; "
                   "run `capy fetch` without --offline"))]))

      (define (repository-tags url)
        (or (hashtable-ref tags url #f)
          (let ([found (git-list-tags url)])
            (hashtable-set! tags url found)
            found)))

      (define (tag-commit url tag)
        (cond
          [(assoc tag (repository-tags url)) => cdr]
          [else
            (let ([known (sort-versions
                          (filter values (map (lambda (pair) (and (tag-version (car pair)) (car pair)))
                                          (repository-tags url))))])
              (fail (string-append "the repository " url " has no tag " tag
                     (if (null? known)
                       " (it has no tags of the form vX.Y.Z)"
                       (string-append " (its version tags: " (join-strings known ", ") ")")))))]))

      (define (sort-versions tags)
        (list-sort (lambda (a b) (version<? (tag-version a) (tag-version b))) tags))

      (define (join-strings list separator)
        (let loop ([l list] [out ""])
          (if (null? l) out (loop (cdr l) (string-append out (if (string=? out "") "" separator) (car l))))))

      (define (ensure-cached! url commit hints label)
        (cond
          [(git-cached? url commit) (git-commit-directory url commit)]
          [offline?
            (fail (string-append label " of " url " (" commit ") is not in the cache; "
                   "run `capy fetch` without --offline"))]
          [else
            (ui-status! ui "Fetching" (string-append (git-repository-id url) " " label))
            (git-fetch-commit! url commit hints)]))

      ;; Reads the package in the fetched DIR and checks it against the lock.
      (define (node-at id url version rev commit dir label)
        (unless (file-exists? (path-join dir "package.scm"))
          (fail (string-append "the repository " url " at " label " has no package.scm")))
        (let* ([manifest (read-manifest (path-join dir "package.scm"))]
               [tree (or (hashtable-ref trees dir #f)
                         (let ([t (tree-hash dir)]) (hashtable-set! trees dir t) t))]
               [locked (find (lambda (e)
                              (and (string=? (git-repository-id (entry-url e)) (git-repository-id url))
                                (string=? (entry-commit e) commit)))
                         old)])
          (when (and locked (not (string=? (entry-tree locked) tree)))
            (fail (string-append "the files of " (git-repository-id url) " at " commit
                   " are not the ones package.lock recorded (tree-sha256 " (entry-tree locked)
                   " against " tree "); remove " dir " to fetch them again")))
          (make-node id url version rev commit tree dir manifest)))

      (define (add-visited! key node)
        (hashtable-set! visited key node)
        (set! visited-list (cons node visited-list)))

      (define (record-required! id v)
        (let ([other (hashtable-ref required id #f)])
          (hashtable-set! required id (if other (version-max other v) v))))

      ;; The node of a `version` dependency, and whether it was visited before.
      (define (acquire-version url text)
        (let* ([v (parse-version text)]
               [version (version->string v)]
               [id (repository-package-id url (car v))]
               [key (list id version)])
          (record-required! id v)
          (cond
            [(hashtable-ref visited key #f) => (lambda (node) (values node #f))]
            [else
              (let* ([tag (string-append "v" version)]
                     [entry (find-entry url version #f)]
                     [commit (cond
                               [entry (entry-commit entry)]
                               [(or offline? locked?) (cannot-ask url (string-append "the tag " tag))]
                               [else (tag-commit url tag)])]
                     [dir (ensure-cached! url commit (list (string-append "refs/tags/" tag)) (string-append "v" version))]
                     [node (node-at id url version #f commit dir tag)])
                (add-visited! key node)
                (values node #t))])))

      ;; The node of a `rev` dependency.
      (define (acquire-rev url rev)
        (let ([spec (cons (git-repository-id url) rev)])
          (cond
            [(hashtable-ref specs spec #f) => (lambda (node) (values node #f))]
            [else
              (let* ([entry (find-entry url #f rev)]
                     [commit (cond
                               [entry (entry-commit entry)]
                               [(or offline? locked?) (cannot-ask url (string-append "the revision " rev))]
                               [else (git-expand-revision url rev)])]
                     [dir (ensure-cached! url commit (list rev) (string-append "rev " rev))]
                     [manifest-version
                       (manifest-version (read-manifest-of dir url rev))]
                     [v (or (parse-version manifest-version)
                          (fail (string-append "the version of the package at the revision " rev " of " url
                                 " is \"" manifest-version "\", which is not a version number")))]
                     [id (repository-package-id url (car v))]
                     [key (list id 'rev commit)])
                (cond
                  [(hashtable-ref visited key #f)
                    => (lambda (node) (hashtable-set! specs spec node) (values node #f))]
                  [else
                    (let ([node (node-at id url (version->string v) rev commit dir (string-append "rev " rev))])
                      (add-visited! key node)
                      (hashtable-set! specs spec node)
                      (values node #t))]))])))

      (define (read-manifest-of dir url rev)
        (unless (file-exists? (path-join dir "package.scm"))
          (fail (string-append "the repository " url " at the revision " rev " has no package.scm")))
        (read-manifest (path-join dir "package.scm")))

      (define (visit-directory dir)
        (let ([dir (canonicalize-path-string dir)])
          (unless (member dir seen-dirs)
            (set! seen-dirs (cons dir seen-dirs))
            (visit-manifest (read-manifest (path-join dir "package.scm")) dir))))

      (define (visit-manifest manifest dir)
        (for-each
          (lambda (dependency)
            (cond
              [(dependency-path dependency)
                => (lambda (relative)
                    (let ([target (path-join dir relative)])
                      (unless (file-exists? (path-join target "package.scm"))
                        (fail (string-append "the dependency "
                               (symbol->string (dependency-name dependency))
                               " of " (symbol->string (manifest-name manifest))
                               " has no package.scm:")
                          target))
                      (visit-directory target)))]
              [else
                (let-values ([(node new?)
                              (if (dependency-rev dependency)
                                (acquire-rev (dependency-git dependency) (dependency-rev dependency))
                                (acquire-version (dependency-git dependency) (dependency-version dependency)))])
                  (when new? (visit-manifest (node-manifest node) (node-directory node))))]))
          (manifest-dependencies manifest)))

      ;; The node of each package that is used.
      (define (select)
        (let ([selected (make-hashtable equal-hash equal?)]
              [by-id (make-hashtable equal-hash equal?)]
              [ids '()])
          (for-each
            (lambda (node)
              (unless (hashtable-ref by-id (node-id node) #f) (set! ids (cons (node-id node) ids)))
              (hashtable-set! by-id (node-id node) (cons node (hashtable-ref by-id (node-id node) '()))))
            (reverse visited-list))
          (for-each
            (lambda (id)
              (let* ([nodes (reverse (hashtable-ref by-id id '()))]
                     [pinned (filter node-rev nodes)]
                     [need (hashtable-ref required id #f)])
                (cond
                  [(pair? pinned)
                    (for-each
                      (lambda (other)
                        (unless (string=? (node-commit other) (node-commit (car pinned)))
                          (fail (string-append "conflicting revisions of " (symbol->string id) ": "
                                 (node-rev (car pinned)) " (" (node-commit (car pinned)) ") and "
                                 (node-rev other) " (" (node-commit other) ")"))))
                      (cdr pinned))
                    (when (and need (version<? (parse-version (node-version (car pinned))) need))
                      (fail (string-append (symbol->string id) " is pinned to the revision " (node-rev (car pinned))
                             ", which is version " (node-version (car pinned)) ", and a package needs version "
                             (version->string need))))
                    (hashtable-set! selected id (car pinned))]
                  [else
                    (hashtable-set! selected id
                      (let loop ([nodes (cdr nodes)] [best (car nodes)])
                        (cond [(null? nodes) best]
                          [(version<? (parse-version (node-version best)) (parse-version (node-version (car nodes))))
                            (loop (cdr nodes) (car nodes))]
                          [else (loop (cdr nodes) best)])))])))
            (reverse ids))
          selected))

      (define (entries-of selected)
        (list-sort entry<?
          (map (lambda (node)
                (list (node-id node) (node-url node) (node-version node) (node-rev node) (node-commit node)
                  (node-tree node) (eq? node (hashtable-ref selected (node-id node) #f))))
            visited-list)))

      (define (difference old new)
        (cond
          [(find (lambda (e) (not (member e old))) new)
            => (lambda (e) (string-append "it has no entry for " (entry-text e)))]
          [(find (lambda (e) (not (member e new))) old)
            => (lambda (e) (string-append "it has an entry for " (entry-text e) " that is not needed"))]
          [else "its entries are in another order"]))

      (for-each visit-directory members)
      (let* ([selected (select)]
             [new (entries-of selected)])
        (unless (equal? old new)
          (cond
            [locked?
              (fail (string-append "package.lock needs to be updated (" (difference old new)
                     "); run `capy fetch` to update it"))]
            [(null? new)
              ;; The lock is committed; keep it, empty, so that the change shows in a diff and
              ;; `--locked` still has something to compare with.
              (write-lock! lock-path '())
              (ui-status! ui "Locking" (string-append "no packages in " lock-file-name))]
            [else
              (write-lock! lock-path new)
              (ui-status! ui "Locking" (string-append (plural (length new) "package" "packages") " in " lock-file-name))]))
        (let ([ids (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                     (vector->list (hashtable-keys selected)))])
          (make-resolution selected specs (map (lambda (id) (hashtable-ref selected id #f)) ids)))))))
