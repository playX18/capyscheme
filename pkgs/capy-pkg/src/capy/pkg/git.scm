;;; Git wrapper for package manager
(library (capy pkg git)
  (export capy-cache-directory
    git-cache-root
    bytevector->hex
    parse-git-url
    git-repository-id
    git-repository-directory
    git-commit-directory
    git-cached?
    git-list-tags
    git-find-ref
    git-expand-revision
    git-fetch-commit!
    hex-string?
    tree-hash)
  (import (rnrs)
    (rnrs hashtables)
    (only (capy) getenv home-directory file-exists? file-directory? directory-list
      create-directory* delete-directory* rename-file microsecond random)
    (only (capy libmap) file-hash)
    (capy crypto sha256)
    (capy path)
    (capy process)
    (capy pkg ui))

  ;;; URLs

  (define (string-index s c start)
    (let loop ([i start])
      (cond [(>= i (string-length s)) #f]
        [(char=? (string-ref s i) c) i]
        [else (loop (+ i 1))])))

  (define (string-rindex s c)
    (let loop ([i (- (string-length s) 1)])
      (cond [(< i 0) #f]
        [(char=? (string-ref s i) c) i]
        [else (loop (- i 1))])))

  (define (substring-index s sub)
    (let ([n (string-length s)] [m (string-length sub)])
      (let loop ([i 0])
        (cond [(> (+ i m) n) #f]
          [(string=? (substring s i (+ i m)) sub) i]
          [else (loop (+ i 1))]))))

  (define (string-suffix? s suffix)
    (let ([n (string-length s)] [m (string-length suffix)])
      (and (>= n m) (string=? (substring s (- n m) n) suffix))))

  (define (strip-suffix s suffix)
    (if (string-suffix? s suffix) (substring s 0 (- (string-length s) (string-length suffix))) s))

  (define (strip-trailing-slashes s)
    (let loop ([n (string-length s)])
      (if (and (> n 0) (char=? (string-ref s (- n 1)) #\/))
        (loop (- n 1))
        (substring s 0 n))))

  (define (split-slashes s)
    (let loop ([i 0] [start 0] [out '()])
      (cond
        [(= i (string-length s)) (reverse (cons (substring s start i) out))]
        [(char=? (string-ref s i) #\/) (loop (+ i 1) (+ i 1) (cons (substring s start i) out))]
        [else (loop (+ i 1) start out)])))

  (define (join-slashes parts)
    (let loop ([parts parts] [out ""])
      (if (null? parts) out (loop (cdr parts) (if (string=? out "") (car parts) (string-append out "/" (car parts)))))))

  ;; The host and the path of a repository URL, see the top of this file.
  (define (parse-git-url url)
    (let* ([url (strip-suffix (strip-trailing-slashes url) ".git")])
      (define (finish host path)
        (let ([parts (filter (lambda (part) (not (string=? part ""))) (split-slashes path))]
              [host (if (string=? host "") "localhost" host)])
          (when (null? parts) (fail "the git URL names no repository:" url))
          (when (or (member ".." parts) (member "." parts) (string-index host #\/ 0))
            (fail "the git URL is not a valid repository name:" url))
          (values host (join-slashes parts))))
      (cond
        [(substring-index url "://")
          => (lambda (i)
              (let* ([rest (substring url (+ i 3) (string-length url))]
                     [slash (string-index rest #\/ 0)]
                     [authority (if slash (substring rest 0 slash) rest)]
                     [path (if slash (substring rest (+ slash 1) (string-length rest)) "")]
                     [at (string-rindex authority #\@)]
                     [authority (if at (substring authority (+ at 1) (string-length authority)) authority)]
                     [colon (string-index authority #\: 0)])
                (finish (if colon (substring authority 0 colon) authority) path)))]
        [(path-absolute? url) (finish "localhost" url)]
        ;; user@host:path
        [(let ([colon (string-index url #\: 0)] [slash (string-index url #\/ 0)])
           (and colon (or (not slash) (< colon slash)) colon))
          => (lambda (colon)
              (let* ([authority (substring url 0 colon)]
                     [at (string-rindex authority #\@)])
                (finish (if at (substring authority (+ at 1) (string-length authority)) authority)
                  (substring url (+ colon 1) (string-length url)))))]
        [else (fail "not a git URL (https://..., ssh://..., git@host:path, file://... or an absolute path):" url)])))

  ;; `HOST/PATH`
  (define (git-repository-id url)
    (let-values ([(host path) (parse-git-url url)])
      (string-append host "/" path)))

  ;;; The cache

  ;; `$XDG_CACHE_HOME/capy`, or `~/.cache/capy`: the cache of the driver.
  (define (capy-cache-directory)
    (let ([xdg (getenv "XDG_CACHE_HOME")])
      (path-join (if (and xdg (not (string=? xdg ""))) xdg (path-join (home-directory) ".cache"))
        "capy")))

  (define (git-cache-root)
    (path-join (capy-cache-directory) "git"))

  (define (git-repository-directory url)
    (path-join (git-cache-root) (git-repository-id url)))

  (define (git-commit-directory url commit)
    (path-join (git-repository-directory url) commit))

  (define (git-cached? url commit)
    (file-exists? (git-commit-directory url commit)))

  ;;; Commands

  ;; A fetch that moves under 1000 bytes/s for 60 s fails instead of hanging (`capy fetch` has no
  ;; other timeout).
  (define git-env '(("GIT_TERMINAL_PROMPT" . "0")
                    ("GIT_HTTP_LOW_SPEED_LIMIT" . "1000")
                    ("GIT_HTTP_LOW_SPEED_TIME" . "60")))

  (define (trim s)
    (let loop ([start 0])
      (if (and (< start (string-length s)) (char-whitespace? (string-ref s start)))
        (loop (+ start 1))
        (let loop ([end (string-length s)])
          (if (and (> end start) (char-whitespace? (string-ref s (- end 1))))
            (loop (- end 1))
            (substring s start end))))))

  ;; Runs `git ARGS` in CWD (or here), returns what it prints. A failure is a driver error that
  ;; says WHAT it was doing and what git said.
  (define (git cwd args what)
    (guard (e [(process-failure? e)
                (fail (string-append "git failed to " what ":") (trim (process-failure-stderr e)))]
              [(and (message-condition? e) (not (driver-error? e)))
                (fail (string-append "cannot run git (" what "):") (condition-message e))])
      (run/string "git" args #:cwd cwd #:env git-env)))

  (define (lines text)
    (let loop ([i 0] [start 0] [out '()])
      (cond
        [(= i (string-length text))
          (reverse (if (> i start) (cons (substring text start i) out) out))]
        [(char=? (string-ref text i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring text start i) out))]
        [else (loop (+ i 1) start out)])))

  ;; `git ls-remote URL PATTERN ...` as a list of (ref . commit).
  (define (list-refs url patterns)
    (let ([text (git #f (append (list "ls-remote" url) patterns) (string-append "list the refs of " url))])
      (filter values
        (map (lambda (line)
              (let ([tab (string-index line #\tab 0)])
                (and tab (cons (substring line (+ tab 1) (string-length line)) (substring line 0 tab)))))
          (lines text)))))

  ;; The tags of the repository as a list of (name . commit), in the order git lists them. An
  ;; annotated tag is peeled to the commit it names.
  (define (git-list-tags url)
    (let ([peeled (make-hashtable string-hash string=?)] [order '()] [plain (make-hashtable string-hash string=?)])
      (for-each
        (lambda (entry)
          (let ([ref (car entry)])
            (when (and (> (string-length ref) 10) (string=? (substring ref 0 10) "refs/tags/"))
              (let* ([name (substring ref 10 (string-length ref))]
                     [is-peeled (string-suffix? name "^{}")]
                     [name (if is-peeled (strip-suffix name "^{}") name)])
                (unless (or (hashtable-ref plain name #f) (hashtable-ref peeled name #f))
                  (set! order (cons name order)))
                (hashtable-set! (if is-peeled peeled plain) name (cdr entry))))))
        (list-refs url '("refs/tags/*")))
      (map (lambda (name) (cons name (or (hashtable-ref peeled name #f) (hashtable-ref plain name #f))))
        (reverse order))))

  ;; The commit the tag, branch or HEAD NAME names, or #f.
  (define (git-find-ref url name)
    (let ([refs (list-refs url (list name (string-append "refs/tags/" name "^{}")
                                 (string-append "refs/tags/" name) (string-append "refs/heads/" name)))])
      (let loop ([wanted (list (string-append "refs/tags/" name "^{}") (string-append "refs/tags/" name)
                           (string-append "refs/heads/" name) name)])
        (cond [(null? wanted) #f]
          [(assoc (car wanted) refs) => cdr]
          [else (loop (cdr wanted))]))))

  (define (hex-string? s)
    (and (> (string-length s) 0)
      (for-all (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f))) (string->list s))))

  (define (scratch-name)
    (string-append ".tmp-" (number->string (microsecond)) "-" (number->string (random 1000000000))))

  ;; The commit (40 hexadecimal digits) that REV names in the repository: a tag, a branch, or the
  ;; beginning of a commit id. Asks the server.
  (define (git-expand-revision url rev)
    (cond
      [(git-find-ref url rev) => values]
      [(and (= (string-length rev) 40) (hex-string? rev)) rev]
      [(and (>= (string-length rev) 4) (< (string-length rev) 40) (hex-string? rev))
        ;; an abbreviation: fetch the refs into a scratch repository to expand it
        (let* ([repo (git-repository-directory url)]
               [scratch (path-join repo (scratch-name))])
          (create-directory* scratch)
          (guard (e [#t (delete-directory* scratch) (raise e)])
            (git scratch '("init" "-q" "--bare") "make a scratch repository")
            (git scratch (list "fetch" "-q" url "+refs/*:refs/remotes/origin/*")
              (string-append "fetch " url))
            (let ([full (guard (e [(driver-error? e) #f])
                          (trim (git scratch (list "rev-parse" "--verify" "-q" (string-append rev "^{commit}"))
                                  "expand a revision")))])
              (delete-directory* scratch)
              (or full (fail (string-append "the revision " rev " is not a commit of") url)))))]
      [else (fail (string-append "the revision " rev " names no tag, branch or commit of") url)]))

  ;; Fetches the commit into the cache (nothing happens when it is there) and returns its
  ;; directory. HINTS are refs to try when the server does not give out the commit by its id.
  (define (git-fetch-commit! url commit hints)
    (let* ([repo (git-repository-directory url)]
           [destination (git-commit-directory url commit)])
      (cond
        [(file-exists? destination) destination]
        [else
          (create-directory* repo)
          (let ([tmp (path-join repo (scratch-name))])
            (create-directory* tmp)
            (guard (e [#t (delete-directory* tmp) (raise e)])
              (git tmp '("init" "-q") "make a scratch repository")
              (let try ([specs (cons commit hints)] [last-error #f])
                (cond
                  [(null? specs)
                    (fail (string-append "could not fetch the commit " commit " of " url
                            (if last-error (string-append ": " last-error) "")))]
                  [else
                    (let ([problem (guard (e [(driver-error? e) (driver-error-message e)])
                                     (git tmp (list "fetch" "-q" "--depth" "1" url (car specs))
                                       (string-append "fetch " (car specs) " of " url))
                                     #f)])
                      (if problem (try (cdr specs) problem) #t))]))
              (git tmp '("-c" "advice.detachedHead=false" "checkout" "-q" "FETCH_HEAD") "check out the commit")
              (let ([head (trim (git tmp '("rev-parse" "HEAD") "read the commit"))])
                (unless (string=? head commit)
                  (fail (string-append "fetched " head " when asking " url " for") commit)))
              (delete-directory* (path-join tmp ".git"))
              ;; another process may have been faster
              ;; Accept the destination only if it holds the same tree; a failed rename for any
              ;; other reason is an error.
              (guard (e [(file-exists? destination)
                         (let ([same? (guard (e2 [#t #f])
                                        (string=? (tree-hash destination) (tree-hash tmp)))])
                           (delete-directory* tmp)
                           (unless same? (raise e)))])
                (rename-file tmp destination))))
          destination])))

  ;;; Hash of a tree

  (define (directory? path) (guard (e [#t #f]) (file-directory? path)))

  ;; The paths of the files below DIR, relative to it, sorted; `.git` is left out.
  (define (tree-files dir)
    (let walk ([prefix ""])
      (apply append
        (map (lambda (name)
              (let* ([relative (if (string=? prefix "") name (string-append prefix "/" name))]
                     [path (path-join dir relative)])
                (cond
                  [(string=? name ".git") '()]
                  [(directory? path) (walk relative)]
                  [else (list relative)])))
          (list-sort string<? (directory-list (if (string=? prefix "") dir (path-join dir prefix))))))))

  (define (bytevector->hex bv)
    (let ([digits "0123456789abcdef"])
      (let loop ([i 0] [out '()])
        (if (= i (bytevector-length bv))
          (list->string (reverse out))
          (let ([b (bytevector-u8-ref bv i)])
            (loop (+ i 1)
              (cons (string-ref digits (mod b 16)) (cons (string-ref digits (div b 16)) out))))))))

  ;; The SHA-256 of the tree in the directory DIR, as 64 hexadecimal digits.
  (define (tree-hash dir)
    (bytevector->hex
      (sha256
        (string->utf8
          (apply string-append
            (map (lambda (relative)
                  (string-append relative "\n" (or (file-hash (path-join dir relative))
                                                       (fail "cannot read a file to hash the tree:" (path-join dir relative))) "\n"))
              (tree-files dir))))))))
