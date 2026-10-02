;;; annotate-magit-test.el --- Magit review regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'annotate-magit)

(defun annotate-magit-test--git (&rest args)
  (with-temp-buffer
    (unless (zerop (apply #'process-file "git" nil t nil args))
      (error "Git failed: %s" (buffer-string)))
    (string-trim (buffer-string))))

(defmacro annotate-magit-test--repository (&rest body)
  "Run BODY in a real temporary Git repository and Magit status buffer."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "annotate-review-test-" t))
          (default-directory (file-name-as-directory directory))
          (annotate-magit-file (expand-file-name "reviews" directory))
          (annotate-file (expand-file-name "source-notes" directory))
          (magit-refresh-verbose nil)
          (magit-auto-revert-mode nil)
          (magit-git-global-arguments '("--no-pager"))
          buffer)
     (unwind-protect
         (progn
           (annotate-magit-test--git "init" "-q")
           (annotate-magit-test--git "config" "user.name" "Review Test")
           (annotate-magit-test--git "config" "user.email" "review@example.test")
           (with-temp-file "example.txt" (insert "first\nold\nlast\n"))
           (annotate-magit-test--git "add" "example.txt")
           (annotate-magit-test--git "commit" "-qm" "Initial")
           (with-temp-file "example.txt" (insert "first\nnew\nlast\n"))
           (setq buffer (magit-status-setup-buffer default-directory))
           (with-current-buffer buffer
             (annotate-magit-mode 1)
             ,@body))
       (dolist (live (buffer-list))
         (with-current-buffer live
           (when (and (string-prefix-p directory default-directory)
                      (not (eq live (get-buffer " *load*"))))
             (set-buffer-modified-p nil)
             (kill-buffer live))))
       (delete-directory directory t))))

(defun annotate-magit-test--select (text)
  "Select line containing TEXT in the current real Magit diff."
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line)
  (set-mark (line-end-position))
  (setq transient-mark-mode t mark-active t))

(ert-deftest annotate-magit-source-line-mapping ()
  (let ((hunk "@@ -10,3 +20,3 @@\n before\n-deleted\n+added\n after\n"))
    (let ((start (string-match "-deleted" hunk)))
      (should (equal (annotate-magit--line-ranges hunk start (+ start 8))
                     '((11 . 11) nil))))
    (let ((start (string-match "+added" hunk)))
      (should (equal (annotate-magit--line-ranges hunk start (+ start 6))
                     '(nil (21 . 21)))))
    (should (equal (annotate-magit--line-ranges hunk 0 (length hunk))
                   '((10 . 12) (20 . 22))))))

(ert-deftest annotate-magit-empty-side-and-no-newline-marker ()
  (let ((hunk "@@ -0,0 +1 @@\n+added\n\\ No newline at end of file\n"))
    (should (equal (annotate-magit--line-ranges hunk 0 (length hunk))
                   '(nil (1 . 1)))))
  (let ((hunk "@@ -1 +0,0 @@\n-deleted\n"))
    (should (equal (annotate-magit--line-ranges hunk 0 (length hunk))
                   '((1 . 1) nil)))))

(ert-deftest annotate-magit-persists-and-restores-real-magit-hunk ()
  (annotate-magit-test--repository
    (annotate-magit-test--select "-old")
    (let* ((text-before (buffer-string))
           (note (annotate-magit--snapshot)))
      (should (equal (plist-get note :file) "example.txt"))
      (should (equal (plist-get note :old-lines) '(2 . 2)))
      (should-not (plist-get note :new-lines))
      (annotate-magit--save-note note "Keep the old behavior.\nExplain the change.")
      (should (file-exists-p annotate-magit-file))
      (should-not (file-exists-p annotate-file))
      (should (equal (buffer-string) text-before))
      (should annotate-magit--overlays)
      (magit-refresh-buffer)
      (should annotate-magit--overlays)
      (should (string-match-p "Old lines: 2; new lines: none"
                              (annotate-magit-review-string)))
      (should (string-match-p "Status: MATCHED" (annotate-magit-review-string)))
      (let ((root default-directory))
        (kill-buffer buffer)
        (setq buffer (magit-status-setup-buffer root))
        (with-current-buffer buffer
          (annotate-magit-mode 1)
          (should annotate-magit--overlays)
          (should (string-match-p "Keep the old behavior" (annotate-magit-review-string))))))))

(ert-deftest annotate-magit-changed-hunks-are-retained-but-not-reattached ()
  (annotate-magit-test--repository
    (annotate-magit-test--select "+new")
    (let ((note (annotate-magit--snapshot)))
      (annotate-magit--save-note note "Check this replacement")
      (with-temp-file "example.txt" (insert "first\ndifferent\nlast\n"))
      (magit-refresh-buffer)
      (should-not annotate-magit--overlays)
      (let ((report (annotate-magit-review-string)))
        (should (string-match-p "UNMATCHED" report))
        (should (string-match-p "+new" report))
        (should (string-match-p "Check this replacement" report)))
      (annotate-magit-delete (plist-get note :id))
      (should-not (annotate-magit--read)))))

(ert-deftest annotate-magit-staging-does-not-misattach-unstaged-notes ()
  (annotate-magit-test--repository
    (annotate-magit-test--select "+new")
    (annotate-magit--save-note (annotate-magit--snapshot) "Review unstaged change")
    (annotate-magit-test--git "add" "example.txt")
    (magit-refresh-buffer)
    (should-not annotate-magit--overlays)
    (should (string-match-p "UNMATCHED" (annotate-magit-review-string)))))

(ert-deftest annotate-magit-whole-hunk-and-editor-roundtrip ()
  (annotate-magit-test--repository
    (goto-char (plist-get (car (annotate-magit--hunks)) :start))
    (setq mark-active nil)
    (let ((note (annotate-magit--snapshot)))
      (should (equal (plist-get note :old-lines) '(1 . 3)))
      (should (equal (plist-get note :new-lines) '(1 . 3))))
    (annotate-magit-annotate)
    (should (derived-mode-p 'annotate-magit-edit-mode))
    (insert "A multiline review\nwith a second line")
    (annotate-magit-edit-save)
    (should (eq (current-buffer) buffer))
    (should (= (length (annotate-magit--read)) 1))
    (goto-char (overlay-start (car annotate-magit--overlays)))
    (annotate-magit-annotate)
    (erase-buffer)
    (insert "Updated comment")
    (annotate-magit-edit-save)
    (should (= (length (annotate-magit--read)) 1))
    (annotate-magit-copy-review)
    (should (string-match-p "Updated comment" (current-kill 0)))
    (should (= (length (annotate-magit--read)) 1))))

(ert-deftest annotate-magit-editor-survives-origin-refresh ()
  (annotate-magit-test--repository
    (annotate-magit-test--select "+new")
    (annotate-magit-annotate)
    (let ((editor (current-buffer)))
      (insert "Based on the original snapshot")
      (with-current-buffer buffer
        (with-temp-file "example.txt" (insert "first\nchanged while reviewing\nlast\n"))
        (magit-refresh-buffer))
      (with-current-buffer editor (annotate-magit-edit-save)))
    (should-not annotate-magit--overlays)
    (should (string-match-p "UNMATCHED" (annotate-magit-review-string)))))

(ert-deftest annotate-magit-multiple-buffers-merge-saved-notes ()
  (annotate-magit-test--repository
    (annotate-magit-test--select "-old")
    (annotate-magit--save-note (annotate-magit--snapshot) "Deletion note")
    (let ((diff (magit-diff-unstaged)))
      (with-current-buffer diff
        (annotate-magit-mode 1)
        (annotate-magit-test--select "+new")
        (annotate-magit--save-note (annotate-magit--snapshot) "Addition note")
        (should (= (length (annotate-magit--read)) 2))
        (should (= (length annotate-magit--overlays) 2))))
    (should (= (length annotate-magit--overlays) 2))))

(ert-deftest annotate-magit-rejects-cross-hunk-selection ()
  (annotate-magit-test--repository
    (annotate-magit-test--select "+new")
    (set-mark (point-max))
    (should-error (annotate-magit--snapshot) :type 'user-error)))

(ert-deftest annotate-magit-corrupt-database-is-never-overwritten ()
  (let* ((directory (make-temp-file "annotate-corrupt-" t))
         (annotate-magit-file (expand-file-name "reviews" directory)))
    (unwind-protect
        (progn
          (with-temp-file annotate-magit-file (insert "broken database"))
          (should-error (annotate-magit--save-note '(:id "a") "A note") :type 'user-error)
          (with-temp-buffer
            (insert-file-contents annotate-magit-file)
            (should (equal (buffer-string) "broken database"))))
      (delete-directory directory t))))

(ert-deftest annotate-magit-disable-only-removes-display ()
  (annotate-magit-test--repository
    (annotate-magit-test--select "+new")
    (annotate-magit--save-note (annotate-magit--snapshot) "Retained note")
    (annotate-magit-mode -1)
    (should-not annotate-magit--overlays)
    (should-not (memq #'annotate-magit--refresh magit-refresh-buffer-hook))
    (should (= (length (annotate-magit--read)) 1))
    (annotate-magit-mode 1)
    (should annotate-magit--overlays)))

(ert-deftest annotate-magit-rename-retains-both-file-paths ()
  (annotate-magit-test--repository
    (annotate-magit-test--git "mv" "example.txt" "renamed.txt")
    (annotate-magit-test--git "add" "renamed.txt")
    (magit-refresh-buffer)
    (annotate-magit-test--select "+new")
    (let ((note (annotate-magit--snapshot)))
      (should (equal (plist-get note :file) "renamed.txt"))
      (should (equal (plist-get note :old-file) "example.txt"))
      (annotate-magit--save-note note "Review the rename and replacement")
      (should (string-match-p "Old file: example.txt" (annotate-magit-review-string)))
      (magit-refresh-buffer)
      (should annotate-magit--overlays))))

(ert-deftest annotate-magit-revision-buffer-roundtrip ()
  (annotate-magit-test--repository
    (annotate-magit-test--git "add" "example.txt")
    (annotate-magit-test--git "commit" "-qm" "Replace line")
    (let ((revision (magit-revision-setup-buffer "HEAD" nil nil)))
      (with-current-buffer revision
        (annotate-magit-mode 1)
        (annotate-magit-test--select "+new")
        (let ((note (annotate-magit--snapshot)))
          (should (eq (car (plist-get note :context)) 'committed))
          (annotate-magit--save-note note "Review this committed change")
          (magit-refresh-buffer)
          (should annotate-magit--overlays)
          (should (string-match-p "Status: MATCHED" (annotate-magit-review-string))))))))

(ert-deftest annotate-magit-branch-switch-keeps-notes-unmatched ()
  (annotate-magit-test--repository
    (annotate-magit-test--select "+new")
    (annotate-magit--save-note (annotate-magit--snapshot) "Original branch review")
    (annotate-magit-test--git "checkout" "-qb" "another-branch")
    (magit-refresh-buffer)
    (should-not annotate-magit--overlays)
    (should (string-match-p "UNMATCHED" (annotate-magit-review-string)))))

(ert-deftest annotate-magit-removed-file-retains-deletion-note ()
  (annotate-magit-test--repository
    (delete-file "example.txt")
    (magit-refresh-buffer)
    (annotate-magit-test--select "-old")
    (let ((note (annotate-magit--snapshot)))
      (should-not (plist-get note :new-lines))
      (annotate-magit--save-note note "Do not remove this behavior")
      (should annotate-magit--overlays)
      (should (string-match-p "new lines: none" (annotate-magit-review-string))))))

(ert-deftest annotate-magit-worktree-review-isolation ()
  (annotate-magit-test--repository
    (annotate-magit-test--select "+new")
    (annotate-magit--save-note (annotate-magit--snapshot) "Original worktree only")
    (let* ((other-root (expand-file-name "other-tree" directory))
           (database annotate-magit-file)
           other)
      (annotate-magit-test--git "worktree" "add" "-qb" "other" other-root)
      (with-temp-file (expand-file-name "example.txt" other-root)
        (insert "first\nnew\nlast\n"))
      (setq other (magit-status-setup-buffer other-root))
      (with-current-buffer other
        (setq-local annotate-magit-file database)
        (annotate-magit-mode 1)
        (should-not annotate-magit--overlays)
        (should-error (annotate-magit-review-string) :type 'user-error)))))

(ert-deftest annotate-magit-no-source-files-are-changed ()
  (annotate-magit-test--repository
    (let ((patch (annotate-magit-test--git "diff")))
      (annotate-magit-test--select "+new")
      (annotate-magit--save-note (annotate-magit--snapshot) "Review only")
      (should (equal patch (annotate-magit-test--git "diff")))
      (should-not annotate-mode)
      (should-not (memq #'annotate-save-annotations kill-buffer-hook)))))

(ert-deftest annotate-magit-ambiguous-hunks-do-not-reattach ()
  (let ((note '(:file "f" :context (unstaged) :hunk "same")))
    (should-not (annotate-magit--match note (list note note)))))

;;; annotate-magit-test.el ends here
