;;; write-on-test.el --- Tests for write-on -*- lexical-binding: t; -*-

;; Run: emacs --batch -L . -l write-on-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'write-on)

(defmacro write-on-test--with-doc (text &rest body)
  (declare (indent 1))
  `(let* ((dir (make-temp-file "wo" t))
          (file (expand-file-name "essay.md" dir))
          (write-on-directory (expand-file-name "state/" dir)))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,text))
           ,@body)
       (dolist (b (buffer-list))
         (when (string-prefix-p "*overflow: essay.md" (buffer-name b))
           (kill-buffer b)))
       (when-let ((b (get-file-buffer file)))
         (with-current-buffer b (set-buffer-modified-p nil))
         (kill-buffer b))
       (delete-directory dir t))))

(ert-deftest write-on-roundtrip ()
  "Ghost + stash survive save, kill, and reopen."
  (write-on-test--with-doc "One fish. Two fish. Red fish. Blue fish."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      ;; Ghost "Two fish."
      (goto-char (point-min)) (search-forward "Two")
      (write-on-ghost-toggle)
      (should (equal (mapcar (lambda (o) (buffer-substring (overlay-start o) (overlay-end o)))
                             (write-on--ghosts))
                     '("Two fish.")))
      ;; Typing at the ghost's edges stays outside it.
      (goto-char (overlay-start (car (write-on--ghosts)))) (insert "X")
      (should (string= (buffer-substring (overlay-start (car (write-on--ghosts)))
                                         (overlay-end (car (write-on--ghosts))))
                       "Two fish."))
      (delete-char -1)
      ;; Stash "Red fish."
      (goto-char (point-min)) (search-forward "Red")
      (write-on-stash)
      (should-not (string-match-p "Red" (buffer-string)))
      (should (string= (write-on--overflow-text) "Red fish.\n\n"))
      (save-buffer)
      (kill-buffer))
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (should (equal (mapcar (lambda (o) (buffer-substring (overlay-start o) (overlay-end o)))
                             (write-on--ghosts))
                     '("Two fish.")))
      (should (string= (write-on--overflow-text) "Red fish.\n\n"))
      ;; Use it back: move from Overflow into the page.
      (let ((src (current-buffer)))
        (goto-char (point-max)) (insert " ")
        (with-current-buffer (write-on--overflow-buffer)
          (goto-char (point-min))
          (write-on-overflow-use))
        (with-current-buffer src
          (should (string-suffix-p "Red fish." (buffer-string))))
        (should (string= (write-on--overflow-text) "\n"))))))

(ert-deftest write-on-ghost-untoggle ()
  (write-on-test--with-doc "Alpha beta. Gamma delta."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (goto-char (point-min))
      (write-on-ghost-toggle)
      (should (write-on--ghosts))
      (write-on-ghost-toggle)
      (should-not (write-on--ghosts)))))

(ert-deftest write-on-broken-state-not-overwritten ()
  (write-on-test--with-doc "Text."
    (let ((state (write-on--state-file file)))
      (make-directory (file-name-directory state) t)
      (with-temp-file state (insert "(:ghosts ((1 2"))
      (with-current-buffer (find-file-noselect file)
        (write-on-mode 1)
        (should write-on--state-broken)
        (insert "x") (save-buffer)
        (should (string= (with-temp-buffer (insert-file-contents state) (buffer-string))
                         "(:ghosts ((1 2"))))))

(ert-deftest write-on-state-location ()
  (write-on-test--with-doc "Keep this. Drop this."
    ;; Saved under write-on-directory, named after the full path; nothing
    ;; is written next to the document.
    (let ((state (write-on--state-file file)))
      (should (string-prefix-p write-on-directory state))
      (should (string-suffix-p "!essay.md.eld" state))
      (should-not (string-match-p "/" (file-name-nondirectory state)))
      (with-current-buffer (find-file-noselect file)
        (write-on-mode 1)
        (goto-char (point-min)) (search-forward "Drop") (write-on-ghost-toggle)
        (save-buffer))
      (should (file-exists-p state))
      (should-not (directory-files dir nil "\\.wo\\'")))))


(defmacro write-on-test--picking (choice &rest body)
  "Run BODY with `completing-read' returning CHOICE (or quitting if :quit)."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'completing-read)
              (lambda (&rest _) (if (eq ,choice :quit) (signal 'quit nil) ,choice))))
     ,@body))

(defun write-on-test--alts ()
  (mapcar (lambda (o) (list (write-on--text o) (overlay-get o 'write-on-kind)
                            (overlay-get o 'write-on-alts)))
          (write-on--alts)))

(ert-deftest write-on-alt-word ()
  "Add, cycle, fold in edits, persist."
  (write-on-test--with-doc "Much of the tension in design.\n\nSecond paragraph here."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (goto-char (point-min)) (search-forward "tens")
      (write-on-test--picking "pressure" (write-on-alt nil))
      (should (string-match-p "the pressure in" (buffer-string)))
      (should (equal (write-on-test--alts)
                     '(("pressure" word ("tension" "pressure")))))
      (write-on-alt-next 1)
      (should (string-match-p "the tension in" (buffer-string)))
      (write-on-alt-prev 1)
      (should (string-match-p "the pressure in" (buffer-string)))
      ;; Edit in place: the edited text joins the variants.
      (goto-char (point-min)) (search-forward "pre") (insert "X")
      (write-on-alt-next 1)
      (should (equal (overlay-get (car (write-on--alts)) 'write-on-alts)
                     '("tension" "pressure" "preXssure")))
      (should (string-match-p "the tension in" (buffer-string)))
      (save-buffer) (kill-buffer))
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (should (equal (write-on-test--alts)
                     '(("tension" word ("tension" "pressure" "preXssure"))))))))

(ert-deftest write-on-alt-sentence-paragraph ()
  (write-on-test--with-doc "One fish. Two fish.\n\nRed fish."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (goto-char (point-min)) (search-forward "Two")
      (write-on-test--picking "Three fish." (write-on-alt '(4)))
      (should (equal (write-on-test--alts)
                     '(("Three fish." sentence ("Two fish." "Three fish.")))))
      ;; Paragraph: choosing the current text just records it.
      (goto-char (point-max))
      (write-on-test--picking "Red fish." (write-on-alt '(16)))
      (let ((o (car (write-on--alts-at))))
        (should (eq (overlay-get o 'write-on-kind) 'paragraph))
        (should (overlay-get o 'line-prefix))
        (should (equal (overlay-get o 'write-on-alts) '("Red fish.")))))))

(ert-deftest write-on-alt-quit-cleans-up ()
  (write-on-test--with-doc "Alpha beta."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (goto-char (point-min))
      (should (eq 'quit (condition-case nil
                            (write-on-test--picking :quit (write-on-alt nil))
                          (quit 'quit))))
      (should-not (write-on--alts)))))

(defmacro write-on-test--replying (reply &rest body)
  "Run BODY with `gptel-request' answering REPLY at once."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'gptel-request)
              (lambda (_prompt &rest args)
                (funcall (plist-get args :callback) ,reply nil))))
     ,@body))

(ert-deftest write-on-json-parsing ()
  (should (equal (write-on--json "Sure!\n```json\n[\"a\", \"b\"]\n```") '("a" "b")))
  (should (equal (write-on--json "[{\"text\": \"x\", \"note\": \"y\"}]")
                 '(((text . "x") (note . "y")))))
  (should-error (write-on--json "no json")))

(ert-deftest write-on-alt-ai-adds-and-picks ()
  (write-on-test--with-doc "Much of the tension in design."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (goto-char (point-min)) (search-forward "tens")
      ;; Point isn't in a displayed window in batch, so no picker: just added.
      (write-on-test--replying "[\"pressure\", \"struggle\", \"tension\"]"
        (write-on-alt-ai nil))
      (should (equal (overlay-get (car (write-on--alts)) 'write-on-alts)
                     '("tension" "pressure" "struggle")))
      (should (string-match-p "the tension in" (buffer-string))))))

(ert-deftest write-on-alt-ai-failure-cleans-up ()
  (write-on-test--with-doc "Alpha beta."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (goto-char (point-min))
      (write-on-test--replying nil (write-on-alt-ai nil))
      (should-not (write-on--alts))
      (write-on-test--replying "garbage" (write-on-alt-ai nil))
      (should-not (write-on--alts)))))

(ert-deftest write-on-deletions ()
  (should (equal (write-on--deletions "comprehension, etc." "comprehension.")
                 '((13 . 18))))
  (should (equal (write-on--deletions "a very big dog" "a dog") '((2 . 10))))
  (should (equal (write-on--deletions "It all depends. When" "when") '((0 . 15))))
  (should (eq (write-on--deletions "a big dog" "a large dog") 'invalid))
  ;; Cuts never merge across paragraph breaks.
  (should (equal (write-on--deletions "one two\n\nthree four" "one four")
                 '((4 . 7) (9 . 14)))))

(defun write-on-test--lab (choice reply)
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) choice)))
    (write-on-test--replying reply (write-on-lab))))

(ert-deftest write-on-lab-trim-keep-commit ()
  (write-on-test--with-doc "Making it obvious has a cost because you have limited resources, etc. Truly."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (write-on-test--lab "Slight trim (-10%)"
                          "[\"Making it obvious has a cost. Truly.\"]")
      (should (equal (mapcar #'write-on--text (write-on--cuts))
                     '(" because you have limited resources, etc")))
      ;; Keep it, then trim again and commit.
      (goto-char (overlay-start (car (write-on--cuts)))) (forward-char 2)
      (write-on-lab-keep)
      (should-not (write-on--cuts))
      (write-on-test--lab "Slight trim (-10%)" "[\"Making it obvious has a cost because you have resources, etc. Truly.\"]")
      (write-on-test--lab "Make the cuts" nil)
      (should (string= (buffer-string)
                       "Making it obvious has a cost because you have resources, etc. Truly."))
      (should-not (write-on--lab-overlays))
      ;; A rewrite is rejected, nothing marked.
      (write-on-test--lab "Slight trim (-10%)" "[\"Totally different.\"]")
      (should-not (write-on--lab-overlays)))))

(ert-deftest write-on-lab-mark-and-fix ()
  (write-on-test--with-doc "This is relly quite good. It is, perhaps, fine."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (write-on-test--lab "Mark hedges and filler"
                          "[{\"text\": \"perhaps\", \"note\": \"hedge\"}, {\"text\": \"not there\", \"note\": \"x\"}]")
      (should (equal (mapcar #'write-on--text (write-on--lab-overlays)) '("perhaps")))
      (goto-char (point-min)) (write-on-lab-next)
      (should (looking-at "perhaps"))
      (write-on-test--lab "Fix punctuation and typos"
                          "[{\"text\": \"relly\", \"fix\": \"really\"}]")
      (should-not (write-on--lab-overlays))
      (should (string-prefix-p "This is really quite" (buffer-string)))
      (should (equal (write-on-test--alts) '(("really" word ("relly" "really"))))))))

(ert-deftest write-on-ask-streaming ()
  "Streamed chunks are joined and parsed once the stream ends."
  (let (got)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest args)
                 (let ((cb (plist-get args :callback)) (info '(:stream t)))
                   (should (plist-get args :stream))
                   (dolist (c '("[\"pres" "sure\", " "\"struggle\"]")) (funcall cb c info))
                   (funcall cb '(reasoning . "hmm") info)
                   (funcall cb t info)))))
      (write-on--ask "p" (lambda (alts) (setq got alts))))
    (should (equal got '("pressure" "struggle")))))

(ert-deftest write-on-alt-highlight-dots ()
  (write-on-test--with-doc "Much of the tension in design."
    (with-current-buffer (find-file-noselect file)
      (setq-local write-on-alt-indicator 'inline)
      (write-on-mode 1)
      (goto-char (point-min)) (search-forward "tens")
      (write-on-test--picking "pressure" (write-on-alt nil))
      (write-on-test--picking "struggle" (write-on-alt nil))
      (write-on--highlight)
      (let ((o (car (write-on--alts))))
        (should (eq (overlay-get o 'face) 'write-on-alt-active))
        (should (equal (substring-no-properties (overlay-get o 'after-string)) " ○○●"))
        (write-on-alt-next 1) (write-on--highlight)
        (should (equal (substring-no-properties (overlay-get o 'after-string)) " ●○○"))
        ;; Moving away drops the dots.
        (goto-char (point-max)) (write-on--highlight)
        (should (eq (overlay-get o 'face) 'write-on-alt))
        (should-not (overlay-get o 'after-string))))))

(ert-deftest write-on-blend ()
  (should (equal (write-on--blend "#ffffff" "#000000" 0.5) "#7f7f7f"))
  (should-not (write-on--blend "unspecified-fg" "#000000" 0.5)))

(ert-deftest write-on-echo-dots-no-inline ()
  (write-on-test--with-doc "Much of the tension in design."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (goto-char (point-min)) (search-forward "tens")
      (write-on-test--picking "pressure" (write-on-alt nil))
      (write-on--highlight)
      (should-not (overlay-get (car (write-on--alts)) 'after-string))
      (should (equal (substring-no-properties (write-on--dots (car (write-on--alts)))) "○●")))))

;;; Round 2: nesting, undo, preview, spinner, export, panel

(defun write-on-test--add (search choice &optional arg)
  "Put point after SEARCH and add/pick CHOICE as an alternative."
  (goto-char (point-min)) (search-forward search)
  (write-on-test--picking choice (write-on-alt arg)))

(ert-deftest write-on-nested-survive-swaps-and-save ()
  (write-on-test--with-doc "One red fish. Two fish."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (write-on-test--add "re" "blue")                   ; word alt inside sentence 1
      (write-on-test--add "One" "A different line." '(4)) ; sentence alt over it
      (should (string-prefix-p "A different line." (buffer-string)))
      (should (equal (mapcar #'car (write-on-test--alts)) '("A different line.")))
      (save-buffer) (kill-buffer))
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (goto-char (point-min)) (write-on-alt-next 1)      ; back to the original sentence
      (should (string-prefix-p "One blue fish." (buffer-string)))
      ;; The word alternative came back with its variants.
      (should (member '("blue" word ("red" "blue")) (write-on-test--alts))))))

(ert-deftest write-on-cycling-is-one-undo ()
  (write-on-test--with-doc "Much of the tension in design."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (buffer-enable-undo)
      (write-on-test--add "tens" "pressure")
      (write-on-test--picking "struggle" (write-on-alt nil))
      (undo-boundary)
      ;; Two cycles in a row, as the command loop would see them.
      (let ((last-command nil))
        (dotimes (_ 2)
          (write-on-alt-next 1)
          (undo-boundary)
          (setq last-command 'write-on-alt-next)))
      (should (string-match-p "the pressure in" (buffer-string)))
      (let ((last-command nil)) (undo))
      (write-on--repair)
      (should (string-match-p "the struggle in" (buffer-string)))
      ;; The span wraps the restored text again, alternatives intact.
      (should (equal (write-on-test--alts)
                     '(("struggle" word ("tension" "pressure" "struggle"))))))))

(ert-deftest write-on-pick-quit-restores-preview ()
  (write-on-test--with-doc "Much of the tension in design.\n\nOther para."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (write-on-test--add "tens" "pressure")
      (let ((o (car (write-on--alts))) dims-seen)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (write-on--swap o "tension")          ; a preview step
                     (setq dims-seen (seq-filter (lambda (d) (eq (overlay-get d 'face) 'write-on-dim))
                                                 (overlays-in (point-min) (point-max))))
                     (signal 'quit nil))))
          (condition-case nil (write-on-alt nil) (quit nil)))
        (should dims-seen)                                 ; rest of page was dimmed
        (should-not (seq-filter (lambda (d) (eq (overlay-get d 'face) 'write-on-dim))
                                (overlays-in (point-min) (point-max))))
        (should (string-match-p "the pressure in" (buffer-string)))))))

(ert-deftest write-on-spinner-stops ()
  (write-on-test--with-doc "Alpha beta."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (goto-char (point-min))
      (let ((timers (length timer-list)))
        (write-on-test--replying nil (write-on-alt-ai nil))
        (should-not write-on--busy)
        (should (= timers (length timer-list)))))))

(ert-deftest write-on-export-and-count ()
  (write-on-test--with-doc "Keep this. Drop this part. End."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (goto-char (point-min)) (search-forward "Drop")
      (write-on-ghost-toggle)
      (should (string= (write-on--clean-text) "Keep this. End."))
      (should (= (write-on--word-count) 3))
      (let ((out (expand-file-name "out.md" dir)))
        (write-on-export out)
        (should (string= (with-temp-buffer (insert-file-contents out) (buffer-string))
                         "Keep this. End."))))))

(ert-deftest write-on-panel ()
  (write-on-test--with-doc "Much of the tension in design."
    (with-current-buffer (find-file-noselect file)
      (write-on-mode 1)
      (switch-to-buffer (current-buffer))
      (write-on-test--add "tens" "pressure")
      (write-on-panel-toggle)
      (unwind-protect
          (progn
            (with-current-buffer write-on--panel-name
              (should (string-match-p "Word\n○ tension\n● pressure" (buffer-string)))
              (goto-char (point-min)) (search-forward "○ tension")
              (write-on-panel-choose))
            (should (string-match-p "the tension in" (buffer-string)))
            (with-current-buffer write-on--panel-name
              (should (string-match-p "● tension" (buffer-string)))))
        (write-on-panel-toggle)))))
