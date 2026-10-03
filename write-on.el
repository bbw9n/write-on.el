;;; write-on.el --- Alternative control for prose -*- lexical-binding: t; -*-

;; Copyright (C) 2026 bbw9n

;; Author: bbw9n <bbw9nio@gmail.com>
;; URL: https://github.com/bbw9n/write-on.el
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: wp
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

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

;; A minor mode for writing, inspired by Write_On:
;;
;;   Alternatives -- keep variants of a word, sentence, or paragraph.
;;   Ghost        -- dim text without deleting it.
;;   Overflow     -- a side pane that stashes text cut from the page.
;;   AI / Lab     -- model alternatives, review marks, fixes, and trims (gptel).
;;   Panel        -- a side panel listing the alternatives at point.
;;
;; Each document's state is saved in `write-on-directory' (under your
;; Emacs config, like other packages' data), so the document stays clean.
;; See PLAN.md.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'pulse)
(require 'thingatpt)

(defgroup write-on nil
  "Alternative control for prose."
  :group 'wp)

(defface write-on-ghost '((t :inherit shadow))
  "Face for ghosted (dimmed) text.")

(defcustom write-on-overflow-width 60
  "Width of the Overflow side window."
  :type 'integer)

(defvar-local write-on--loaded nil
  "Non-nil once the saved state is loaded, so re-enabling doesn't load it twice.")
(put 'write-on--loaded 'permanent-local t)

(defvar-local write-on--state-broken nil
  "Non-nil when the saved state failed to load; we then refuse to overwrite it.")

(defvar-local write-on--source nil
  "In an Overflow or panel buffer, the document buffer it serves.")

;;; Helpers

(defun write-on--bounds ()
  "Active region, else the sentence at point."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (or (bounds-of-thing-at-point 'sentence)
        (user-error "No region or sentence at point"))))

(defcustom write-on-directory
  (if (boundp 'doom-state-dir)
      (expand-file-name "write-on/" doom-state-dir)
    (locate-user-emacs-file "write-on/"))
  "Where write-on keeps each document's alternatives, ghosts, and Overflow.
This is your writing, not a cache: keep it out of directories that get
wiped.  In Doom it defaults to `doom-state-dir' (user-saved data), never
`doom-cache-dir'."
  :type 'directory)

(defun write-on--state-file (&optional file)
  "Where FILE's state is saved: its full path with `/' as `!', like backups."
  (let* ((file (file-truename (or file buffer-file-name
                                  (user-error "Buffer has no file"))))
         (name (concat (subst-char-in-string ?/ ?! (string-replace "!" "!!" file))
                       ".eld")))
    (expand-file-name (if (> (length name) 200) (concat (md5 name) ".eld") name)
                      write-on-directory)))

(defun write-on--overlays (prop &optional beg end)
  "Overlays with PROP between BEG and END (default: whole buffer)."
  (seq-filter (lambda (o) (overlay-get o prop))
              (overlays-in (or beg (point-min)) (or end (point-max)))))

(defun write-on--text (o)
  "The text overlay O covers."
  (buffer-substring-no-properties (overlay-start o) (overlay-end o)))

;;; Ghost

(defun write-on--ghosts (&optional beg end)
  "Ghost overlays between BEG and END."
  (write-on--overlays 'write-on-ghost beg end))

(defun write-on--ghost (beg end)
  "Dim BEG..END.  Typing at either edge stays outside the ghost."
  (let ((o (make-overlay beg end nil t nil)))
    (overlay-put o 'write-on-ghost t)
    (overlay-put o 'face 'write-on-ghost)
    (overlay-put o 'evaporate t)
    o))

(defun write-on-ghost-toggle ()
  "Dim the region (or sentence at point), or un-dim it if already ghosted."
  (interactive)
  (pcase-let* ((`(,beg . ,end) (write-on--bounds))
               (ghosts (write-on--ghosts beg end)))
    (if ghosts
        (mapc #'delete-overlay ghosts)
      (write-on--ghost beg end))
    (set-buffer-modified-p t)
    (deactivate-mark)))

;;; Alternatives
;;
;; A span (word, sentence, or paragraph) carries a list of variants in an
;; overlay.  The buffer shows one of them; swapping never loses the others.
;; Editing a span in place adds the edited text as a new variant.

(defface write-on-alt '((t :underline (:position 3)))
  "Face for words and sentences that have alternatives.
Colored from the theme when `write-on-theme-colors' is non-nil.")

(defface write-on-alt-active '((t :inherit write-on-alt :weight bold))
  "Face for the span with alternatives at point.")

(defface write-on-alt-dots '((t :inherit shadow))
  "Face for the variant dots of the span at point.")

(defcustom write-on-theme-colors t
  "Non-nil: tint the alternative faces from the theme's accent color.
Recomputed when a theme is enabled.  Set nil to style the faces yourself."
  :type 'boolean)

(defcustom write-on-accent-face 'link
  "Face whose foreground is the theme's accent color."
  :type 'face)

(defcustom write-on-alt-indicator 'echo
  "Where to show the variant dots of the span at point.
`echo' in the echo area, `inline' after the span, nil nowhere."
  :type '(choice (const echo) (const inline) (const nil)))

(defun write-on--blend (fg bg alpha)
  "FG over BG at ALPHA, as a hex color; nil if either isn't a real color."
  (let ((a (color-name-to-rgb fg)) (b (color-name-to-rgb bg)))
    (when (and a b)
      (apply #'color-rgb-to-hex
             (append (cl-mapcar (lambda (x y) (+ (* alpha x) (* (- 1 alpha) y))) a b)
                     '(2))))))

(defcustom write-on-tint 0.6
  "How far the span background moves from the default toward the selection.
0 is the default background, 1 the `region' background.  The span at
point goes further, halfway to 1."
  :type 'number)

(defun write-on--theme-faces (&rest _)
  "Derive the alternative faces from the theme's background, selection, and accent."
  (when write-on-theme-colors
    (let* ((bg (face-background 'default nil t))
           (sel (face-background 'region nil t))
           (acc (face-foreground write-on-accent-face nil t))
           (tint (write-on--blend sel bg write-on-tint))
           (line (write-on--blend acc bg 0.45)))
      (when (and tint line)
        (set-face-attribute 'write-on-alt nil
                            :inherit 'unspecified
                            :background tint
                            :underline `(:color ,line :position 3))
        (set-face-attribute 'write-on-alt-active nil
                            :inherit 'unspecified
                            :weight 'bold
                            :background (write-on--blend sel bg (/ (+ 1 write-on-tint) 2))
                            :underline `(:color ,acc :position 3))
        (set-face-attribute 'write-on-alt-dots nil :foreground acc)))))

(defface write-on-alt-bar '((t :inherit shadow))
  "Face for the bar beside paragraphs that have alternatives.")

(defvar-local write-on--active nil
  "The alternative overlay currently highlighted at point.")

(defun write-on--dots (o)
  "Dots for O's variants, the shown one filled."
  (let* ((vs (overlay-get o 'write-on-alts))
         (i (seq-position vs (write-on--text o))))
    (propertize (mapconcat (lambda (k) (if (eql k i) "●" "○"))
                           (number-sequence 0 (1- (length vs))) "")
                'face 'write-on-alt-dots)))

(defun write-on--render (o active)
  "Style span O as ACTIVE (at point) or not."
  (unless (eq (overlay-get o 'write-on-kind) 'paragraph)
    (overlay-put o 'face (if active 'write-on-alt-active 'write-on-alt)))
  (unless (overlay-get o 'write-on-busy)   ; the spinner owns it meanwhile
    (overlay-put o 'after-string
                 (and active (eq write-on-alt-indicator 'inline)
                      (propertize (concat " " (write-on--dots o))
                                  'display '((height 0.6) (raise -0.3)))))))

(defun write-on--echo-dots (o)
  "Show O's variant dots in the echo area, if configured."
  (when (eq write-on-alt-indicator 'echo)
    (let ((message-log-max nil))
      (message "%s %s" (overlay-get o 'write-on-kind) (write-on--dots o)))))

(defun write-on--highlight ()
  "Highlight the span at point and show its dots (`post-command-hook')."
  (write-on--repair)
  (write-on--panel-refresh)
  (let ((o (car (write-on--alts-at))))
    (when (and write-on--active (not (eq o write-on--active))
               (overlay-buffer write-on--active))
      (write-on--render write-on--active nil))
    (when o
      (write-on--render o t)
      ;; Echo only on entering a span, so other messages aren't clobbered.
      (unless (eq o write-on--active) (write-on--echo-dots o)))
    (setq write-on--active o)))

(defun write-on--alts (&optional beg end)
  "Alternative overlays between BEG and END."
  (write-on--overlays 'write-on-alts beg end))

(defun write-on--alts-at (&optional pos)
  "Alternative overlays touching POS, innermost first."
  (let ((pos (or pos (point))))
    (sort (seq-filter (lambda (o) (<= (overlay-start o) pos (overlay-end o)))
                      (write-on--alts (max (point-min) (1- pos))
                                      (min (point-max) (1+ pos))))
          (lambda (a b) (< (- (overlay-end a) (overlay-start a))
                           (- (overlay-end b) (overlay-start b)))))))

(defun write-on--alt-make (beg end kind variants)
  "Make a span of KIND over BEG..END holding VARIANTS."
  (let ((o (make-overlay beg end nil t nil)))
    (overlay-put o 'write-on-alts variants)
    (overlay-put o 'write-on-kind kind)
    (if (eq kind 'paragraph)
        (let ((bar (propertize "▎" 'face 'write-on-alt-bar)))
          (overlay-put o 'line-prefix bar)
          (overlay-put o 'wrap-prefix bar))
      (overlay-put o 'face 'write-on-alt))
    o))

(defun write-on--trim (bounds)
  "BOUNDS (BEG . END) shrunk past surrounding whitespace."
  (save-excursion
    (goto-char (cdr bounds)) (skip-chars-backward " \t\n")
    (let ((end (point)))
      (goto-char (car bounds)) (skip-chars-forward " \t\n")
      (cons (min (point) end) end))))

(defun write-on--span (kind)
  "Bounds of the KIND (word, sentence, paragraph) at point, trimmed."
  (write-on--trim (or (bounds-of-thing-at-point kind)
                      (user-error "No %s at point" kind))))

(defun write-on--infer-kind (beg end)
  "Guess whether BEG..END is a word, sentence, or paragraph."
  (cond ((not (string-match-p "[ \t\n]" (buffer-substring beg end))) 'word)
        ((or (string-match-p "\n" (buffer-substring beg end))
             (equal (cons beg end) (save-excursion (goto-char beg)
                                                   (write-on--span 'paragraph))))
         'paragraph)
        (t 'sentence)))

(defun write-on--alt-target (arg)
  "The alternative overlay to act on, and whether it was just created.
Region > existing span at point > new span: word, or sentence when ARG
is \\[universal-argument], or paragraph with two."
  (let* ((kind (pcase arg ('nil nil) ('(4) 'sentence) (_ 'paragraph)))
         (bounds (cond ((use-region-p) (write-on--trim (cons (region-beginning)
                                                             (region-end))))
                       (kind (write-on--span kind))))
         (existing (seq-find (lambda (o)
                               (cond (bounds (equal bounds (cons (overlay-start o)
                                                                 (overlay-end o))))
                                     (kind (eq kind (overlay-get o 'write-on-kind)))
                                     (t t)))
                             (write-on--alts-at))))
    (if existing
        (list existing nil)
      (pcase-let ((`(,b . ,e) (or bounds (write-on--span 'word))))
        (list (write-on--alt-make b e (if bounds (write-on--infer-kind b e) 'word)
                                  (list (buffer-substring-no-properties b e)))
              t)))))

(defun write-on--shown-variants (o)
  "O's variants plus its current text, without recording the text."
  (let ((cur (write-on--text o))
        (vs (overlay-get o 'write-on-alts)))
    (if (or (member cur vs) (string-empty-p cur))
        vs
      (append vs (list cur)))))

(defun write-on--variants (o)
  "O's variants, with its current text folded in so edits aren't lost.
Only call this when acting on O, not while the user may be mid-edit."
  (overlay-put o 'write-on-alts (write-on--shown-variants o)))

;; Swapping a span replaces its text, which would destroy the alternatives
;; and ghosts inside it.  So they're captured into the span's
;; `write-on-nested' alist, keyed by the variant they belong to, and
;; brought back when that variant returns.

(defun write-on--capture (o)
  "Remove the alternatives and ghosts inside O; return them relative to its start."
  (let ((b (overlay-start o)) (e (overlay-end o)) out)
    (dolist (in (overlays-in b e) out)
      (when (and (not (eq in o)) (<= b (overlay-start in)) (<= (overlay-end in) e)
                 (or (overlay-get in 'write-on-alts) (overlay-get in 'write-on-ghost)))
        (push (list (- (overlay-start in) b) (- (overlay-end in) b)
                    (overlay-get in 'write-on-kind)
                    (and (overlay-get in 'write-on-alts) (write-on--variants in))
                    (overlay-get in 'write-on-nested))
              out)
        (delete-overlay in)))))

(defun write-on--unpack (entries base)
  "Recreate captured ENTRIES at BASE."
  (pcase-dolist (`(,rb ,re ,kind ,vs ,nested) entries)
    (if kind
        (overlay-put (write-on--alt-make (+ base rb) (+ base re) kind vs)
                     'write-on-nested nested)
      (write-on--ghost (+ base rb) (+ base re)))))

(defun write-on--settle (o)
  "Bring back whatever was nested inside O's current variant."
  (let* ((stash (overlay-get o 'write-on-nested))
         (entry (assoc (write-on--text o) stash)))
    (when entry
      (overlay-put o 'write-on-nested (delq entry stash))
      (write-on--unpack (cdr entry) (overlay-start o)))))

(defun write-on--swap (o new)
  "Replace O's text with NEW, keeping what's nested inside each variant."
  (let ((b (overlay-start o))
        (old (write-on--text o)))
    (unless (string= new old)
      (let ((inner (write-on--capture o))
            (stash (assoc-delete-all old (overlay-get o 'write-on-nested))))
        (overlay-put o 'write-on-nested (if inner (cons (cons old inner) stash) stash)))
      (save-excursion
        (delete-region b (overlay-end o))
        (goto-char b)
        (insert new))
      (move-overlay o b (+ b (length new)))
      (write-on--settle o))))

(defun write-on--repair ()
  "Re-wrap spans that undo emptied.
Undo deletes a swapped-in variant and reinserts the old text outside the
overlay; find which variant now sits there and cover it again."
  (dolist (o (write-on--alts))
    (when (= (overlay-start o) (overlay-end o))
      (let ((p (overlay-start o)))
        (seq-some
         (lambda (v)
           (let ((n (length v)))
             (cond ((and (<= (+ p n) (point-max))
                         (string= v (buffer-substring-no-properties p (+ p n))))
                    (move-overlay o p (+ p n)))
                   ((and (>= (- p n) (point-min))
                         (string= v (buffer-substring-no-properties (- p n) p)))
                    (move-overlay o (- p n) p)))))
         ;; Longest first, so "pressure" wins over a variant "press".
         (sort (copy-sequence (overlay-get o 'write-on-alts))
               (lambda (x y) (> (length x) (length y)))))
        (unless (= (overlay-start o) (overlay-end o))
          (write-on--settle o))))))

(defun write-on-alt (arg)
  "Pick or add an alternative for the span at point.
Choose an existing variant to swap it in, or type a new one to add it.
Choosing the current text just records the span, so you can rewrite it in
place and keep the original.  The span is the region, an existing span at
point, or the word at point; with prefix ARG the sentence
\(\\[universal-argument]) or paragraph (two)."
  (interactive "P")
  (apply #'write-on--pick (write-on--alt-target arg)))

(defcustom write-on-dim-others t
  "Non-nil: dim the rest of the page while picking an alternative."
  :type 'boolean)

(defface write-on-dim '((t :inherit shadow))
  "Face for the page around the paragraph being worked on.")

(defun write-on--dim (o)
  "Dim everything outside O's paragraph; return the dimming overlays."
  (when write-on-dim-others
    (pcase-let ((`(,b . ,e) (save-excursion (goto-char (overlay-start o))
                                            (write-on--span 'paragraph))))
      ;; Front-advance t, rear-advance nil: text inserted at either edge
      ;; stays outside, so a previewed variant (swapped in right where the
      ;; dimmed text after it begins) isn't dimmed with it.
      (mapcar (lambda (r)
                (let ((d (make-overlay (car r) (cdr r) nil t nil)))
                  (overlay-put d 'face 'write-on-dim)
                  d))
              (list (cons (point-min) (min b (overlay-start o)))
                    (cons (max e (overlay-end o)) (point-max)))))))

(defun write-on--flash (o)
  "Briefly flash O's text, then fade (like Write_On's swap)."
  (when (overlay-buffer o)
    (pulse-momentary-highlight-region (overlay-start o) (overlay-end o) 'region)))

(declare-function vertico--candidate "ext:vertico")
(declare-function helm-get-selection "ext:helm-core")
(defvar helm-alive-p)
(defvar vertico--input)

(defun write-on--candidate ()
  "The completion candidate the user is on, else the input.
Knows Vertico, Helm, and Icomplete/Fido."
  ;; Ask the UI that is actually running this minibuffer: configs may enable
  ;; several (Doom can have Vertico on while Helm handles `completing-read').
  (cond ((and (bound-and-true-p helm-alive-p) (fboundp 'helm-get-selection))
         (helm-get-selection))
        ((and (bound-and-true-p vertico--input) (fboundp 'vertico--candidate))
         (vertico--candidate))
        ((bound-and-true-p icomplete-mode)
         (car (completion-all-sorted-completions)))
        (t (minibuffer-contents-no-properties))))

(defun write-on--pick (o fresh)
  "Let the user choose O's variant, previewing each in place.
Quitting restores the original; drop O on quit if FRESH.  The whole
pick is one undo step."
  (let* ((src (current-buffer))
         (orig (write-on--text o))
         (vs (write-on--variants o))
         (group (prepare-change-group))
         (dims (write-on--dim o))
         ;; Preview whatever the completion UI has highlighted.  Polled, not
         ;; hooked: Vertico, Helm, Fido... each signal moves differently.
         (preview (run-with-timer
                   0.1 0.1
                   (lambda ()
                     (when-let* ((mini (active-minibuffer-window))
                                 (c (with-current-buffer (window-buffer mini)
                                      (write-on--candidate))))
                       (when (and (member c vs) (overlay-buffer o))
                         (with-current-buffer src (write-on--swap o c)))))))
         (done nil))
    (activate-change-group group)
    (unwind-protect
        (let ((pick (completing-read
                     (format "Alternative %s (%d): "
                             (overlay-get o 'write-on-kind) (length vs))
                     vs nil nil nil nil orig)))
          (unless (member pick vs)
            (overlay-put o 'write-on-alts (append vs (list pick))))
          (write-on--swap o pick)
          (write-on--flash o)
          (set-buffer-modified-p t)
          (setq done t))
      (cancel-timer preview)
      (when (and (not done) (overlay-buffer o))
        (write-on--swap o orig))
      (mapc #'delete-overlay dims)
      (undo-amalgamate-change-group group)
      (accept-change-group group)
      (when (and fresh (not done))
        (delete-overlay o))
      (deactivate-mark)
      (write-on--panel-refresh))))

(defvar-keymap write-on-cycle-map
  :doc "Active right after cycling: keep cycling with single keys."
  "]" #'write-on-alt-next
  "[" #'write-on-alt-prev)

(defvar-local write-on--cycle-group nil
  "Change group joining consecutive cycling into one undo step.")

(defun write-on-alt-next (n)
  "Swap in the Nth next alternative of the innermost span at point.
Then ] and [ keep cycling; a run of cycling undoes in one step."
  (interactive "p")
  (let* ((o (or (car (write-on--alts-at)) (user-error "No alternatives here")))
         (vs (write-on--variants o))
         (i (mod (+ (or (seq-position vs (write-on--text o)) 0) n) (length vs))))
    (unless (and write-on--cycle-group
                 (memq last-command '(write-on-alt-next write-on-alt-prev)))
      (setq write-on--cycle-group (prepare-change-group))
      (activate-change-group write-on--cycle-group))
    (write-on--swap o (nth i vs))
    (undo-amalgamate-change-group write-on--cycle-group)
    (write-on--flash o)
    (write-on--panel-refresh)
    (if (eq write-on-alt-indicator 'echo)
        (write-on--echo-dots o)
      (message "%s %d/%d" (overlay-get o 'write-on-kind) (1+ i) (length vs)))
    (set-transient-map write-on-cycle-map)))

(defun write-on-alt-prev (n)
  "Swap in the Nth previous alternative of the innermost span at point."
  (interactive "p")
  (write-on-alt-next (- n)))

(defun write-on-alt-remove ()
  "Keep the current text of the innermost span at point, forget its alternatives."
  (interactive)
  (delete-overlay (or (car (write-on--alts-at)) (user-error "No alternatives here")))
  (set-buffer-modified-p t))

;;; AI
;;
;; Any gptel backend works.  Replies are JSON parsed from the text, so no
;; structured-output support is needed.

(declare-function gptel-request "gptel-request")

(defcustom write-on-ai-count 6
  "How many alternatives to ask the model for."
  :type 'integer)

(defvar write-on-ai-system
  "You are a sharp, tasteful prose editor. Reply with JSON only, no commentary."
  "System message for write-on's model requests.")

(defun write-on--json (s)
  "Parse the JSON array in model reply S."
  (let ((b (string-search "[" s))
        (e (let ((i (cl-position ?\] s :from-end t))) (and i (1+ i)))))
    (unless (and b e) (error "No JSON array in reply"))
    (json-parse-string (substring s b e) :object-type 'alist :array-type 'list)))

(defvar-local write-on--busy nil
  "Spinner frame shown in the mode line while a request runs.")

(defconst write-on--frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])

(defun write-on--spin (&optional o)
  "Animate a spinner in the mode line, and at overlay O if given.
For a word or sentence the spinner follows the span; for a paragraph it
leads it, next to the cursor rather than at the far end of the paragraph.
Return a function that stops it."
  (let* ((buf (current-buffer))
         (i 0)
         (lead (and o (eq (overlay-get o 'write-on-kind) 'paragraph)))
         (show (lambda (frame)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (setq write-on--busy frame)
                     (when (and o (overlay-buffer o))
                       (overlay-put o 'write-on-busy frame)
                       (overlay-put o (if lead 'before-string 'after-string)
                                    (and frame
                                         (propertize (if lead (concat frame " ") (concat " " frame))
                                                     'face 'write-on-alt-dots))))
                     (force-mode-line-update)))))
         (timer (run-at-time 0 0.1 (lambda ()
                                     (setq i (mod (1+ i) (length write-on--frames)))
                                     (funcall show (aref write-on--frames i))))))
    (lambda () (cancel-timer timer) (funcall show nil))))

(defun write-on--ask (prompt on-reply &optional on-fail o)
  "Send PROMPT; call ON-REPLY with the parsed JSON array, or ON-FAIL.
A spinner runs meanwhile, also after overlay O if given."
  ;; Load all of gptel, not just the autoloaded gptel-request, so the
  ;; user's deferred gptel config (backend, model, key) has run.
  (require 'gptel nil t)
  (unless (fboundp 'gptel-request)
    (user-error "write-on: AI features need gptel"))
  (let* ((buf (current-buffer))
         (chunks nil)
         (stop (write-on--spin o))
         (on-fail (lambda () (funcall stop) (when on-fail (funcall on-fail)))))
    (cl-flet ((finish (reply)
                (funcall stop)
                (with-current-buffer buf
                  (condition-case err
                      (funcall on-reply (write-on--json reply))
                    (error
                     (message "write-on: unusable reply: %s" (error-message-string err))
                     (funcall on-fail))))))
      (message "write-on: asking...")
      ;; Stream: some endpoints reject non-streaming requests.  When the
      ;; backend can't stream, gptel ignores this and replies in one string.
      ;; If the request can't even start, stop the spinner and clean up.
      (condition-case err
          (gptel-request prompt
            :system write-on-ai-system
            :stream t
            :callback
            (lambda (resp info)
              (cond
               ((and (stringp resp) (plist-get info :stream)) (push resp chunks))
               ((stringp resp) (finish resp))
               ((eq resp t) (finish (apply #'concat (nreverse chunks))))
               ((memq resp '(nil abort))
                (let ((err (plist-get info :error)))
                  (message "write-on: request failed: %s %s" (plist-get info :status)
                           (or (and (listp err) (alist-get 'message err)) err "")))
                (with-current-buffer buf (funcall on-fail))))))
        (error (funcall on-fail)
               (signal (car err) (cdr err)))))))

(defun write-on--paragraph-at (pos)
  "Text of the paragraph at POS."
  (save-excursion
    (goto-char pos)
    (pcase-let ((`(,b . ,e) (write-on--span 'paragraph)))
      (buffer-substring-no-properties b e))))

(defun write-on-alt-ai (arg)
  "Ask the model for alternatives to the span at point, then pick one.
The span is chosen from prefix ARG as in `write-on-alt'."
  (interactive "P")
  (deactivate-mark)
  (apply #'write-on--ai (write-on--alt-target arg)))

(defun write-on--ai (o fresh)
  "Ask the model for alternatives to O.  Drop O on failure if FRESH."
  (let ((kind (overlay-get o 'write-on-kind))
        (text (write-on--text o)))
    (write-on--ask
     (format "Paragraph:\n%s\n\nSuggest %d alternatives for this %s: %S
Each must drop into the paragraph in its place, keep the meaning, and match the voice.
Reply with a JSON array of strings."
             (write-on--paragraph-at (overlay-start o)) write-on-ai-count kind text)
     (lambda (alts)
       (if (not (overlay-buffer o))
           (message "write-on: span is gone")
         (overlay-put o 'write-on-alts
                      (seq-uniq (append (write-on--variants o)
                                        (seq-filter #'stringp alts))))
         (set-buffer-modified-p t)
         (write-on--panel-refresh)
         ;; Open the picker only if the user is still there.
         (if (and (eq (window-buffer) (current-buffer))
                  (not (active-minibuffer-window))
                  (<= (overlay-start o) (point) (overlay-end o)))
             (write-on--pick o nil)
           (message "write-on: %d alternatives ready (C-c w ; to pick)"
                    (1- (length (overlay-get o 'write-on-alts)))))))
     (lambda () (when fresh (delete-overlay o)))
     o)))

;;; Lab
;;
;; AI review of the region (or whole buffer).  Marks are transient
;; overlays; fixes become alternatives (original kept); trims show as
;; ghosted cuts you can keep before committing.

(defface write-on-lab-mark '((t :underline (:style wave :color "#d08770")))
  "Face for text the Lab flagged.")

(defconst write-on-lab-checks
  '(("Fix punctuation and typos" fix
     "Find typos, spelling mistakes, and punctuation errors.")
    ("Mark the weakest sentences" mark
     "Find the few weakest sentences: vague, flat, or redundant.")
    ("Mark sentences that run long" mark "Find sentences that run too long.")
    ("Mark convoluted sentences" mark "Find sentences that are hard to follow.")
    ("Mark words that don't fit the tone" mark
     "Find words or phrases that don't fit the tone of the text.")
    ("Mark hedges and filler" mark "Find hedges and filler words or phrases.")
    ("Slight trim (-10%)" trim 10)
    ("Tighten more (-20%)" trim 20)
    ("Even sharper (-30%)" trim 30)
    ("Cut in half (-50%)" trim 50))
  "Lab menu: (LABEL TYPE ARG).")

(defvar-keymap write-on-cut-map
  "RET" #'write-on-lab-keep
  "<mouse-1>" #'write-on-lab-keep)

(defun write-on--lab-overlays (&optional beg end)
  "Lab overlays between BEG and END."
  (write-on--overlays 'write-on-lab beg end))

(defun write-on--cuts ()
  "Lab overlays proposing a cut."
  (seq-filter (lambda (o) (overlay-get o 'write-on-cut)) (write-on--lab-overlays)))

(defun write-on--lab-overlay (beg end &rest props)
  "Make a Lab overlay over BEG..END with PROPS."
  (let ((o (make-overlay beg end nil t nil)))
    (overlay-put o 'write-on-lab t)
    (overlay-put o 'evaporate t)
    (while props (overlay-put o (pop props) (pop props)))
    o))

(defun write-on--locate (text beg end)
  "First occurrence of TEXT between BEG and END, as (START . END)."
  (save-excursion
    (goto-char beg)
    (when (and (stringp text) (not (string-empty-p text))
               (search-forward text end t))
      (cons (match-beginning 0) (point)))))

(defun write-on--tokens (s)
  "Word and punctuation tokens of S as (TOKEN START END), case-folded."
  (let ((i 0) toks)
    (while (string-match "\\w+\\|[^[:space:][:word:]]" s i)
      (push (list (downcase (match-string 0 s)) (match-beginning 0) (match-end 0))
            toks)
      (setq i (match-end 0)))
    (nreverse toks)))

(defun write-on--deletions (orig trimmed)
  "Ranges (START . END) of ORIG deleted to get TRIMMED.
Return `invalid' if TRIMMED is not ORIG with only words removed."
  (let ((want (mapcar #'car (write-on--tokens trimmed)))
        ranges)
    (dolist (tok (write-on--tokens orig))
      (if (equal (car tok) (car want))
          (pop want)
        ;; Merge with the previous cut when only spaces separate them
        ;; (never across a line break, so paragraphs stay apart).
        (if (and ranges (string-match-p "\\`[ \t]*\\'"
                                        (substring orig (cdar ranges) (nth 1 tok))))
            (setcdr (car ranges) (nth 2 tok))
          (push (cons (nth 1 tok) (nth 2 tok)) ranges))))
    (if want 'invalid (nreverse ranges))))

(defun write-on--cut-bounds (b e)
  "Widen cut B..E over one side's spaces, so no gap is left after cutting.
A cut after a space takes that space; a cut at the start of a line takes
the space after it; a cut attached to the previous word (\", in the
end,\") takes neither, so the words around it stay apart."
  (save-excursion
    (goto-char b)
    (cond ((memq (char-before) '(?\s ?\t))
           (skip-chars-backward " \t")
           (cons (point) e))
          ((or (bobp) (bolp))
           (goto-char e)
           (skip-chars-forward " \t")
           (cons b (point)))
          (t (cons b e)))))

(defun write-on--lab-prompt (type arg text)
  "Model prompt for Lab check TYPE (with its ARG) over TEXT."
  (pcase type
    ('mark (format "%s
Reply with a JSON array of objects {\"text\": the exact passage copied verbatim from the text, \"note\": a short reason}.

Text:
%s" arg text))
    ('fix (format "%s
Reply with a JSON array of objects {\"text\": the exact erroneous passage copied verbatim from the text, with enough surrounding words to be unique, \"fix\": the corrected passage}. Reply [] if there are none.

Text:
%s" arg text))
    ('trim (format "Cut about %d%% of the words from this text by deleting the weakest words and phrases.
Only delete: never add, change, or reorder anything.
Reply with a JSON array holding one string: the trimmed text.

Text:
%s" arg text))))

(defun write-on--lab-apply (type items beg end)
  "Show the model's ITEMS for Lab TYPE in BEG..END."
  (pcase type
    ('mark
     (let ((n 0))
       (dolist (it items)
         (when-let* ((pos (write-on--locate (alist-get 'text it) beg end)))
           (write-on--lab-overlay (car pos) (cdr pos)
                                  'face 'write-on-lab-mark
                                  'help-echo (alist-get 'note it)
                                  'write-on-note (alist-get 'note it))
           (cl-incf n)))
       (message "Lab: %d marked. %s walks through them." n "C-c w `")))
    ('fix
     (let ((n 0))
       (dolist (it items)
         (let ((pos (write-on--locate (alist-get 'text it) beg end))
               (fix (alist-get 'fix it)))
           (when (and pos (stringp fix))
             (write-on--swap (write-on--alt-make (car pos) (cdr pos)
                                                 (write-on--infer-kind (car pos) (cdr pos))
                                                 (list (alist-get 'text it) fix))
                             fix)
             (cl-incf n))))
       (message "Lab: %d fixed; each keeps its original (C-c w [ to revert)." n)))
    ('trim
     (let* ((orig (buffer-substring-no-properties beg end))
            (cuts (write-on--deletions orig (or (car items) ""))))
       (if (eq cuts 'invalid)
           (message "Lab: the model rewrote instead of cutting; try again.")
         (pcase-dolist (`(,b . ,e) cuts)
           (pcase-let ((`(,cb . ,ce) (write-on--cut-bounds (+ beg b) (+ beg e))))
             (write-on--lab-overlay cb ce
                                    'face 'write-on-ghost
                                    'write-on-cut t
                                    'keymap write-on-cut-map
                                    'help-echo "RET keeps this")))
         (message "Lab: %d → %d words. RET on faded text keeps it; C-c w = to make the cuts."
                  (length (split-string orig)) (length (split-string (car items)))))))))

(defun write-on-lab ()
  "Run a Lab check on the region (or whole buffer), or finish a Lab pass."
  (interactive)
  (let* ((cuts (write-on--cuts))
         (extra (append (when cuts '(("Make the cuts" commit)))
                        (when (write-on--lab-overlays) '(("Done (clear marks)" clear)))))
         (menu (append extra write-on-lab-checks))
         (choice (assoc (completing-read "Lab: " menu nil t) menu)))
    (pcase (nth 1 choice)
      ('commit (write-on-lab-commit))
      ('clear (write-on-lab-clear))
      (type
       (let* ((beg (copy-marker (if (use-region-p) (region-beginning) (point-min))))
              (end (copy-marker (if (use-region-p) (region-end) (point-max))))
              (text (buffer-substring-no-properties beg end)))
         (deactivate-mark)
         (write-on-lab-clear)
         (write-on--ask
          (write-on--lab-prompt type (nth 2 choice) text)
          (lambda (items)
            (if (string= text (buffer-substring-no-properties beg end))
                (write-on--lab-apply type items beg end)
              (message "Lab: text changed while waiting; run it again.")))))))))

(defun write-on-lab-keep ()
  "Keep the faded text at point instead of cutting it."
  (interactive)
  (mapc #'delete-overlay
        (seq-filter (lambda (o) (overlay-get o 'write-on-cut)) (overlays-at (point)))))

(defun write-on-lab-commit ()
  "Delete all text still marked to cut."
  (interactive)
  (dolist (o (sort (write-on--cuts) (lambda (a b) (> (overlay-start a) (overlay-start b)))))
    (delete-region (overlay-start o) (overlay-end o)))
  (write-on-lab-clear))

(defun write-on-lab-clear ()
  "Remove every Lab mark and cut."
  (interactive)
  (mapc #'delete-overlay (write-on--lab-overlays)))

(defvar-keymap write-on-lab-repeat-map
  "`" #'write-on-lab-next)

(defun write-on-lab-next ()
  "Go to the next Lab mark or cut and show its note.  Then ` repeats."
  (interactive)
  (let* ((all (sort (mapcar #'overlay-start (write-on--lab-overlays)) #'<))
         (pos (or (seq-find (lambda (p) (> p (point))) all) (car all))))
    (unless pos (user-error "No Lab marks"))
    (goto-char pos)
    (when-let* ((note (get-char-property pos 'write-on-note)))
      (message "%s" note))
    (set-transient-map write-on-lab-repeat-map)))

;;; Export and word count

(defun write-on--clean-text ()
  "The buffer's text without its ghosted passages."
  (let ((pos (point-min)) parts)
    (pcase-dolist (`(,b . ,e)
                   (sort (mapcar (lambda (o) (write-on--cut-bounds (overlay-start o)
                                                                   (overlay-end o)))
                                 (write-on--ghosts))
                         (lambda (x y) (< (car x) (car y)))))
      (when (> b pos) (push (buffer-substring-no-properties pos b) parts))
      (setq pos (max pos e)))
    (push (buffer-substring-no-properties pos (point-max)) parts)
    (apply #'concat (nreverse parts))))

(defun write-on-export (file)
  "Write the document to FILE without its ghosted text."
  (interactive
   (list (read-file-name
          "Export to: " nil nil nil
          (concat (file-name-base (or buffer-file-name (buffer-name))) "-clean."
                  (or (and buffer-file-name (file-name-extension buffer-file-name))
                      "md")))))
  (when (and (file-exists-p file)
             (not (y-or-n-p (format "%s exists; overwrite? " file))))
    (user-error "Export cancelled"))
  (let ((text (write-on--clean-text)))
    (with-temp-file file (insert text))
    (message "write-on: exported %s" (abbreviate-file-name file))))

(defvar-local write-on--count-cache nil
  "(KEY . WORDS) for the last word count.")

(defun write-on--count-key ()
  "What the word count depends on: the text and where the ghosts are."
  (cons (buffer-chars-modified-tick) (mapcar #'overlay-start (write-on--ghosts))))

(defun write-on--word-count ()
  "Words in the buffer, not counting ghosted text.  Cached."
  (let ((key (write-on--count-key)))
    (unless (equal key (car write-on--count-cache))
      (setq write-on--count-cache
            (cons key (let ((text (write-on--clean-text)))
                        (with-temp-buffer
                          (insert text)
                          (count-words (point-min) (point-max)))))))
    (cdr write-on--count-cache)))

(defvar-local write-on--count-timer nil
  "Pending recount for the mode line, run after a pause in typing.")

(defun write-on--lighter ()
  "Mode line text, with the word count as of the last pause in typing.
Recounting a book-length buffer on every keystroke is noticeable, so the
count is redone once, after 1s idle."
  (unless (or write-on--count-timer
              (equal (write-on--count-key) (car write-on--count-cache)))
    (let ((buf (current-buffer)))
      (setq write-on--count-timer
            (run-with-idle-timer
             1 nil (lambda ()
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (setq write-on--count-timer nil)
                         (write-on--word-count)
                         (force-mode-line-update))))))))
  (concat " WO" (or write-on--busy "")
          (and write-on--count-cache (format " %dw" (cdr write-on--count-cache)))))

(defun write-on-count-words ()
  "Show the word count, not counting ghosted text."
  (interactive)
  (message "%d words (ghosted text not counted)" (write-on--word-count)))

;;; Panel
;;
;; A Write_On-style side panel listing the alternatives of every span at
;; point (word, sentence, paragraph), live as you move.

(defcustom write-on-panel-width 60
  "Width of the alternatives panel."
  :type 'integer)

(defconst write-on--panel-name "*write-on*")

(defun write-on--panel-window ()
  "The window showing the alternatives panel, if any."
  (get-buffer-window write-on--panel-name))

(defun write-on--panel-refresh ()
  "Redraw the panel for the spans at point, if the panel is showing."
  (when-let* ((win (write-on--panel-window)))
    (let ((src (current-buffer))
          (spans (write-on--alts-at)))
      (with-current-buffer (window-buffer win)
        (let ((inhibit-read-only t)
              (line (line-number-at-pos)))
          (erase-buffer)
          (setq write-on--source src)
          (if (null spans)
              (insert (propertize (concat "No alternatives here.\n\n"
                                          "C-c w ; adds one.\n"
                                          "C-c w : asks the model.\n")
                                  'face 'shadow))
            (dolist (o spans)
              (let ((cur (with-current-buffer src (write-on--text o))))
                (insert (propertize (capitalize (symbol-name (overlay-get o 'write-on-kind)))
                                    'face 'bold)
                        "\n")
                (dolist (v (with-current-buffer src (write-on--shown-variants o)))
                  (insert (propertize (concat (if (equal v cur) "● " "○ ") v "\n")
                                      'write-on-o o 'write-on-v v
                                      'face (if (equal v cur) 'write-on-alt-dots 'default)))))
              (insert "\n")))
          (insert (propertize "RET use · a add · g ask model · q close" 'face 'shadow))
          (goto-char (point-min))
          (forward-line (1- line))
          (set-window-point win (point)))))))

(defun write-on--panel-span ()
  "The span and variant on this panel line."
  (or (get-text-property (point) 'write-on-o)
      (user-error "Not on an alternative"))
  (unless (buffer-live-p write-on--source) (user-error "Document buffer is gone"))
  (list (get-text-property (point) 'write-on-o) (get-text-property (point) 'write-on-v)))

(defun write-on-panel-choose ()
  "Swap this line's alternative into the document."
  (interactive)
  (pcase-let ((`(,o ,v) (write-on--panel-span)))
    (with-current-buffer write-on--source
      (write-on--swap o v)
      (set-buffer-modified-p t)
      (write-on--flash o)
      (write-on--panel-refresh))))

(defun write-on-panel-add (text)
  "Add TEXT as an alternative to this line's span and use it."
  (interactive "sNew alternative: ")
  (pcase-let ((`(,o ,_) (write-on--panel-span)))
    (with-current-buffer write-on--source
      (overlay-put o 'write-on-alts (append (write-on--variants o) (list text)))
      (write-on--swap o text)
      (set-buffer-modified-p t)
      (write-on--panel-refresh))))

(defun write-on-panel-ai ()
  "Ask the model for more alternatives to this line's span."
  (interactive)
  (pcase-let ((`(,o ,_) (write-on--panel-span)))
    (with-current-buffer write-on--source
      (write-on--ai o nil))))

(defvar-keymap write-on-panel-mode-map
  "RET" #'write-on-panel-choose
  "a" #'write-on-panel-add
  "g" #'write-on-panel-ai
  "n" #'next-line
  "p" #'previous-line)

(defun write-on--pad-pane ()
  "Give a side pane's text room: a blank line above, margins at the sides.
Display only (an overlay and window margins), so the text, which for
Overflow is saved, doesn't change."
  (setq-local left-margin-width 2
              right-margin-width 2)
  (let ((pad (make-overlay (point-min) (point-min) nil nil t)))
    (overlay-put pad 'before-string "\n")
    (overlay-put pad 'write-on-pad t)))

(define-derived-mode write-on-panel-mode special-mode "Alternatives"
  "Side panel listing the alternatives at point in a write-on document."
  (visual-line-mode 1)
  (write-on--pad-pane))

(defun write-on-panel-toggle ()
  "Show or hide the alternatives panel."
  (interactive)
  (if-let* ((win (write-on--panel-window)))
      (delete-window win)
    (with-current-buffer (get-buffer-create write-on--panel-name)
      (unless (derived-mode-p 'write-on-panel-mode) (write-on-panel-mode)))
    (display-buffer-in-side-window (get-buffer write-on--panel-name)
                                   `((side . left) (window-width . ,write-on-panel-width)))
    (write-on--panel-refresh)))

;;; Overflow

(defvar-local write-on--overflow nil
  "The document's Overflow buffer.
Held by reference: the document's name can change (renames, uniquify).")
(put 'write-on--overflow 'permanent-local t)

(defun write-on--overflow-buffer ()
  "The document's Overflow buffer, created on first use."
  (unless (buffer-live-p write-on--overflow)
    (let ((src (current-buffer)))
      (setq write-on--overflow
            (generate-new-buffer (format "*overflow: %s*" (buffer-name))))
      (with-current-buffer write-on--overflow
        (write-on-overflow-mode)
        (setq write-on--source src))))
  write-on--overflow)

(defun write-on--kill-overflow ()
  "Kill the document's Overflow buffer (`kill-buffer-hook')."
  (when (buffer-live-p write-on--overflow)
    (kill-buffer write-on--overflow)))

(defun write-on--overflow-text ()
  "The text in the document's Overflow."
  (with-current-buffer (write-on--overflow-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun write-on-overflow-toggle ()
  "Show or hide the Overflow pane."
  (interactive)
  (let* ((buf (write-on--overflow-buffer))
         (win (get-buffer-window buf)))
    (if win
        (delete-window win)
      (display-buffer-in-side-window
       buf `((side . right) (window-width . ,write-on-overflow-width))))))

(defun write-on-stash ()
  "Move the region (or sentence at point) to the top of Overflow."
  (interactive)
  (pcase-let ((`(,beg . ,end) (write-on--bounds)))
    (let ((text (string-trim (buffer-substring-no-properties beg end))))
      (with-current-buffer (write-on--overflow-buffer)
        (goto-char (point-min))
        (insert text "\n\n"))
      (delete-region beg end)
      (unless (get-buffer-window (write-on--overflow-buffer))
        (write-on-overflow-toggle)))))

(defun write-on-overflow-use ()
  "Move the Overflow paragraph at point into the page at its point."
  (interactive)
  (unless (buffer-live-p write-on--source)
    (user-error "Document buffer is gone"))
  (pcase-let* ((`(,beg . ,end) (or (bounds-of-thing-at-point 'paragraph)
                                   (user-error "Nothing here")))
               (text (string-trim (buffer-substring-no-properties beg end))))
    (delete-region beg end)
    (with-current-buffer write-on--source
      (insert text))
    (when-let* ((w (get-buffer-window write-on--source)))
      (select-window w))))

(defun write-on--overflow-changed (&rest _)
  "Mark the document modified: Overflow is saved along with it."
  (when (buffer-live-p write-on--source)
    (with-current-buffer write-on--source (set-buffer-modified-p t))))

(defvar-keymap write-on-overflow-mode-map
  "C-c C-c" #'write-on-overflow-use)

(define-derived-mode write-on-overflow-mode text-mode "Overflow"
  "Stash pane for a write-on document.  \\[write-on-overflow-use] puts text back."
  (visual-line-mode 1)
  (write-on--pad-pane)
  (add-hook 'after-change-functions #'write-on--overflow-changed nil t))

;;; Saved state

(defun write-on--save ()
  "Save alternatives, ghosts, and Overflow (`after-save-hook')."
  (let ((ghosts (mapcar (lambda (o)
                          (list (overlay-start o) (overlay-end o) (write-on--text o)))
                        (write-on--ghosts)))
        ;; Empty spans mean the text was deleted; let them go.
        (alts (mapcar (lambda (o)
                        (list (overlay-start o) (overlay-end o) (write-on--text o)
                              (overlay-get o 'write-on-kind) (write-on--variants o)
                              (overlay-get o 'write-on-nested)))
                      (seq-remove (lambda (o) (= (overlay-start o) (overlay-end o)))
                                  (write-on--alts))))
        (overflow (write-on--overflow-text))
        (file (write-on--state-file)))
    (cond
     (write-on--state-broken
      (message "write-on: %s failed to load; not overwriting it" file))
     ((or ghosts alts (not (string-empty-p overflow)) (file-exists-p file))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (let (print-length print-level)
          (prin1 (list :version 1 :ghosts ghosts :alts alts
                       :overflow overflow)
                 (current-buffer))))))))

(defun write-on--find (beg end text)
  "Where TEXT is now: at BEG..END if unchanged, else its first occurrence."
  (if (and (<= end (point-max))
           (string= text (buffer-substring-no-properties beg end)))
      (cons beg end)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward text nil t)
        (cons (match-beginning 0) (point))))))

(defun write-on--restore ()
  "Load the document's saved alternatives, ghosts, and Overflow."
  (let ((file (write-on--state-file)))
    (when (file-exists-p file)
      (condition-case err
          (let ((data (with-temp-buffer
                        (insert-file-contents file)
                        (read (current-buffer)))))
            (pcase-dolist (`(,b ,e ,text) (plist-get data :ghosts))
              (when-let* ((pos (write-on--find b e text)))
                (write-on--ghost (car pos) (cdr pos))))
            (pcase-dolist (`(,b ,e ,text ,kind ,vs ,nested) (plist-get data :alts))
              (when-let* ((pos (write-on--find b e text)))
                (overlay-put (write-on--alt-make (car pos) (cdr pos) kind vs)
                             'write-on-nested nested)))
            (with-current-buffer (write-on--overflow-buffer)
              (let (after-change-functions)
                (erase-buffer)
                (insert (or (plist-get data :overflow) ""))
                (goto-char (point-min)))))
        (error
         (setq write-on--state-broken t)
         (message "write-on: can't read %s: %s" file (error-message-string err)))))))

;;; Mode

(defvar-keymap write-on-prefix-map
  :doc "write-on commands, under \`C-c w' in `write-on-mode'.
One prefix keeps clear of Org's and Markdown's own \`C-c' keys."
  ";" #'write-on-alt
  ":" #'write-on-alt-ai
  "]" #'write-on-alt-next
  "[" #'write-on-alt-prev
  "'" #'write-on-panel-toggle
  "/" #'write-on-ghost-toggle
  "." #'write-on-stash
  "," #'write-on-overflow-toggle
  "=" #'write-on-lab
  "`" #'write-on-lab-next
  "e" #'write-on-export
  "c" #'write-on-count-words
  "r" #'write-on-alt-remove)

(defvar-keymap write-on-mode-map
  "C-c w" write-on-prefix-map)

;;;###autoload
(define-minor-mode write-on-mode
  "Alternative control for prose.

\\{write-on-mode-map}"
  :lighter (:eval (write-on--lighter))
  (if write-on-mode
      (progn
        ;; Modern prose uses one space after a period.
        (setq-local sentence-end-double-space nil)
        (unless write-on--loaded
          (setq write-on--state-broken nil)
          (when buffer-file-name (write-on--restore))
          (setq write-on--loaded t))
        (add-hook 'after-save-hook #'write-on--save nil t)
        (add-hook 'post-command-hook #'write-on--highlight nil t)
        (write-on--theme-faces)
        (add-hook 'enable-theme-functions #'write-on--theme-faces)
        (add-hook 'kill-buffer-hook #'write-on--kill-overflow nil t))
    (remove-hook 'after-save-hook #'write-on--save t)
    (remove-hook 'post-command-hook #'write-on--highlight t)
    (remove-hook 'kill-buffer-hook #'write-on--kill-overflow t)
    (setq write-on--loaded nil)
    (when write-on--count-timer
      (cancel-timer write-on--count-timer)
      (setq write-on--count-timer nil))
    (mapc #'delete-overlay (append (write-on--ghosts) (write-on--alts)
                                   (write-on--lab-overlays)))))

;; Apply now too, so reloading the file replaces stale face colors.
(write-on--theme-faces)

(provide 'write-on)
;;; write-on.el ends here
