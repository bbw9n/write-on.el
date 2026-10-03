# write-on.el

[![test](https://github.com/bbw9n/write-on.el/actions/workflows/test.yml/badge.svg)](https://github.com/bbw9n/write-on.el/actions/workflows/test.yml)

"Alternative control" for prose in Emacs, inspired by [Jason Fried's Write_On](https://x.com/jasonfried/status/2105403067793584590). Keep variants of a word, sentence, or paragraph; dim text instead of deleting it; stash cuts nearby; let a model suggest alternatives, flag weak spots, and trim.

## Demo

https://github.com/user-attachments/assets/16168d41-628f-4101-a943-8356eeb6011d

Word, sentence and paragraph alternatives (from the model or typed in), cycling through them, the side panel, dimming text, stashing to Overflow and popping it back, and a Lab trim. The model's suggestions in the demo are pre-recorded.

## Setup

Requires Emacs 29+. AI features need [gptel](https://github.com/karthink/gptel) with a backend configured. Live preview while picking works with Vertico, Helm, and Icomplete/Fido.

```elisp
;; config.el
(add-to-list 'load-path "/path/to/write-on.el")   ; where you cloned this repo
(require 'write-on)
(add-hook 'markdown-mode-hook #'write-on-mode)
(add-hook 'org-mode-hook #'write-on-mode)
```

## Keys

Everything is under `C-c w`, so it never shadows Org or Markdown keys.

| Key                 | What it does                                                                                                                                                                                                                                                                        |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `C-c w ;`           | `write-on-alt`: Pick or add an alternative for the word at point (`C-u` sentence, `C-u C-u` paragraph, or the region). Moving through the list previews each one in place; `C-g` restores. Choosing the current text records it, so you can rewrite in place and keep the original. |
| `C-c w :`           | `write-on-alt-ai`: Same span, alternatives from the model (spinner while it thinks)                                                                                                                                                                                                 |
| `C-c w ]` `C-c w [` | `write-on-alt-next` / `write-on-alt-prev`: Cycle the span at point; then plain `]` / `[` keep cycling. A run of cycling is one undo.                                                                                                                                                |
| `C-c w '`           | `write-on-panel-toggle`: Side panel listing Word / Sentence / Paragraph alternatives at point: `RET` use, `a` add, `g` ask model                                                                                                                                                    |
| `C-c w /`           | `write-on-ghost-toggle`: Dim / un-dim the region or sentence                                                                                                                                                                                                                        |
| `C-c w .`           | `write-on-stash`: Move the region or sentence to Overflow                                                                                                                                                                                                                           |
| `C-c w ,`           | `write-on-overflow-toggle`: Show/hide Overflow; `C-c C-c` there puts a paragraph back                                                                                                                                                                                               |
| `C-c w =`           | `write-on-lab`: Lab on the region or buffer: mark weak/long/convoluted sentences, off-tone words, hedges; fix typos (as alternatives); trim 10–50%. Afterwards also offers *Make the cuts* / *Done*                                                                                 |
| ``C-c w ` ``        | `write-on-lab-next`: Walk through Lab marks; then plain `` ` `` repeats                                                                                                                                                                                                             |
| `RET` on faded text | `write-on-lab-keep`: Keep a proposed cut                                                                                                                                                                                                                                            |
| `C-c w e`           | `write-on-export`: Write the document without ghosted text                                                                                                                                                                                                                          |
| `C-c w c`           | `write-on-count-words`: Word count, ghosted text excluded (also in the mode line: `WO 535w`)                                                                                                                                                                                        |
| `C-c w r`           | `write-on-alt-remove`: Keep the current text, forget the span's alternatives                                                                                                                                                                                                        |

## Behavior

- Swapping a sentence or paragraph keeps the word alternatives and ghosts inside each version; they come back with it.
- Undo works on swaps: the span re-wraps the restored text.
- While picking, the rest of the page dims (`write-on-dim-others`); a swap flashes briefly.

## Look

Colors come from your theme: span background between the default background and the selection (`write-on-tint`, 0–1), underline and dots from the accent (`write-on-accent-face`). The span at point is bold. Variant dots show in the echo area (`write-on-alt-indicator`: `echo`, `inline`, or nil). Set `write-on-theme-colors` to nil to style the faces yourself.

## Storage

Alternatives (with what's nested in each version), ghosts, and Overflow are saved on each document save to `write-on-directory`, one file per document named after its full path (`!Users!you!essay.md.eld`, like Emacs backup names). Nothing is written next to the document, which stays plain text. Lab marks are not saved.

| Setup         | `write-on-directory` default                                                                                               |
|---------------|----------------------------------------------------------------------------------------------------------------------------|
| Doom          | `doom-state-dir` + `write-on/` (`~/.config/emacs/.local/state/write-on/`): Doom's place for user-saved data, not the cache |
| vanilla Emacs | `~/.emacs.d/write-on/` (`locate-user-emacs-file`)                                                                          |

Moving or renaming a document outside Emacs detaches its state (it's keyed by path).

## Tests

```bash
emacs --batch -L . -l write-on-test.el -f ert-run-tests-batch-and-exit
```
