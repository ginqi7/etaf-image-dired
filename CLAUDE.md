# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

The project uses [Eldev](https://github.com/dgutov/eldev). Dependencies (ETAF,
ecss, ekp, ebox) are loaded from local checkouts configured in `Eldev`;
override the parent directory with `ETAF_IMAGE_DIRED_DEPS`.

```sh
eldev test                          # run all Buttercup specs
eldev test "dired binding"          # run every spec whose full name matches
eldev test "deletes only the flagged files"   # run one spec
eldev compile                       # byte-compile etaf-image-dired.el
eldev lint                          # doc/package/relint (the doc linter has pre-existing warnings)
```

Tests need no external image tools: `image-dired-create-thumb` is stubbed and
specs write a 1x1 PNG themselves.

### Reloading into a running Emacs

A plain `(load-file "etaf-image-dired.el")` fails with
`Component ... is already registered` once a component definition changed.
Load inside ETAF's redefinition boundary instead:

```sh
emacsclient --eval '(etaf-component-redefine-run (lambda () (load-file "/absolute/path/etaf-image-dired.el")))'
```

The test file can be reloaded with a plain `(load-file ...)`.

## Architecture

The whole package lives in `etaf-image-dired.el`; `test/etaf-image-dired-test.el`
is the Buttercup suite. There is no build step beyond byte-compilation.

### State and reactivity

`etaf-image-dired--session` (one per gallery buffer) holds the directory, the
live buffer/frame, timers, and a set of ETAF refs:

- `entries-ref` — list of thumbnail plists (`:file :thumb :ref :name :state
  :columns :rows :inset :pad-top ...`). It is the render source for the grid.
- `focus-ref` — selected/current card ref (mouse hover or keyboard focus).
- `preview-ref`, `split-ref`, `preview-width-ref` — preview pane state.
- `marks-ref` — a cache of Dired marks (`(FILE . marked|flagged)`); Dired is
  the source of truth, see below.
- `hidden-ref` — files removed from the gallery with `C-d` (kept out until a
  rescan).
- `thumbs-per-row-ref` — fixed line-up width, or nil for automatic wrapping.

Rendering is driven by `etaf-set-value` on these refs. **Never mutate a plist
that is already inside a ref**; copy it first (`etaf-image-dired--entry-ready`
does this). The reactive layer compares old and new values and cannot notice
in-place mutation.

### Thumbnail pipeline

1. `etaf-image-dired--image-files` scans the directory with
   `image-file-name-regexp`.
2. `etaf-image-dired--entry` uses `image-dired-thumb-name` and
   `image-dired-dir`, compares thumbnail and image mtimes, and delegates
   missing/stale thumbnails to `image-dired-create-thumb` (never duplicating a
   job already in `image-dired-queue`).
3. `etaf-image-dired--start-timer` / `--tick` poll `image-dired-queue` and
   `image-dired-queue-active-jobs`, mark entries `ready` or `failed`, and stop
   once the queue drains. `C-r` deletes the cached thumbnails and rescans.

Image-dired's cache layout is deliberately reused so the gallery and
`image-dired` share thumbnails.

### ETAF view

The UI is an `etaf-view` tree of components defined with
`etaf-define-component`:

- `--card` — one thumbnail, fixed box size, selection/mark/flag border, RET
  action.
- `--library` — header, wrapping grid, status line; scrolls internally.
- `--preview` — image pane inside the same window (no extra Emacs window).
- `--splitter` — drag handle for the preview width.
- `--app` — root; holds the global `:keys` map.

`etaf-image-dired--view` wraps `--app` with `etaf-app-open` in the entry point
`etaf-image-dired`.

ETAF constraints that shape the keymap (`etaf-image-dired--keys`):

- No multi-key sequence that starts with a printable character; image-dired's
  `g g`, `g f`, `g i`, `t t`, `t r` become `G`, `F`, `I`, `t`, `T`.
- TAB, Escape, `M-g`, `C-g`, `?` and arrows are reserved by ETAF; TAB-based
  image-dired commands become other keys (TAB jumps to Dired as `J`).
- The keymap test calls `etaf--keys-validate`; keep every key legal for ETAF.

Layout details: the grid height is clamped to the ETAF viewport and scrolls
inside the buffer; opening the preview remounts the layout via `split-ref`,
while moving the selection only repaints borders and the preview. Fixed
line-ups set an explicit pixel width on the wrapping flex container so exactly
N cards fit per row.

### Dired binding

Marks are not gallery-local. `m`, `u`, `d` and `U` write through the Dired
buffer visiting the same directory (created with `dired-noselect` when
missing), so Dired commands operate on files marked in the gallery.

- `--sync-marks-from-dired` reads `*`/`D` markers back into `marks-ref`.
- `--sync-dired-buffer` runs from buffer-local entries on
  `dired-mode-hook` (`post-command-hook`, `dired-after-readin-hook`), updates
  all galleries for that directory, and mirrors Dired point into the gallery
  selection when `image-dired-track-movement` is non-nil.
- `--track-in-dired` mirrors gallery selection back to a visible Dired window.
- `x` calls `dired-do-flagged-delete`, preserving Dired's confirmation and
  `delete-by-moving-to-trash` behavior.
- Opening a gallery adopts marks already present in a matching Dired buffer.

### Tests

Specs use `etaf-image-dired-test--with-gallery` (temporary directory and
isolated thumbnail cache), `--with-session` (unmounted session) and
`--with-tick-session` (fake ETAF runtime for timer tests). Use
`etaf-image-dired-test--select` to make a card current and
`etaf-image-dired-test--write` to create image files. Commands are tested with
`etaf-app-state` spied to return the session, so they do not need a mounted
buffer. Dired integration specs must clean up any Dired buffer they create and
stub `dired-deletion-confirmer` when exercising deletion.
