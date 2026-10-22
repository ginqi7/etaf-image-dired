;;; etaf-image-dired.el --- Image-dired thumbnail gallery rendered with ETAF  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (etaf "0.3.1"))
;; Keywords: multimedia, hypermedia
;; URL: https://etaf.top/

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

;; `etaf-image-dired' shows every image in a directory as a wrapping grid of
;; thumbnails.  Thumbnails are produced and cached by the built-in
;; `image-dired' package; the grid itself is an ETAF View, so the pictures
;; are committed through ETAF's `image' Host and the buffer keeps ETAF's
;; keyboard navigation.  The card under the mouse pointer, or the one that
;; holds keyboard focus, is framed as the current selection.
;;
;; Usage:
;;
;;   M-x etaf-image-dired
;;
;; shows the images in `default-directory'.  With a prefix argument it asks
;; for a directory.  Inside the gallery, the keys follow
;; `image-dired-thumbnail-mode-map' where ETAF allows them:
;;
;;   f b n p a e   move by image, line, or to the ends of a line
;;   RET           display the selected image in the preview pane
;;   SPC DEL       display the next / previous image
;;   C-<return>    display the image in an external viewer
;;   P             toggle the preview pane
;;   m u U         mark, unmark, unmark everything
;;   d <delete>    flag the image for deletion; x deletes the flagged files
;;   C-d           remove the thumbnail from the gallery (file is kept)
;;   t T           add / remove image-dired tags
;;   c             add or edit the image comment
;;   D             set the ImageDescription EXIF tag
;;   L R           rotate the original image left / right
;;   S             start or stop a slideshow
;;   w             copy the selected (or marked) file names
;;   W             set the selected image as the wallpaper
;;   .             track the file in an existing Dired buffer
;;   J             jump to Dired for the selected file
;;   G F I         automatic / fixed / interactive thumbnails per row
;;   [ ] 0         adjust the preview pane width
;;   o             open the image in another window
;;   g             rescan the directory
;;   C-r           delete cached thumbnails and generate them again
;;   q             close the gallery
;;
;; Marks are shared with Dired.  The gallery keeps no mark list of its own:
;; `m', `u', `d' and `U' write through the Dired buffer visiting the same
;; directory (creating one when necessary), so Dired's own commands operate
;; on the files marked here.  Changes made in Dired are copied back after
;; every Dired command, and moving in either buffer moves the other to the
;; same image when `image-dired-track-movement' is non-nil.  `x' runs
;; `dired-do-flagged-delete', including its confirmation and trash settings.
;;
;; ETAF does not allow multi-key sequences that start with a printing
;; character, so image-dired's "g g", "g f", "g i", "t t" and "t r" become
;; the single keys G, F, I, t and T.  TAB is reserved by ETAF for focus
;; movement, hence J jumps to Dired instead.
;;
;; The preview does not create a new window: the gallery's own window is
;; divided into thumbnails on the left and the selected image on the right.
;; Clicking a card opens the original image.
;;
;; The thumbnail grid is exactly as tall as the Emacs window, so the buffer
;; itself never scrolls.  Cards scroll inside the grid instead: moving focus
;; down or up reveals the newly focused card, and the mouse wheel scrolls the
;; grid under the pointer.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'dired)
(require 'exif)
(require 'image-dired)
(require 'image-file)
(require 'etaf)

(declare-function w32-shell-execute "w32fns.c")

(defgroup etaf-image-dired nil
  "Image-dired thumbnail gallery rendered with ETAF."
  :group 'multimedia
  :prefix "etaf-image-dired-")

(defcustom etaf-image-dired-refresh-interval 0.3
  "Seconds between checks while image-dired creates thumbnails."
  :type 'number
  :group 'etaf-image-dired)

(defcustom etaf-image-dired-preview-width-step 4
  "Columns added or removed by one preview width adjustment."
  :type 'integer
  :group 'etaf-image-dired)

(defcustom etaf-image-dired-thumbs-per-row 3
  "Thumbnails per row used by `etaf-image-dired-line-up'."
  :type 'integer
  :group 'etaf-image-dired)

(defconst etaf-image-dired--muted-color "gray50"
  "Color used for secondary text.")

(defconst etaf-image-dired--error-color "red"
  "Color used for a thumbnail that could not be created.")

(defconst etaf-image-dired--border-color "gray60"
  "Color of the frame drawn around every card.")

(defconst etaf-image-dired--selection-color "#3B82F6"
  "Frame color of the card under the mouse pointer or keyboard focus.")

(defconst etaf-image-dired--selection-width '(px 1)
  "Width of every card's frame.
All cards draw a frame of this width, so moving the selection repaints
borders instead of changing the layout.  Ebox allows only 1px top and
bottom borders.")

(defconst etaf-image-dired--mark-color "green3"
  "Frame color of a file marked with `etaf-image-dired-mark-thumb-original-file'.")

(defconst etaf-image-dired--flag-color "red"
  "Frame color of a file flagged for deletion.")

(defconst etaf-image-dired--card-gap '((lh 1) (px 16))
  "Row and column gap between thumbnail cards.
The vertical component must use line-height units; the horizontal one may
use pixels.")

(defconst etaf-image-dired--preview-min-width 10
  "Narrowest preview pane, in character columns.")

(defconst etaf-image-dired--grid-min-width 24
  "Columns kept free for the thumbnail grid, including gaps and the divider.")

(defconst etaf-image-dired--divider-width '(ch 1)
  "Width of the drag handle between the grid and the preview pane.")

(defconst etaf-image-dired--keys
  '(;; Navigation
    ("f" etaf-image-dired-forward-image "下一张图片")
    ("b" etaf-image-dired-backward-image "上一张图片")
    ("n" etaf-image-dired-next-line "下一行缩略图")
    ("p" etaf-image-dired-previous-line "上一行缩略图")
    ("a" etaf-image-dired-move-beginning-of-line "移到行首")
    ("e" etaf-image-dired-move-end-of-line "移到行尾")
    ;; Marks and deletion
    ("m" etaf-image-dired-mark-thumb-original-file "标记图片")
    ("u" etaf-image-dired-unmark-thumb-original-file "取消标记")
    ("U" etaf-image-dired-unmark-all-marks "取消全部标记")
    ("d" etaf-image-dired-flag-thumb-original-file "标记为待删除")
    ("<delete>" etaf-image-dired-flag-thumb-original-file "标记为待删除")
    ("x" etaf-image-dired-do-flagged-delete "删除待删除的文件")
    ("C-d" etaf-image-dired-delete-char "从图库移除缩略图")
    ("." etaf-image-dired-track-original-file "在 Dired 中定位")
    ("J" etaf-image-dired-jump-original-dired-buffer "跳转到 Dired")
    ;; Layout
    ("G" etaf-image-dired-line-up-dynamic "每行自动排列")
    ("F" etaf-image-dired-line-up "每行固定张数")
    ("I" etaf-image-dired-line-up-interactive "交互设置每行张数")
    ;; Metadata
    ("t" etaf-image-dired-tag-thumbnail "添加标签")
    ("T" etaf-image-dired-tag-thumbnail-remove "移除标签")
    ("c" etaf-image-dired-comment-thumbnail "编辑注释")
    ("D" etaf-image-dired-thumbnail-set-image-description "设置图片描述")
    ;; Display
    ("RET" etaf-image-dired-display-this "在预览区显示")
    ("SPC" etaf-image-dired-display-next "显示下一张")
    ("DEL" etaf-image-dired-display-previous "显示上一张")
    ("C-<return>" etaf-image-dired-thumbnail-display-external "用外部程序打开")
    ("P" etaf-image-dired-preview "切换预览区")
    ("o" etaf-image-dired-open-at-point "在另一个窗口打开")
    ;; Editing
    ("L" etaf-image-dired-rotate-original-left "向左旋转")
    ("R" etaf-image-dired-rotate-original-right "向右旋转")
    ("W" etaf-image-dired-wallpaper-set "设为壁纸")
    ("S" etaf-image-dired-slideshow-start "开始/停止幻灯片")
    ("w" etaf-image-dired-copy-filename-as-kill "复制文件名")
    ;; Window and gallery
    ("[" etaf-image-dired-narrow-preview "缩窄预览区")
    ("]" etaf-image-dired-widen-preview "加宽预览区")
    ("0" etaf-image-dired-reset-preview-width "预览宽度恢复自动")
    ("g" etaf-image-dired-refresh "重新扫描目录")
    ("C-r" etaf-image-dired-regenerate "重新生成全部缩略图")
    ("q" etaf-image-dired-quit "关闭图库"))
  "Key bindings of the gallery root Host.
Follows `image-dired-thumbnail-mode-map' except where ETAF reserves a key or
forbids a multi-key sequence starting with a printing character: \"g g\",
\"g f\", \"g i\", \"t t\" and \"t r\" become G, F, I, t and T, and TAB
becomes J.")

(defvar etaf-image-dired--pointer-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-movement] #'etaf-image-dired--pointer-motion)
    map)
  "Keymap turning mouse motion into card selection.
It is installed in `minor-mode-overriding-map-alist' so motion reaches the
selection command before Ebox's own hover handler.")

;;; State

(cl-defstruct (etaf-image-dired--session
               (:constructor etaf-image-dired--session--create))
  "Mutable application state for one gallery buffer."
  directory entries-ref focus-ref preview-ref split-ref preview-width-ref
  marks-ref hidden-ref thumbs-per-row-ref
  refs timer buffer frame resize-behavior
  dired-buffer slideshow-timer slideshow-delay)

(defun etaf-image-dired--current-session ()
  "Return the session of the current gallery buffer."
  (let ((session (etaf-app-state)))
    (unless (etaf-image-dired--session-p session)
      (user-error "Not an image gallery buffer"))
    session))

(defun etaf-image-dired--image-files (directory)
  "Return the image files directly inside DIRECTORY, sorted by name."
  (sort (seq-filter
         (lambda (file)
           (and (file-regular-p file)
                (string-match-p (image-file-name-regexp) file)))
         (directory-files directory t directory-files-no-dot-files-regexp))
        #'string-lessp))

(defun etaf-image-dired--thumb-current-p (file thumb)
  "Return non-nil when THUMB exists and is at least as new as FILE."
  (let ((thumb-attributes (file-attributes thumb))
        (file-attributes (file-attributes file)))
    (and thumb-attributes file-attributes
         (not (time-less-p
               (file-attribute-modification-time thumb-attributes)
               (file-attribute-modification-time file-attributes))))))

(defun etaf-image-dired--queue-thumb (file thumb)
  "Ask image-dired to create THUMB for FILE unless that job is queued."
  (unless (member (list file thumb) image-dired-queue)
    (image-dired-create-thumb file thumb)))

(defun etaf-image-dired--frame-metrics (session)
  "Return (CHAR-WIDTH . CHAR-HEIGHT) for SESSION's frame.
Batch and terminal frames report unusable font metrics, so keep a typical
graphical cell size for them; the image Host then falls back to an ASCII
placeholder or a registered converter anyway."
  (let ((frame (etaf-image-dired--session-frame session)))
    (if (frame-live-p frame)
        (let ((width (frame-char-width frame))
              (height (frame-char-height frame)))
          (if (or (< width 4) (< height 8))
              '(8 . 17)
            (cons width height)))
      '(8 . 17))))

(defun etaf-image-dired--window-height (session)
  "Return the number of lines SESSION's gallery may fill.
Inside a render this is the ETAF runtime viewport, which is reactive: the
grid is laid out again whenever the display window or text scale changes.
Outside a render, or before the mount has measured the window, fall back to
the body height of the window showing SESSION's buffer."
  (let ((viewport (ignore-errors (etaf-viewport))))
    (cond
     ((and (consp viewport)
           (integerp (cdr viewport))
           (> (cdr viewport) 0))
      (cdr viewport))
     (t
      (let* ((buffer (etaf-image-dired--session-buffer session))
             (window (or (and (buffer-live-p buffer)
                              (get-buffer-window buffer t))
                         (selected-window))))
        (if (window-live-p window)
            (window-body-height window)
          24))))))

(defconst etaf-image-dired--chrome-rows 6
  "Lines of the window the thumbnail grid does not get.
The gallery root spends one line of padding above and below its content,
and the library spends a header, a gap, a gap and a status line around
the grid.")

(defun etaf-image-dired--grid-height (session)
  "Return the line height of SESSION's thumbnail grid.
The grid is exactly as tall as the window allows, so the buffer itself
never scrolls; the cards scroll inside the grid instead."
  (max 3 (- (etaf-image-dired--window-height session)
            etaf-image-dired--chrome-rows)))

(defun etaf-image-dired--window-columns (session)
  "Return the number of columns in SESSION's display window."
  (let* ((buffer (etaf-image-dired--session-buffer session))
         (window (or (and (buffer-live-p buffer)
                          (get-buffer-window buffer t))
                     (selected-window))))
    (if (window-live-p window)
        (window-body-width window)
      80)))

(defun etaf-image-dired--preview-width-bounds (session)
  "Return the (MIN . MAX) preview pane width of SESSION, in columns.
The maximum leaves `etaf-image-dired--grid-min-width' columns for the grid,
the row gaps and the drag handle."
  (let* ((width (etaf-image-dired--window-columns session))
         (maximum (max etaf-image-dired--preview-min-width
                       (- width etaf-image-dired--grid-min-width))))
    (cons etaf-image-dired--preview-min-width maximum)))

(defun etaf-image-dired--default-preview-width (session)
  "Return SESSION's automatic preview pane width in columns.
Three fifths of the window, unless that starves the thumbnail grid."
  (let ((bounds (etaf-image-dired--preview-width-bounds session)))
    (max (car bounds)
         (min (cdr bounds)
              (floor (* 0.6 (etaf-image-dired--window-columns session)))))))

(defun etaf-image-dired--clamp-preview-width (session width)
  "Return WIDTH clamped to SESSION's preview pane bounds."
  (let ((bounds (etaf-image-dired--preview-width-bounds session)))
    (max (car bounds) (min (cdr bounds) width))))

(defun etaf-image-dired--preview-panel-width (session)
  "Return the width of SESSION's preview pane in columns.
A width stored by the resize commands wins; nil means the automatic
three-fifths default.  The result is clamped to the window, so a stored
width stays usable after the window shrinks."
  (let ((stored (etaf-value (etaf-image-dired--session-preview-width-ref session))))
    (etaf-image-dired--clamp-preview-width
     session
     (or stored (etaf-image-dired--default-preview-width session)))))

(defun etaf-image-dired--preview-width-delta (gesture)
  "Convert GESTURE's horizontal pixel movement to character columns."
  (let* ((window (plist-get gesture :window))
         (char-width (if (window-live-p window)
                         (frame-char-width (window-frame window))
                       8))
         (dx (car (plist-get gesture :delta))))
    (cons (round (/ (float dx) (max 1 char-width))) 0)))

(defun etaf-image-dired--make-resize-behavior (session)
  "Return SESSION's retained drag Behavior for the splitter.
Constructing it once per session matters: a render-created handler is a
new identity and cannot carry an active drag across a re-render."
  (etaf-resizable-controlled
   (lambda ()
     (list :x 0 :y 0
           :width (etaf-image-dired--preview-panel-width session)
           :height 1))
   (lambda (next)
     (etaf-set-value (etaf-image-dired--session-preview-width-ref session)
                     (etaf-image-dired--clamp-preview-width
                      session (round (plist-get next :width)))))
   :edges '(left)
   :min-width etaf-image-dired--preview-min-width
   :delta #'etaf-image-dired--preview-width-delta))

(defun etaf-image-dired--thumb-box (session)
  "Return the uniform (COLUMNS . ROWS) box every thumbnail is shown in.
The box is as large as an image-dired thumbnail converted to character
cells, so every card has the same width and height.  Pictures are scaled
to fit inside the box and the leftover cells stay blank."
  (let* ((metrics (etaf-image-dired--frame-metrics session))
         (pixels (if (boundp 'image-dired-thumb-size)
                     image-dired-thumb-size
                   128)))
    (cons (max 2 (round (/ (float pixels) (car metrics))))
          (max 2 (round (/ (float pixels) (cdr metrics)))))))

(defun etaf-image-dired--thumb-layout (session thumb)
  "Return the display layout of THUMB inside its uniform box.
The value is a plist with :columns and :rows (the character cells the image
block occupies), :inset (PIXELS-ABOVE . PIXELS-BELOW) that centers the
picture inside the block without stretching it, and :pad-top (whole blank
lines above the block).  The picture keeps its aspect ratio; the box cells
left over stay blank."
  (let* ((box (etaf-image-dired--thumb-box session))
         (frame (etaf-image-dired--session-frame session))
         (metrics (etaf-image-dired--frame-metrics session))
         (char-width (car metrics))
         (char-height (cdr metrics))
         (descriptor (ignore-errors (create-image thumb)))
         (pixels (and descriptor
                      (ignore-errors (image-size descriptor t frame)))))
    (if (not (and (consp pixels)
                  (> (car pixels) 0)
                  (> (cdr pixels) 0)))
        (list :columns (car box) :rows (cdr box)
              :inset '(0 . 0) :pad-top 0)
      (let* ((box-width (* (car box) char-width))
             (box-height (* (cdr box) char-height))
             (width (car pixels))
             (height (cdr pixels))
             (scale (min (/ (float box-width) width)
                         (/ (float box-height) height)))
             (columns (max 1 (min (car box)
                                  (round (/ (* width scale) char-width)))))
             (display-height (max 1 (round (* columns char-width
                                              (/ (float height) width))))))
        ;; Rounding the width up can make the picture one pixel too tall.
        (while (and (> columns 1) (> display-height box-height))
          (setq columns (1- columns)
                display-height (max 1 (round (* columns char-width
                                                 (/ (float height) width))))))
        (let* ((min-rows (max 1 (/ (+ display-height char-height -1)
                                   char-height)))
               (max-rows (min (cdr box)
                              (/ (+ display-height (* 2 (1- char-height)))
                                 char-height)))
               (best-rows min-rows)
               (best-inset '(0 . 0))
               (best-pad-top 0)
               (best-diff most-positive-fixnum))
          ;; A taller image block can leave room for pixel insets, which
          ;; center the picture more precisely than whole blank lines.
          (cl-loop for rows from min-rows to max-rows do
                   (let* ((inset-total (- (* rows char-height) display-height))
                          (spare (- (cdr box) rows))
                          (base-top (* (/ spare 2) char-height))
                          (base-bottom (* (- spare (/ spare 2)) char-height))
                          (low (max 0 (- inset-total (1- char-height))))
                          (high (min inset-total (1- char-height)))
                          (top-inset (max low (min high
                                                   (round (/ (+ (- base-bottom base-top)
                                                                inset-total)
                                                             2.0)))))
                          (diff (abs (- (+ base-top top-inset)
                                        (+ base-bottom (- inset-total top-inset))))))
                     (when (< diff best-diff)
                       (setq best-diff diff
                             best-rows rows
                             best-inset (cons top-inset
                                              (- inset-total top-inset))
                             best-pad-top (/ spare 2)))))
          (list :columns columns :rows best-rows
                :inset best-inset :pad-top best-pad-top))))))

(defun etaf-image-dired--entry-ready (session entry)
  "Return ENTRY marked ready, with the display size of its thumbnail.
ENTRY is copied first: entries inside a ref must never be mutated in place,
or the reactive layer cannot tell the old and the new value apart."
  (let* ((entry (copy-sequence entry))
         (thumb (plist-get entry :thumb))
         (attributes (file-attributes thumb))
         (layout (etaf-image-dired--thumb-layout session thumb)))
    (setf entry (plist-put entry :state 'ready)
          entry (plist-put entry :queued-at nil)
          entry (plist-put entry :thumb-mtime
                           (and attributes
                                (file-attribute-modification-time attributes)))
          entry (plist-put entry :columns (plist-get layout :columns))
          entry (plist-put entry :rows (plist-get layout :rows))
          entry (plist-put entry :inset (plist-get layout :inset))
          entry (plist-put entry :pad-top (plist-get layout :pad-top)))
    entry))

(defun etaf-image-dired--entry (session file)
  "Return the gallery entry for FILE, queueing a thumbnail when needed."
  (let* ((thumb (image-dired-thumb-name file))
         (thumb-exists (file-exists-p thumb))
         (entry (list :file file
                      :thumb thumb
                      :ref (intern (concat "etaf-image-dired-card-" (sha1 file)))
                      :name (file-name-nondirectory file))))
    (cond
     ((etaf-image-dired--thumb-current-p file thumb)
      (etaf-image-dired--entry-ready session entry))
     (thumb-exists
      ;; Keep showing the old thumbnail while image-dired refreshes it.
      (etaf-image-dired--queue-thumb file thumb)
      (plist-put (etaf-image-dired--entry-ready session entry)
                 :queued-at (float-time)))
     (t
      (etaf-image-dired--queue-thumb file thumb)
      (let ((box (etaf-image-dired--thumb-box session)))
        (append entry
                (list :state 'pending
                      :columns (car box)
                      :rows (cdr box)
                      :inset '(0 . 0)
                      :pad-top 0
                      :queued-at (float-time))))))))

(defun etaf-image-dired--job-finished-p (entry)
  "Return non-nil when ENTRY's thumbnail job is no longer running."
  (let ((queued-at (plist-get entry :queued-at))
        (file (plist-get entry :file))
        (thumb (plist-get entry :thumb)))
    (and queued-at
         (not (member (list file thumb) image-dired-queue))
         (null image-dired-queue)
         (zerop image-dired-queue-active-jobs)
         (> (float-time (time-since queued-at)) 1.0))))

(defun etaf-image-dired--start-timer (session)
  "Start SESSION's thumbnail polling timer when it is not running."
  (unless (timerp (etaf-image-dired--session-timer session))
    (setf (etaf-image-dired--session-timer session)
          (run-at-time etaf-image-dired-refresh-interval
                       etaf-image-dired-refresh-interval
                       (lambda () (etaf-image-dired--tick session))))))

(defun etaf-image-dired--stop-timer (session)
  "Cancel SESSION's thumbnail polling timer."
  (when (timerp (etaf-image-dired--session-timer session))
    (cancel-timer (etaf-image-dired--session-timer session)))
  (setf (etaf-image-dired--session-timer session) nil))

(defun etaf-image-dired--tick (session)
  "Refresh thumbnail states owned by SESSION after image-dired runs."
  (let ((buffer (etaf-image-dired--session-buffer session)))
    (if (not (and (buffer-live-p buffer)
                  (etaf-runtime-for-buffer buffer)))
        (etaf-image-dired--stop-timer session)
      (let ((entries (etaf-value (etaf-image-dired--session-entries-ref session)))
            (changed nil)
            (pending nil)
            result)
        (dolist (entry entries)
          (let* ((file (plist-get entry :file))
                 (thumb (plist-get entry :thumb))
                 (current (etaf-image-dired--thumb-current-p file thumb))
                 (attributes (and current (file-attributes thumb)))
                 (mtime (and attributes
                             (file-attribute-modification-time attributes)))
                 (state (plist-get entry :state)))
            (cond
             ((eq state 'pending)
              (cond
               (mtime
                (setq changed t)
                (push (etaf-image-dired--entry-ready session entry) result))
               ((etaf-image-dired--job-finished-p entry)
                (setq changed t)
                (push (plist-put (copy-sequence entry) :state 'failed) result))
               (t
                (setq pending t)
                (push entry result))))
             ((eq state 'ready)
              (cond
               ;; A regenerated thumbnail replaced the old one.
               ((and mtime (not (equal mtime (plist-get entry :thumb-mtime))))
                (setq changed t)
                (push (etaf-image-dired--entry-ready session entry) result))
               ;; A stale thumbnail is being regenerated in the background.
               ((and (not current) (plist-get entry :queued-at))
                (cond
                 ;; The regenerated thumbnail replaced the old one.
                 ((etaf-image-dired--thumb-current-p file thumb)
                  (setq changed t)
                  (push (etaf-image-dired--entry-ready session entry) result))
                 ;; image-dired gave up; keep the old picture but stop waiting.
                 ((etaf-image-dired--job-finished-p entry)
                  (setq changed t)
                  (push (plist-put (copy-sequence entry) :queued-at nil)
                        result))
                 (t
                  (setq pending t)
                  (push entry result))))
               (t
                (push entry result))))
             (t
              (push entry result)))))
        (setq result (nreverse result))
        (when changed
          (etaf-set-value (etaf-image-dired--session-entries-ref session) result))
        (unless pending
          (etaf-image-dired--stop-timer session))))))

(defun etaf-image-dired--hidden-files (session)
  "Return the files removed from SESSION's gallery with `C-d'."
  (let ((ref (etaf-image-dired--session-hidden-ref session)))
    (and ref (etaf-value ref))))

(defun etaf-image-dired--visible-files (session)
  "Return the image files of SESSION that are still in the gallery."
  (let ((hidden (etaf-image-dired--hidden-files session)))
    (seq-remove (lambda (file) (member file hidden))
                (etaf-image-dired--image-files
                 (etaf-image-dired--session-directory session)))))

(defun etaf-image-dired--refresh (session)
  "Rescan SESSION's directory and start creating missing thumbnails.
Files removed from the gallery with `C-d' stay removed; marks that refer to
files which no longer exist are dropped."
  (let* ((files (etaf-image-dired--visible-files session))
         (entries (mapcar (lambda (file) (etaf-image-dired--entry session file))
                          files))
         (refs (make-hash-table :test #'eq))
         (present (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (puthash (plist-get entry :ref) (plist-get entry :file) refs))
    (dolist (file files)
      (puthash file t present))
    (setf (etaf-image-dired--session-refs session) refs)
    (when-let* ((marks-ref (etaf-image-dired--session-marks-ref session)))
      (etaf-set-value marks-ref
                      (seq-filter (lambda (pair) (gethash (car pair) present))
                                  (etaf-value marks-ref))))
    (etaf-set-value (etaf-image-dired--session-entries-ref session) entries)
    (if (seq-some (lambda (entry)
                    (or (eq (plist-get entry :state) 'pending)
                        (plist-get entry :queued-at)))
                  entries)
        (etaf-image-dired--start-timer session)
      (etaf-image-dired--stop-timer session))))

;;; Marks, files and navigation

(defun etaf-image-dired--files (session)
  "Return the files shown in SESSION, in display order."
  (mapcar (lambda (entry) (plist-get entry :file))
          (etaf-value (etaf-image-dired--session-entries-ref session))))

(defun etaf-image-dired--marks (session)
  "Return SESSION's mark alist, mapping files to `marked' or `flagged'."
  (let ((ref (etaf-image-dired--session-marks-ref session)))
    (and ref (etaf-value ref))))

(defun etaf-image-dired--file-mark (session file)
  "Return SESSION's mark for FILE: `marked', `flagged', or nil."
  (cdr (assoc file (etaf-image-dired--marks session))))

(defun etaf-image-dired--set-marks (session marks)
  "Make MARKS the mark alist of SESSION."
  (when-let* ((ref (etaf-image-dired--session-marks-ref session)))
    (etaf-set-value ref marks)))

(defun etaf-image-dired--set-file-mark (session file mark)
  "Set SESSION's MARK of FILE, replacing an existing mark."
  (let ((marks (assoc-delete-all
                file (copy-sequence (etaf-image-dired--marks session)))))
    (when mark
      (push (cons file mark) marks))
    (etaf-image-dired--set-marks session marks)))

(defun etaf-image-dired--marked-files (session)
  "Return the files marked with `m' in SESSION, in display order."
  (seq-filter (lambda (file)
                (eq (etaf-image-dired--file-mark session file) 'marked))
              (etaf-image-dired--files session)))

(defun etaf-image-dired--flagged-files (session)
  "Return the files flagged for deletion in SESSION, in display order."
  (seq-filter (lambda (file)
                (eq (etaf-image-dired--file-mark session file) 'flagged))
              (etaf-image-dired--files session)))

;;;; Dired-backed marks

;; The gallery does not keep marks of its own: the Dired buffer visiting the
;; same directory is the source of truth.  Commands here write through Dired,
;; and the hooks below copy changes made in Dired back into every gallery
;; showing that directory.

(defun etaf-image-dired--directory-name (session)
  "Return SESSION's directory as an absolute, trailing-slash name."
  (file-name-as-directory
   (expand-file-name (etaf-image-dired--session-directory session))))

(defun etaf-image-dired--dired-buffer (session)
  "Return a Dired buffer visiting SESSION's directory, or nil.
The buffer cached in SESSION is preferred, so later commands keep acting on
the same Dired buffer even when several visit the directory."
  (let ((cached (etaf-image-dired--session-dired-buffer session))
        (directory (etaf-image-dired--directory-name session)))
    (if (and (buffer-live-p cached)
             (with-current-buffer cached
               (and (derived-mode-p 'dired-mode)
                    (equal (file-name-as-directory
                            (expand-file-name default-directory))
                           directory))))
        cached
      (setf (etaf-image-dired--session-dired-buffer session) nil)
      (when-let* ((found (seq-find
                          (lambda (buffer)
                            (with-current-buffer buffer
                              (and (derived-mode-p 'dired-mode)
                                   (equal
                                    (file-name-as-directory
                                     (expand-file-name default-directory))
                                    directory))))
                          (buffer-list))))
        (setf (etaf-image-dired--session-dired-buffer session) found)
        found))))

(defun etaf-image-dired--ensure-dired-buffer (session)
  "Return SESSION's Dired buffer, creating one when none exists."
  (or (etaf-image-dired--dired-buffer session)
      (let ((buffer (dired-noselect
                     (etaf-image-dired--directory-name session))))
        (setf (etaf-image-dired--session-dired-buffer session) buffer)
        (with-current-buffer buffer
          (etaf-image-dired--dired-mode-setup))
        buffer)))

(defun etaf-image-dired--sync-marks-from-dired (session)
  "Copy the Dired marks of SESSION's directory into the gallery."
  (when-let* ((buffer (etaf-image-dired--dired-buffer session)))
    (let (marks)
      (with-current-buffer buffer
        (save-excursion
          (dolist (file (etaf-image-dired--files session))
            (when (dired-goto-file file)
              (cond
               ((image-dired-dired-file-flagged-p)
                (push (cons file 'flagged) marks))
               ((image-dired-dired-file-marked-p)
                (push (cons file 'marked) marks)))))))
      (setq marks (nreverse marks))
      (unless (equal marks (etaf-image-dired--marks session))
        (etaf-image-dired--set-marks session marks)))))

(defun etaf-image-dired--apply-dired-mark (session file mark)
  "Set MARK of FILE in SESSION through the directory's Dired buffer.
MARK is `marked', `flagged', or nil to unmark.  The gallery then re-reads
Dired, so every viewer of the directory sees the same marks."
  (let ((buffer (etaf-image-dired--ensure-dired-buffer session)))
    (with-current-buffer buffer
      (save-excursion
        (unless (dired-goto-file file)
          (user-error "Dired 中找不到该文件"))
        ;; The mark is changed outside Dired's command loop, so Emacs has
        ;; not inserted an undo boundary.  Delimit the change explicitly:
        ;; otherwise `dired-undo' in the Dired buffer either cannot find the
        ;; mark or reverts a whole run of gallery marks at once.
        (undo-boundary)
        (pcase mark
          ('marked (dired-mark 1))
          ('flagged (dired-flag-file-deletion 1))
          (_ (dired-unmark 1)))
        (undo-boundary)))
    (etaf-image-dired--sync-marks-from-dired session)))

(defun etaf-image-dired--ref-for-file (session file)
  "Return the ref of the card showing FILE in SESSION, or nil."
  (when-let* ((entries (etaf-value
                        (etaf-image-dired--session-entries-ref session))))
    (plist-get (seq-find (lambda (entry)
                           (equal file (plist-get entry :file)))
                         entries)
               :ref)))

(defun etaf-image-dired--track-in-dired (session file)
  "Move the point of SESSION's visible Dired window to FILE.
This mirrors `image-dired-track-movement': moving through the gallery keeps
the Dired buffer on the same image."
  (when (and image-dired-track-movement file)
    (when-let* ((buffer (etaf-image-dired--dired-buffer session)))
      (with-current-buffer buffer
        (save-excursion
          (when (dired-goto-file file)
            (when-let* ((window (get-buffer-window buffer t)))
              (set-window-point window (point)))))))))

(defun etaf-image-dired--dired-flagged-p ()
  "Return non-nil when any line of the current Dired buffer is flagged."
  (let ((dired-marker-char dired-del-marker))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (dired-marker-regexp) nil t))))

(defun etaf-image-dired--sync-dired-buffer ()
  "Reflect the current Dired buffer into matching galleries.
Called from Dired's `post-command-hook' and `dired-after-readin-hook': marks
are re-read, and, when `image-dired-track-movement' is non-nil, the gallery
selection follows Dired's point."
  (let ((directory (file-name-as-directory
                    (expand-file-name default-directory)))
        (source (current-buffer))
        (file (ignore-errors (dired-get-filename nil t))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (let ((session (ignore-errors (etaf-app-state))))
          (when (and (etaf-image-dired--session-p session)
                     (equal (etaf-image-dired--directory-name session)
                            directory))
            ;; The Dired buffer that triggered the sync becomes the one the
            ;; gallery writes through.
            (setf (etaf-image-dired--session-dired-buffer session) source)
            (etaf-image-dired--sync-marks-from-dired session)
            (when (and image-dired-track-movement file)
              (when-let* ((ref (etaf-image-dired--ref-for-file session file)))
                (unless (eq ref (etaf-value
                                 (etaf-image-dired--session-focus-ref
                                  session)))
                  (etaf-image-dired--set-selection session ref))))))))))

(defun etaf-image-dired--dired-mode-setup ()
  "Keep galleries in sync with the current Dired buffer."
  (add-hook 'post-command-hook #'etaf-image-dired--sync-dired-buffer nil t)
  (add-hook 'dired-after-readin-hook
            #'etaf-image-dired--sync-dired-buffer nil t))

(add-hook 'dired-mode-hook #'etaf-image-dired--dired-mode-setup)

(defun etaf-image-dired--file-at-point (session)
  "Return the file of the card painted at point in SESSION, or nil."
  (let ((runtime (ignore-errors (etaf-runtime-for-buffer (current-buffer)))))
    (when runtime
      (when-let* ((ref (ignore-errors (etaf-host-ref-at runtime (point)))))
        (gethash ref (etaf-image-dired--session-refs session))))))

(defun etaf-image-dired--current-file (session)
  "Return the file the next command should act on in SESSION.
The card under the mouse pointer, the focused card, the card at point, and
finally the first card are tried in that order."
  (or (etaf-image-dired--selection-file session)
      (etaf-image-dired--focused-file session)
      (etaf-image-dired--file-at-point session)
      (car (etaf-image-dired--files session))))

(defun etaf-image-dired--current-index (session)
  "Return the index in SESSION's grid of the current file, or nil."
  (when-let* ((file (etaf-image-dired--current-file session)))
    (seq-position (etaf-image-dired--files session) file #'equal)))

(defun etaf-image-dired--target-files (session)
  "Return the files a metadata or editing command should act on.
Marked files win; otherwise the current file is used."
  (or (etaf-image-dired--marked-files session)
      (let ((file (etaf-image-dired--current-file session)))
        (unless file
          (user-error "没有选中的图片"))
        (list file))))

(defun etaf-image-dired--focus-index (session index)
  "Focus the thumbnail at INDEX in SESSION and select it.
Returns the bounded index.  Outside a live ETAF runtime only the selection
is updated, which keeps the movement commands testable in batch."
  (let* ((entries (etaf-value (etaf-image-dired--session-entries-ref session)))
         (count (length entries)))
    (when (zerop count)
      (user-error "当前目录没有图片"))
    (let* ((index (max 0 (min (1- count) index)))
           (ref (plist-get (nth index entries) :ref))
           (runtime (ignore-errors (etaf-runtime-for-buffer (current-buffer)))))
      (when runtime
        (condition-case nil
            (progn
              (etaf-focus runtime ref)
              (ignore-errors (etaf-scroll-into-view runtime ref)))
          (error nil)))
      ;; `etaf-focus' does not emit `on-focus-enter' when the target is
      ;; already focused, and batch runs have no runtime at all.
      (etaf-image-dired--set-selection session ref)
      index)))

(defun etaf-image-dired--navigate-to (session target &optional boundary-message)
  "Move SESSION's selection to TARGET, clamped to the grid.
When TARGET lies outside the grid and BOUNDARY-MESSAGE is non-nil, show it."
  (let* ((files (etaf-image-dired--files session))
         (count (length files)))
    (when (zerop count)
      (user-error "当前目录没有图片"))
    (let ((bounded (max 0 (min (1- count) target))))
      (etaf-image-dired--focus-index session bounded)
      (when (and boundary-message (/= target bounded))
        (message "%s" boundary-message))
      bounded)))

(defun etaf-image-dired--grid-columns (session)
  "Return the number of thumbnail columns SESSION currently shows.
A fixed line-up wins; otherwise estimate the wrapping of the flex grid from
the window width, the preview pane and the card size."
  (or (let ((ref (etaf-image-dired--session-thumbs-per-row-ref session)))
        (and ref (etaf-value ref)))
      (let* ((metrics (etaf-image-dired--frame-metrics session))
             (char-width (max 1 (car metrics)))
             (box (etaf-image-dired--thumb-box session))
             (card-width (+ (* (car box) char-width) 2))
             (gap 16)
             (split (etaf-value (etaf-image-dired--session-split-ref session)))
             (panel-pixels (if split
                               (* (etaf-image-dired--preview-panel-width session)
                                  char-width)
                             0))
             ;; The root pads 14px on each side, and the splitter plus the
             ;; column gap around it take three character cells.
             (reserved (+ 28 panel-pixels (if split (* 3 char-width) 0)))
             (window-pixels (* (etaf-image-dired--window-columns session)
                               char-width))
             (available (max card-width (- window-pixels reserved))))
        (max 1 (floor (/ (+ available gap) (+ card-width gap)))))))

(defun etaf-image-dired--grid-pixel-width (session columns)
  "Return the pixel width that fits exactly COLUMNS cards of SESSION.
Cards are drawn with a 1px frame on each side and separated by a 16px gap."
  (let* ((metrics (etaf-image-dired--frame-metrics session))
         (char-width (max 1 (car metrics)))
         (box (etaf-image-dired--thumb-box session))
         (card-width (+ (* (car box) char-width) 2)))
    (+ (* columns card-width)
       (* (max 0 (1- columns)) 16))))

;;; Selection

(defun etaf-image-dired--selection-file (session)
  "Return the file of the card selected in SESSION, or nil."
  (when-let* ((ref (etaf-value (etaf-image-dired--session-focus-ref session))))
    (gethash ref (etaf-image-dired--session-refs session))))

(defun etaf-image-dired--focused-file (session)
  "Return the file of the card holding keyboard focus in SESSION, or nil."
  (let* ((buffer (etaf-image-dired--session-buffer session))
         (runtime (and (buffer-live-p buffer)
                       (etaf-runtime-for-buffer buffer))))
    (when-let* ((ref (and runtime (etaf-focused-host-ref runtime))))
      (gethash ref (etaf-image-dired--session-refs session)))))

(defun etaf-image-dired--set-selection (session ref)
  "Make REF the selected card in SESSION.
An open preview follows the new selection, so hovering another thumbnail
replaces the picture on the right."
  (etaf-set-value (etaf-image-dired--session-focus-ref session) ref)
  (when (etaf-value (etaf-image-dired--session-split-ref session))
    (etaf-set-value (etaf-image-dired--session-preview-ref session)
                    (and ref
                         (gethash ref (etaf-image-dired--session-refs session)))))
  (when ref
    (when-let* ((file (gethash ref (etaf-image-dired--session-refs session))))
      (etaf-image-dired--track-in-dired session file))))

(defun etaf-image-dired--select-at-position (session position)
  "Select the card painted at buffer POSITION in SESSION.
POSITION outside every card clears the selection, unless the preview pane is
open: then the last card stays selected while the pointer crosses it."
  (let* ((buffer (etaf-image-dired--session-buffer session))
         (runtime (and (buffer-live-p buffer)
                       (etaf-runtime-for-buffer buffer)))
         (refs (etaf-image-dired--session-refs session))
         (ref (and runtime
                   (etaf-host-ref-at
                    runtime position
                    (lambda (ref) (gethash ref refs))))))
    (when (and runtime
               (or ref
                   (not (etaf-value (etaf-image-dired--session-split-ref session)))))
      (etaf-image-dired--set-selection session ref))))

(defun etaf-image-dired--pointer-motion (event)
  "Select the card under the mouse pointer described by EVENT."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position))
         (point (posn-point position)))
    (when (and (window-live-p window)
               (eq (window-buffer window) (current-buffer))
               (null (posn-area position))
               (integer-or-marker-p point))
      (let ((session (etaf-app-state)))
        (when (etaf-image-dired--session-p session)
          (etaf-image-dired--select-at-position session point))))))

(defun etaf-image-dired--clear-selection ()
  "Clear the card selected through the mouse pointer.
The selection survives leaving the buffer while the preview pane is open, so
the preview keeps showing the last selected image."
  (let ((session (etaf-app-state)))
    (when (and (etaf-image-dired--session-p session)
               (not (etaf-value (etaf-image-dired--session-split-ref session))))
      (etaf-image-dired--set-selection session nil))))

(defun etaf-image-dired--install-pointer-tracking (session buffer)
  "Make BUFFER report mouse motion to SESSION's selection.
`track-mouse' asks Emacs for motion events even without a pressed button;
the overriding key map routes them to `etaf-image-dired--pointer-motion'."
  (with-current-buffer buffer
    (setq-local track-mouse t)
    (setq-local minor-mode-overriding-map-alist
                (cons (cons 'etaf-image-dired--pointer-map
                            etaf-image-dired--pointer-map)
                      minor-mode-overriding-map-alist))
    (add-hook 'mouse-leave-buffer-hook
              (lambda ()
                (when (eq (etaf-app-state) session)
                  (etaf-image-dired--clear-selection)))
              nil t)))

(defun etaf-image-dired--disable-current-line-highlight (buffer)
  "Turn off current-line highlighting in gallery BUFFER.
ETAF moves point while laying out and focusing Hosts, so an `hl-line'
overlay would highlight an arbitrary line and compete with the card
selection frame.  `hl-line' documents a buffer-local nil value of
`global-hl-line-mode' as the way to exclude one buffer from the global
mode; local `hl-line-mode' is switched off as well."
  (with-current-buffer buffer
    (when (boundp 'global-hl-line-mode)
      (setq-local global-hl-line-mode nil))
    (when (fboundp 'global-hl-line-unhighlight)
      (global-hl-line-unhighlight))
    (when (fboundp 'hl-line-unhighlight)
      (hl-line-unhighlight))
    (when (and (fboundp 'hl-line-mode)
               (bound-and-true-p hl-line-mode))
      (hl-line-mode -1))))

;;; View

(defun etaf-image-dired--card-selected-p (session entry)
  "Return non-nil when ENTRY's card is the selected one in SESSION."
  (eq (etaf-value (etaf-image-dired--session-focus-ref session))
      (plist-get entry :ref)))

(etaf-define-component etaf-image-dired--card (&key entry session)
  "Render one thumbnail card for ENTRY.
Every card occupies the same box: the picture keeps its aspect ratio and is
centered in it, with blank rows above and below.  The card's frame follows
SESSION's selection ref, so the card under the mouse or holding keyboard
focus is framed.  RET opens the card's image even when the card itself holds
focus, because activation would otherwise win."
  :render
  (let* ((box (etaf-image-dired--thumb-box session))
         (box-columns (car box))
         (box-rows (cdr box))
         (rows (or (plist-get entry :rows) box-rows))
         (pad-top (or (plist-get entry :pad-top) 0))
         (pad-bottom (max 0 (- box-rows rows pad-top)))
         (file (plist-get entry :file))
         (mark (etaf-image-dired--file-mark session file))
         (border-color
          (cond ((etaf-image-dired--card-selected-p session entry)
                 etaf-image-dired--selection-color)
                ((eq mark 'flagged) etaf-image-dired--flag-color)
                ((eq mark 'marked) etaf-image-dired--mark-color)
                (t etaf-image-dired--border-color))))
    (etaf-view
     (column :ref (plist-get entry :ref)
             :width (list 'ch box-columns)
             :height (list 'lh (1+ box-rows))
             :box-sizing 'content-box
             :cross-align 'center
             :border (list etaf-image-dired--selection-width 'solid
                           border-color)
             :item-gap '(lh 0)
             :role 'button
             :tab-index 0
             :keys '(("RET" etaf-image-dired-display-this "在预览区显示"))
             :help-echo (plist-get entry :file)
             :on-press (lambda () (etaf-image-dired--open-file (plist-get entry :file)))
             :on-focus-enter
             (lambda ()
               (etaf-image-dired--set-selection session (plist-get entry :ref)))
             :on-focus-leave
             (lambda ()
               (let ((focus-ref (etaf-image-dired--session-focus-ref session)))
                 (when (eq (etaf-value focus-ref) (plist-get entry :ref))
                   (etaf-set-value focus-ref nil))))
       (box :if (> pad-top 0)
            :width (list 'ch box-columns)
            :height (list 'lh pad-top))
       (image :if (eq (plist-get entry :state) 'ready)
              :src (plist-get entry :thumb)
              :columns (plist-get entry :columns)
              :rows (plist-get entry :rows)
              :inset (plist-get entry :inset))
       (box :else-if (eq (plist-get entry :state) 'failed)
            :width (list 'ch box-columns)
            :height (list 'lh box-rows)
            :text-align 'center
         (text :color etaf-image-dired--error-color "生成失败"))
       (box :else t
            :width (list 'ch box-columns)
            :height (list 'lh box-rows)
            :text-align 'center
         (text :color etaf-image-dired--muted-color "生成中…"))
       (box :if (> pad-bottom 0)
            :width (list 'ch box-columns)
            :height (list 'lh pad-bottom))
       (box :width (list 'ch box-columns)
            :height '(lh 1)
            :overflow 'hidden
            :text-align 'center
         (text :if (eq mark 'flagged)
               :font-size 14
               :color etaf-image-dired--flag-color
               (expr (concat "D " (plist-get entry :name))))
         (text :else-if (eq mark 'marked)
               :font-size 14
               :color etaf-image-dired--mark-color
               (expr (concat "* " (plist-get entry :name))))
         (text :else t
               :font-size 14
               (expr (plist-get entry :name))))))))

(etaf-define-component etaf-image-dired--library (&key session)
  "Render SESSION's header, thumbnail grid and status line.
It grows to fill whatever width the surrounding row leaves for it, so the
grid rewraps when the preview pane opens."
  :render
  (let* ((session (etaf-current-prop 'session))
         (entries (etaf-value (etaf-image-dired--session-entries-ref session)))
         (total (length entries))
         (ready (seq-count (lambda (entry)
                             (eq (plist-get entry :state) 'ready))
                           entries))
         (failed (seq-count (lambda (entry)
                              (eq (plist-get entry :state) 'failed))
                            entries))
         (pending (- total ready failed))
         (grid-height (etaf-image-dired--grid-height session))
         (directory (etaf-image-dired--session-directory session))
         (columns-ref (etaf-image-dired--session-thumbs-per-row-ref session))
         (columns (and columns-ref (etaf-value columns-ref)))
         (grid-width (and columns
                          (etaf-image-dired--grid-pixel-width session columns)))
         (marks (etaf-image-dired--marks session))
         (marked (seq-count (lambda (pair) (eq (cdr pair) 'marked)) marks))
         (flagged (seq-count (lambda (pair) (eq (cdr pair) 'flagged)) marks))
         (mark-summary
          (cond ((and (> marked 0) (> flagged 0))
                 (format " · 已标记 %d，待删除 %d" marked flagged))
                ((> marked 0) (format " · 已标记 %d" marked))
                ((> flagged 0) (format " · 待删除 %d" flagged))
                (t ""))))
    (etaf-view
     (column :flex-grow 1
             :flex-basis '(px 0)
             :flex-shrink 1
             :min-width '(px 0)
             :overflow 'hidden
             :item-gap '(lh 1)
       (row :width 'stretch
            :cross-align 'center
            :item-gap '(ch 1)
         (text :font-size 18
               :font-weight 'bold
               (expr (abbreviate-file-name directory)))
         (text :font-size 14
               :color etaf-image-dired--muted-color
               (expr (format "%d 张图片" total))))
       (box :if (null entries)
            :width 'stretch
            :padding '((lh 1) (px 4))
         (text :color etaf-image-dired--muted-color
               "当前目录没有图片。按 g 重新扫描。"))
       (flex :else t
             :flex-wrap 'wrap
             :width (if grid-width (list 'px grid-width) 'stretch)
             :flex-shrink (if grid-width 0 1)
             :height (list 'lh grid-height)
             :overflow 'scroll
             :gap etaf-image-dired--card-gap
             :align-items 'start
         (etaf-image-dired--card :for (entry entries)
                              :key (plist-get entry :file)
                              :session session
                              :entry entry))
       (text :font-size 13
             :color etaf-image-dired--muted-color
             (expr (format "%d 已生成 · %d 生成中 · %d 失败%s · RET 预览 · SPC 下一张 · m 标记 · d 待删 · x 删除 · q 关闭"
                           ready pending failed mark-summary)))))))

;;;; Preview

(defun etaf-image-dired--preview-area (session)
  "Return the (COLUMNS . ROWS) SESSION's preview pane may occupy.
The width follows the stored split, or three fifths of the window when
nothing was stored, and always leaves room for the thumbnail grid to wrap."
  ;; The pane shares the window with the grid: one line of root padding
  ;; above, one below, then the filename and its gap.
  (cons (etaf-image-dired--preview-panel-width session)
        (max 4 (- (etaf-image-dired--window-height session) 4))))

(defun etaf-image-dired--preview-cells (session file)
  "Return the (COLUMNS . ROWS) that show FILE inside SESSION's preview pane.
The picture is scaled down to fit the pane and keeps its aspect ratio; small
pictures are shown at their natural size instead of being enlarged."
  (let ((area (etaf-image-dired--preview-area session)))
    (if (not file)
        (cons (car area) (min 6 (cdr area)))
      (let* ((metrics (etaf-image-dired--frame-metrics session))
             (frame (etaf-image-dired--session-frame session))
             (descriptor (ignore-errors (create-image file)))
             (pixels (and descriptor
                          (ignore-errors (image-size descriptor t frame)))))
        (if (not (and (consp pixels)
                      (> (car pixels) 0)
                      (> (cdr pixels) 0)))
            (cons (car area) (min 6 (cdr area)))
          (let* ((max-width (* (car area) (car metrics)))
                 (max-height (* (cdr area) (cdr metrics)))
                 (scale (min 1.0
                             (/ (float max-width) (car pixels))
                             (/ (float max-height) (cdr pixels)))))
            (cons (max 1 (round (/ (* (car pixels) scale)
                                   (float (car metrics)))))
                  (max 1 (round (/ (* (cdr pixels) scale)
                                   (float (cdr metrics))))))))))))

(etaf-define-component etaf-image-dired--preview (&key session)
  "Render SESSION's preview pane for the currently selected image.
The pane is part of the gallery buffer, so opening it does not create a new
Emacs window.  Its width stays fixed while the selection moves, so the
thumbnail grid on the left does not reflow under the mouse pointer."
  :render
  (let* ((session (etaf-current-prop 'session))
         (file (etaf-value (etaf-image-dired--session-preview-ref session)))
         (panel (car (etaf-image-dired--preview-area session)))
         (cells (etaf-image-dired--preview-cells session file))
         (columns (car cells))
         (rows (cdr cells)))
    (etaf-view
     (column :width (list 'ch panel)
             :item-gap '(lh 1)
             :cross-align 'center
       (image :if file
              :src file
              :columns columns
              :rows rows)
       (box :else t
            :width (list 'ch panel)
            :height (list 'lh (max 3 (- rows 1)))
            :text-align 'center
         (text :color etaf-image-dired--muted-color
               "没有选中的图片"))
       (box :width (list 'ch panel)
            :height '(lh 1)
            :overflow 'hidden
            :text-align 'center
         (text :font-size 14
               (expr (or (and file (file-name-nondirectory file)) ""))))))))

(etaf-define-component etaf-image-dired--splitter (&key session)
  "Render the drag handle between the thumbnail grid and the preview pane.
Dragging it changes both widths; [ and ] adjust it from the keyboard."
  :render
  (let ((session (etaf-current-prop 'session)))
    (etaf-view
     (box :ref 'etaf-image-dired-divider
          :width etaf-image-dired--divider-width
          :height 'stretch
          :background-color etaf-image-dired--border-color
          :pointer 'hdrag
          :help-echo "拖动调整缩略图与预览区宽度；也可按 [ 和 ] 调整"
          :use (etaf-image-dired--session-resize-behavior session)))))

(etaf-define-component etaf-image-dired--app (&key session)
  "Render SESSION's gallery.
The entries ref is read from the library's retained effect, so a thumbnail
that image-dired finishes in the background repaints the grid.  Only the
split flag is read here: toggling P remounts the layout, while hovering a
card repaints just the selection frames and the preview."
  :render
  (let* ((session (etaf-current-prop 'session))
         (split (etaf-value (etaf-image-dired--session-split-ref session))))
    (etaf-view
     (column :width 'stretch
             :padding '((lh 1) (px 14))
             :item-gap '(lh 1)
             :keys etaf-image-dired--keys
       (flex :flex-direction 'row
             :flex-wrap 'nowrap
             :width 'stretch
             :align-items 'stretch
             :column-gap '(ch 2)
         (etaf-image-dired--library :session session)
         (etaf-image-dired--splitter :if split :session session)
         (etaf-image-dired--preview :if split :session session))))))

(defun etaf-image-dired--view (session)
  "Return the ETAF View of SESSION."
  (etaf-view (etaf-image-dired--app :session session)))

;;; Commands

(defun etaf-image-dired--open-file (file)
  "Open image FILE in another window."
  (find-file-other-window file))

;;;; Navigation

(defun etaf-image-dired-forward-image (&optional arg)
  "Move ARG images forward in the gallery.
Stop at the last image, like `image-dired-forward-image'."
  (interactive "p")
  (let* ((session (etaf-image-dired--current-session))
         (index (or (etaf-image-dired--current-index session) 0)))
    (etaf-image-dired--navigate-to session (+ index (or arg 1))
                                   "已经是最后一张图片")))

(defun etaf-image-dired-backward-image (&optional arg)
  "Move ARG images backward in the gallery.
Stop at the first image, like `image-dired-backward-image'."
  (interactive "p")
  (let* ((session (etaf-image-dired--current-session))
         (index (or (etaf-image-dired--current-index session) 0)))
    (etaf-image-dired--navigate-to session (- index (or arg 1))
                                   "已经是第一张图片")))

(defun etaf-image-dired-next-line (&optional arg)
  "Move down ARG thumbnail rows in the gallery."
  (interactive "p")
  (let* ((session (etaf-image-dired--current-session))
         (index (or (etaf-image-dired--current-index session) 0))
         (columns (etaf-image-dired--grid-columns session)))
    (etaf-image-dired--navigate-to session (+ index (* (or arg 1) columns)))))

(defun etaf-image-dired-previous-line (&optional arg)
  "Move up ARG thumbnail rows in the gallery."
  (interactive "p")
  (let* ((session (etaf-image-dired--current-session))
         (index (or (etaf-image-dired--current-index session) 0))
         (columns (etaf-image-dired--grid-columns session)))
    (etaf-image-dired--navigate-to session (- index (* (or arg 1) columns)))))

(defun etaf-image-dired--line-edge (session end-p)
  "Move to the first or, when END-P is non-nil, last card of the current row."
  (let* ((index (or (etaf-image-dired--current-index session) 0))
         (columns (etaf-image-dired--grid-columns session))
         (start (- index (mod index columns))))
    (etaf-image-dired--navigate-to session
                                   (if end-p (+ start (1- columns)) start))))

(defun etaf-image-dired-move-beginning-of-line ()
  "Move to the first thumbnail of the current row."
  (interactive)
  (etaf-image-dired--line-edge (etaf-image-dired--current-session) nil))

(defun etaf-image-dired-move-end-of-line ()
  "Move to the last thumbnail of the current row."
  (interactive)
  (etaf-image-dired--line-edge (etaf-image-dired--current-session) t))

;;;; Marks and deletion

(defun etaf-image-dired--mark-command (session mark message)
  "Apply MARK to SESSION's current file and show MESSAGE.
When `image-dired-marking-shows-next' is non-nil, move to the next image
afterwards, exactly like image-dired."
  (let ((file (etaf-image-dired--current-file session)))
    (unless file
      (user-error "没有选中的图片"))
    (etaf-image-dired--apply-dired-mark session file mark)
    (message "%s：%s" message (file-name-nondirectory file))
    (when image-dired-marking-shows-next
      (let ((index (etaf-image-dired--current-index session)))
        (when index
          (etaf-image-dired--navigate-to session (1+ index)))))))

(defun etaf-image-dired-mark-thumb-original-file ()
  "Mark the current image and move to the next one."
  (interactive)
  (etaf-image-dired--mark-command
   (etaf-image-dired--current-session) 'marked "已标记"))

(defun etaf-image-dired-unmark-thumb-original-file ()
  "Unmark the current image and move to the next one."
  (interactive)
  (etaf-image-dired--mark-command
   (etaf-image-dired--current-session) nil "已取消标记"))

(defun etaf-image-dired-flag-thumb-original-file ()
  "Flag the current image for deletion and move to the next one."
  (interactive)
  (etaf-image-dired--mark-command
   (etaf-image-dired--current-session) 'flagged "已标记为待删除"))

(defun etaf-image-dired-unmark-all-marks ()
  "Remove every mark and deletion flag from the directory.
When a Dired buffer visits the directory, its marks are cleared too."
  (interactive)
  (let ((session (etaf-image-dired--current-session)))
    (if-let* ((buffer (etaf-image-dired--dired-buffer session)))
        (progn
          (with-current-buffer buffer
            (dired-unmark-all-marks))
          (etaf-image-dired--sync-marks-from-dired session))
      (etaf-image-dired--set-marks session nil))
    (message "已取消全部标记")))

(defun etaf-image-dired-do-flagged-delete ()
  "Delete the files flagged with `d' or <delete> through Dired.
This runs `dired-do-flagged-delete', so it honours
`delete-by-moving-to-trash' and asks through `dired-deletion-confirmer'."
  (interactive)
  (let* ((session (etaf-image-dired--current-session))
         (flagged (etaf-image-dired--flagged-files session))
         (buffer (etaf-image-dired--ensure-dired-buffer session)))
    ;; Gallery marks may have been set without going through Dired; mirror
    ;; them so a single Dired deletion flow removes the union of both.
    (dolist (file flagged)
      (with-current-buffer buffer
        (save-excursion
          (when (dired-goto-file file)
            (dired-flag-file-deletion 1)))))
    (unless (or flagged
                (with-current-buffer buffer
                  (etaf-image-dired--dired-flagged-p)))
      (user-error "没有标记为删除的文件"))
    (let ((files (etaf-image-dired--files session)))
      (with-current-buffer buffer
        (dired-do-flagged-delete))
      (etaf-image-dired--sync-marks-from-dired session)
      (etaf-image-dired--refresh session)
      (let ((deleted (seq-count (lambda (file) (not (file-exists-p file)))
                                files)))
        (if (> deleted 0)
            (message "已删除 %d 个文件" deleted)
          (message "没有删除任何文件"))))))

(defun etaf-image-dired-delete-char ()
  "Remove the current thumbnail from the gallery without deleting the file.
This mirrors `image-dired-delete-char'; a later `g' rescans the directory
and brings the thumbnail back."
  (interactive)
  (let* ((session (etaf-image-dired--current-session))
         (file (etaf-image-dired--current-file session))
         (index (or (etaf-image-dired--current-index session) 0)))
    (unless file
      (user-error "没有选中的图片"))
    (etaf-image-dired--set-selection session nil)
    (when-let* ((ref (etaf-image-dired--session-hidden-ref session)))
      (etaf-set-value ref (cons file (etaf-value ref))))
    (etaf-image-dired--set-file-mark session file nil)
    (etaf-image-dired--refresh session)
    (let ((count (length (etaf-image-dired--files session))))
      (if (zerop count)
          (etaf-image-dired--set-selection session nil)
        (etaf-image-dired--focus-index session (min index (1- count)))))
    (message "已从图库移除缩略图：%s" (file-name-nondirectory file))))

;;;; Dired integration

(defun etaf-image-dired-track-original-file ()
  "Move point in a visible Dired buffer to the current image."
  (interactive)
  (let* ((session (etaf-image-dired--current-session))
         (file (etaf-image-dired--current-file session))
         (buffer (etaf-image-dired--dired-buffer session)))
    (unless file
      (user-error "没有选中的图片"))
    (unless buffer
      (user-error "没有找到对应的 Dired 缓冲区"))
    (with-current-buffer buffer
      (unless (dired-goto-file file)
        (user-error "Dired 中找不到该文件"))
      (when-let* ((window (get-buffer-window buffer t)))
        (set-window-point window (point))))
    (message "已在 Dired 中定位：%s" (file-name-nondirectory file))))

(defun etaf-image-dired-jump-original-dired-buffer ()
  "Jump to Dired for the current image.
Opens Dired in another window when no Dired buffer visits the directory."
  (interactive)
  (let* ((session (etaf-image-dired--current-session))
         (file (etaf-image-dired--current-file session))
         (buffer (etaf-image-dired--dired-buffer session)))
    (unless file
      (user-error "没有选中的图片"))
    (if (null buffer)
        (progn
          (dired-other-window (etaf-image-dired--session-directory session))
          ;; `dired-mode-hook' has installed the sync hooks; cache the new
          ;; buffer so gallery marks are written through it.
          (setf (etaf-image-dired--session-dired-buffer session)
                (etaf-image-dired--dired-buffer session))
          (unless (dired-goto-file file)
            (message "Dired 中找不到该文件")))
      (select-window (or (get-buffer-window buffer t)
                         (display-buffer buffer)))
      (unless (dired-goto-file file)
        (message "Dired 中找不到该文件")))))

;;;; Thumbnail layout

(defun etaf-image-dired-line-up ()
  "Lay the thumbnails out with `etaf-image-dired-thumbs-per-row' per row."
  (interactive)
  (let* ((session (etaf-image-dired--current-session))
         (columns (max 1 etaf-image-dired-thumbs-per-row)))
    (etaf-set-value (etaf-image-dired--session-thumbs-per-row-ref session)
                    columns)
    (message "每行 %d 张缩略图" columns)))

(defun etaf-image-dired-line-up-dynamic ()
  "Let the gallery choose how many thumbnails fit per row."
  (interactive)
  (let ((session (etaf-image-dired--current-session)))
    (etaf-set-value (etaf-image-dired--session-thumbs-per-row-ref session) nil)
    (message "每行自动排列（当前约 %d 张）"
             (etaf-image-dired--grid-columns session))))

(defun etaf-image-dired-line-up-interactive (columns)
  "Prompt for the number of thumbnails COLUMNS shown per row."
  (interactive
   (list (read-number "每行缩略图数量："
                      etaf-image-dired-thumbs-per-row)))
  (unless (and (integerp columns) (> columns 0))
    (user-error "数量必须大于 0"))
  (let ((session (etaf-image-dired--current-session)))
    (etaf-set-value (etaf-image-dired--session-thumbs-per-row-ref session)
                    columns)
    (message "每行 %d 张缩略图" columns)))

(defun etaf-image-dired-refresh ()
  "Rescan the directory of the current gallery."
  (interactive)
  (let ((session (etaf-image-dired--current-session)))
    ;; A rescan is a fresh view: thumbnails removed with C-d come back.
    (when-let* ((ref (etaf-image-dired--session-hidden-ref session)))
      (etaf-set-value ref nil))
    (etaf-image-dired--refresh session)
    (message "已扫描 %s"
             (abbreviate-file-name
              (etaf-image-dired--session-directory session)))))

(defun etaf-image-dired-regenerate ()
  "Delete cached thumbnails of the current gallery and create them again."
  (interactive)
  (let* ((session (etaf-image-dired--current-session))
         (entries (etaf-value (etaf-image-dired--session-entries-ref session))))
    (dolist (entry entries)
      (let ((thumb (plist-get entry :thumb)))
        (when (file-exists-p thumb)
          (ignore-errors (delete-file thumb))
          (ignore-errors (clear-image-cache thumb)))))
    (etaf-image-dired--refresh session)
    (message "正在重新生成缩略图…")))

(defun etaf-image-dired-open-at-point ()
  "Open the original image of the selected card.
The card selected under the mouse pointer, or holding keyboard focus, wins;
point's card is used when there is no selection."
  (interactive)
  (let* ((session (etaf-image-dired--current-session))
         (file (or (etaf-image-dired--selection-file session)
                   (let ((runtime (etaf-runtime-for-buffer (current-buffer)))
                         (refs (etaf-image-dired--session-refs session)))
                     (when runtime
                       (when-let* ((ref (etaf-host-ref-at runtime (point))))
                         (gethash ref refs)))))))
    (if file
        (etaf-image-dired--open-file file)
      (user-error "光标处没有图片"))))

(defun etaf-image-dired--reveal-card (session ref)
  "Scroll SESSION's grid so REF's card is visible.
The grid is narrower with the preview pane open, so the selected card may
have reflowed below the fixed-height viewport; focus movement scrolls on
its own, but opening the pane does not move focus."
  (when ref
    (etaf-next-tick
     (lambda ()
       (when-let* ((buffer (etaf-image-dired--session-buffer session))
                   (runtime (and (buffer-live-p buffer)
                                 (etaf-runtime-for-buffer buffer))))
         (ignore-errors (etaf-scroll-into-view runtime ref)))))))

(defun etaf-image-dired-preview ()
  "Toggle the preview pane of the current gallery.
The pane is added to the gallery buffer itself: the thumbnails keep the left
side and the selected image appears on the right.  The preview follows the
selection while it is open."
  (interactive)
  (let* ((session (etaf-image-dired--current-session))
         (split-ref (etaf-image-dired--session-split-ref session))
         (preview-ref (etaf-image-dired--session-preview-ref session)))
    (if (etaf-value split-ref)
        (progn
          (etaf-set-value preview-ref nil)
          (etaf-set-value split-ref nil))
      (let ((file (or (etaf-image-dired--selection-file session)
                      (etaf-image-dired--focused-file session))))
        (unless file
          (user-error "没有选中的图片"))
        (etaf-set-value preview-ref file)
        (etaf-set-value split-ref t)
        (etaf-image-dired--reveal-card
         session
         (or (etaf-value (etaf-image-dired--session-focus-ref session))
             (let ((runtime (etaf-runtime-for-buffer (current-buffer))))
               (and runtime (etaf-focused-host-ref runtime)))))))))

;;;; Display

(defun etaf-image-dired--display-file (session file)
  "Show FILE in SESSION's preview pane, opening the pane if necessary."
  (unless file
    (user-error "没有选中的图片"))
  (etaf-set-value (etaf-image-dired--session-preview-ref session) file)
  (etaf-set-value (etaf-image-dired--session-split-ref session) t))

(defun etaf-image-dired-display-this ()
  "Display the current image in the gallery's preview pane."
  (interactive)
  (let ((session (etaf-image-dired--current-session)))
    (etaf-image-dired--display-file
     session (etaf-image-dired--current-file session))))

(defun etaf-image-dired-display-next (&optional arg)
  "Move ARG images forward, wrapping around, and display the new image."
  (interactive "p")
  (let* ((session (etaf-image-dired--current-session))
         (files (etaf-image-dired--files session))
         (count (length files)))
    (when (zerop count)
      (user-error "当前目录没有图片"))
    (let* ((index (or (etaf-image-dired--current-index session) 0))
           (target (mod (+ index (or arg 1)) count))
           (file (nth target files)))
      (etaf-image-dired--focus-index session target)
      (etaf-image-dired--display-file session file)
      (message "%s" (file-name-nondirectory file)))))

(defun etaf-image-dired-display-previous (&optional arg)
  "Move ARG images backward, wrapping around, and display the new image."
  (interactive "p")
  (etaf-image-dired-display-next (- (or arg 1))))

(defun etaf-image-dired-thumbnail-display-external ()
  "Display the current image in the external viewer.
The viewer is `image-dired-external-viewer'."
  (interactive)
  (let* ((session (etaf-image-dired--current-session))
         (file (etaf-image-dired--current-file session)))
    (unless file
      (user-error "没有选中的图片"))
    (setq file (expand-file-name file))
    (cond
     ((stringp image-dired-external-viewer)
      (apply #'start-process "etaf-image-dired-thumb-external" nil
             (append (string-split image-dired-external-viewer " ")
                     (list file))))
     ((eq system-type 'windows-nt)
      (w32-shell-execute "open" file))
     (t
      (user-error "`image-dired-external-viewer' 没有指定图片查看程序")))))

;;;; Metadata

(defun etaf-image-dired-tag-thumbnail ()
  "Add an image-dired tag to the current image or to all marked images."
  (interactive)
  (let* ((session (etaf-image-dired--current-session))
         (tag (string-trim
               (read-string "添加标签（多个标签用分号分隔）：")))
         (files (etaf-image-dired--target-files session)))
    (when (string-empty-p tag)
      (user-error "标签不能为空"))
    (image-dired-write-tags (mapcar (lambda (file) (cons file tag)) files))
    (message "已为 %d 张图片添加标签：%s" (length files) tag)))

(defun etaf-image-dired-tag-thumbnail-remove ()
  "Remove an image-dired tag from the current image or marked images."
  (interactive)
  (let* ((session (etaf-image-dired--current-session))
         (files (etaf-image-dired--target-files session))
         (known (delete-dups
                 (apply #'append (mapcar #'image-dired-list-tags files))))
         (tag (completing-read "移除标签：" known nil t)))
    (when (string-empty-p tag)
      (user-error "标签不能为空"))
    (image-dired-remove-tag files tag)
    (message "已移除标签：%s" tag)))

(defun etaf-image-dired-comment-thumbnail ()
  "Add or edit the image-dired comment of the current image."
  (interactive)
  (let* ((session (etaf-image-dired--current-session))
         (file (etaf-image-dired--current-file session)))
    (unless file
      (user-error "没有选中的图片"))
    (let ((comment (image-dired-read-comment file)))
      (image-dired-write-comments (list (cons file comment)))
      (message "已保存注释"))))

(defun etaf-image-dired-thumbnail-set-image-description ()
  "Set the ImageDescription EXIF tag of the current image."
  (interactive)
  (let* ((session (etaf-image-dired--current-session))
         (file (etaf-image-dired--current-file session)))
    (unless file
      (user-error "没有选中的图片"))
    (let* ((old-value (or (ignore-errors
                            (exif-field 'description (exif-parse-file file)))
                          ""))
           (value (read-string "ImageDescription: " old-value)))
      (with-temp-buffer
        (if (eq 0 (image-dired-set-exif-data file "ImageDescription" value))
            (message "已写入 ImageDescription 标签")
          (user-error "无法写入 ImageDescription 标签"))))))

;;;; Editing originals

(defun etaf-image-dired--rotate-files (session files degrees)
  "Rotate FILES by DEGREES using image-dired's rotation command.
Only JPEG images can be rotated.  The originals are overwritten after the
confirmation controlled by `image-dired-rotate-original-ask-before-overwrite',
and their thumbnails are regenerated."
  (unless (executable-find image-dired-cmd-rotate-original-program)
    (user-error "未找到旋转程序 `%s'" image-dired-cmd-rotate-original-program))
  (dolist (file files)
    (unless (eq 'jpeg (image-type file))
      (user-error "只能旋转 JPEG 图片：%s" (file-name-nondirectory file))))
  (when (and image-dired-rotate-original-ask-before-overwrite
             (not (y-or-n-p "旋转后覆盖原图？")))
    (user-error "已取消旋转"))
  (dolist (file files)
    (let* ((spec (list (cons ?d degrees)
                       (cons ?o (expand-file-name file))
                       (cons ?t image-dired-temp-rotate-image-file)))
           (status (apply #'call-process
                          image-dired-cmd-rotate-original-program
                          nil nil nil
                          (mapcar (lambda (option) (format-spec option spec))
                                  image-dired-cmd-rotate-original-options))))
      (unless (zerop status)
        (user-error "无法旋转：%s" (file-name-nondirectory file)))
      (copy-file image-dired-temp-rotate-image-file file t)
      (let ((thumb (image-dired-thumb-name file)))
        (ignore-errors (delete-file thumb))
        (ignore-errors (clear-image-cache thumb)))))
  (etaf-image-dired--refresh session)
  (message "已旋转 %d 张图片" (length files)))

(defun etaf-image-dired-rotate-original-left ()
  "Rotate the current or marked images 90 degrees counter clockwise."
  (interactive)
  (let ((session (etaf-image-dired--current-session)))
    (etaf-image-dired--rotate-files
     session (etaf-image-dired--target-files session) "270")))

(defun etaf-image-dired-rotate-original-right ()
  "Rotate the current or marked images 90 degrees clockwise."
  (interactive)
  (let ((session (etaf-image-dired--current-session)))
    (etaf-image-dired--rotate-files
     session (etaf-image-dired--target-files session) "90")))

(defun etaf-image-dired-wallpaper-set (&optional file)
  "Set the wallpaper to FILE, defaulting to the current image."
  (interactive)
  (let ((file (or file
                  (etaf-image-dired--current-file
                   (etaf-image-dired--current-session)))))
    (unless file
      (user-error "没有选中的图片"))
    (wallpaper-set file)))

(defun etaf-image-dired-copy-filename-as-kill (&optional arg)
  "Copy the names of the marked (or current) images to the kill ring.
With prefix ARG 0, copy absolute names; otherwise copy the base names,
like `dired-copy-filename-as-kill'."
  (interactive "P")
  (let* ((session (etaf-image-dired--current-session))
         (files (or (etaf-image-dired--marked-files session)
                    (let ((file (etaf-image-dired--current-file session)))
                      (unless file
                        (user-error "没有选中的图片"))
                      (list file))))
         (names (mapcar (if (equal (prefix-numeric-value arg) 0)
                            #'expand-file-name
                          #'file-name-nondirectory)
                        files)))
    (kill-new (mapconcat #'identity names "\n"))
    (message "已复制 %d 个文件名" (length names))))

;;;; Slideshow

(defun etaf-image-dired--slideshow-stop-timer (session)
  "Cancel SESSION's slideshow timer, keeping its post-command hook."
  (when (timerp (etaf-image-dired--session-slideshow-timer session))
    (cancel-timer (etaf-image-dired--session-slideshow-timer session)))
  (setf (etaf-image-dired--session-slideshow-timer session) nil))

(defun etaf-image-dired--slideshow-start-timer (session)
  "Start SESSION's slideshow timer with its stored delay."
  (etaf-image-dired--slideshow-stop-timer session)
  (let ((delay (or (etaf-image-dired--session-slideshow-delay session)
                   image-dired-slideshow-delay)))
    (setf (etaf-image-dired--session-slideshow-timer session)
          (run-at-time delay delay
                       (lambda () (etaf-image-dired--slideshow-step session))))))

(defun etaf-image-dired--slideshow-stop (session)
  "Stop SESSION's slideshow and remove its post-command hook."
  (etaf-image-dired--slideshow-stop-timer session)
  (let ((buffer (etaf-image-dired--session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (remove-hook 'post-command-hook
                     #'etaf-image-dired--slideshow-post-command t)))))

(defun etaf-image-dired--slideshow-step (session)
  "Advance SESSION's slideshow by one image."
  (let ((buffer (etaf-image-dired--session-buffer session)))
    (if (and (buffer-live-p buffer)
             (etaf-runtime-for-buffer buffer))
        (with-current-buffer buffer
          (condition-case nil
              (etaf-image-dired-display-next 1)
            (error (etaf-image-dired--slideshow-stop session))))
      (etaf-image-dired--slideshow-stop session))))

(defun etaf-image-dired--slideshow-post-command ()
  "Keep the slideshow running after its own commands, stop it otherwise.
RET pauses or resumes it, like `image-dired-display-this' does."
  (let ((session (ignore-errors (etaf-app-state))))
    (when (etaf-image-dired--session-p session)
      (cond
       ((memq this-command '(etaf-image-dired-slideshow-start
                             etaf-image-dired-display-next
                             etaf-image-dired-display-previous))
        (when (timerp (etaf-image-dired--session-slideshow-timer session))
          (etaf-image-dired--slideshow-start-timer session)))
       ((eq this-command 'etaf-image-dired-display-this)
        (if (timerp (etaf-image-dired--session-slideshow-timer session))
            (progn
              (etaf-image-dired--slideshow-stop-timer session)
              (message "幻灯片已暂停；RET 继续，其它命令停止"))
          (etaf-image-dired--slideshow-start-timer session)
          (message "幻灯片继续")))
       (t
        (etaf-image-dired--slideshow-stop session)
        (message "幻灯片已停止"))))))

(defun etaf-image-dired-slideshow-start (&optional arg)
  "Start or stop a slideshow of the gallery.
With a numeric prefix ARG, wait that many seconds between images; with a
non-positive prefix, prompt for the delay.  SPC and DEL move through the
slideshow, RET pauses it, and any other command stops it."
  (interactive "P")
  (let* ((session (etaf-image-dired--current-session))
         (prefix (cond ((null arg) nil)
                       ((numberp arg) arg)
                       ((consp arg) (prefix-numeric-value arg))
                       (t nil))))
    (if (timerp (etaf-image-dired--session-slideshow-timer session))
        (progn
          (etaf-image-dired--slideshow-stop session)
          (message "幻灯片已停止"))
      (etaf-image-dired--display-file
       session (etaf-image-dired--current-file session))
      (setf (etaf-image-dired--session-slideshow-delay session)
            (cond ((null prefix) image-dired-slideshow-delay)
                  ((> prefix 0) prefix)
                  (t (read-number "幻灯片间隔（秒）："
                                  image-dired-slideshow-delay))))
      (etaf-image-dired--slideshow-start-timer session)
      (let ((buffer (etaf-image-dired--session-buffer session)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (add-hook 'post-command-hook
                      #'etaf-image-dired--slideshow-post-command nil t))))
      (message "幻灯片：每 %.1f 秒切换；SPC/DEL 切换，RET 暂停，其它命令停止"
               (etaf-image-dired--session-slideshow-delay session)))))

(defun etaf-image-dired--adjust-preview-width (session columns)
  "Move SESSION's split by COLUMNS, positive widening the preview pane.
The stored width is clamped to the window, so the thumbnail grid always
keeps room to wrap."
  (let* ((current (etaf-image-dired--preview-panel-width session))
         (next (etaf-image-dired--clamp-preview-width session (+ current columns))))
    (etaf-set-value (etaf-image-dired--session-preview-width-ref session) next)
    (message "预览区宽度：%d 列%s"
             next
             (if (etaf-value (etaf-image-dired--session-split-ref session))
                 ""
               "（按 P 打开预览）"))))

(defun etaf-image-dired-widen-preview (&optional steps)
  "Widen the preview pane by STEPS column steps.
Interactively, STEPS is the numeric prefix (one without a prefix), each
step being `etaf-image-dired-preview-width-step' columns."
  (interactive "p")
  (etaf-image-dired--adjust-preview-width
   (etaf-image-dired--current-session)
   (* (max 1 (or steps 1)) etaf-image-dired-preview-width-step)))

(defun etaf-image-dired-narrow-preview (&optional steps)
  "Narrow the preview pane by STEPS column steps.
Interactively, STEPS is the numeric prefix (one without a prefix), each
step being `etaf-image-dired-preview-width-step' columns."
  (interactive "p")
  (etaf-image-dired--adjust-preview-width
   (etaf-image-dired--current-session)
   (- (* (max 1 (or steps 1)) etaf-image-dired-preview-width-step))))

(defun etaf-image-dired-reset-preview-width ()
  "Restore the automatic three-fifths preview pane width."
  (interactive)
  (let ((session (etaf-image-dired--current-session)))
    (etaf-set-value (etaf-image-dired--session-preview-width-ref session) nil)
    (message "预览区宽度：自动（%d 列）"
             (etaf-image-dired--preview-panel-width session))))

(defun etaf-image-dired-quit ()
  "Close the current gallery."
  (interactive)
  (etaf-app-close))

;;;###autoload
(defun etaf-image-dired (&optional directory)
  "Display an ETAF gallery of the images in DIRECTORY.
Interactively, use the current buffer's `default-directory'; with a prefix
argument, prompt for a directory instead."
  (interactive
   (list (when current-prefix-arg
           (read-directory-name "etaf-image-dired directory: "
                                nil default-directory t))))
  (let* ((dir (file-name-as-directory
               (expand-file-name (or directory default-directory))))
         (session (etaf-image-dired--session--create
                   :directory dir
                   :entries-ref (etaf-ref nil :name 'etaf-image-dired-entries)
                   :focus-ref (etaf-ref nil :name 'etaf-image-dired-focus)
                   :preview-ref (etaf-ref nil :name 'etaf-image-dired-preview)
                   :split-ref (etaf-ref nil :name 'etaf-image-dired-split)
                   :preview-width-ref
                   (etaf-ref nil :name 'etaf-image-dired-preview-width)
                   :marks-ref (etaf-ref nil :name 'etaf-image-dired-marks)
                   :hidden-ref (etaf-ref nil :name 'etaf-image-dired-hidden)
                   :thumbs-per-row-ref
                   (etaf-ref nil :name 'etaf-image-dired-thumbs-per-row)
                   :refs (make-hash-table :test #'eq)
                   :frame (selected-frame)))
         (name (format "*etaf-image-dired: %s*" (abbreviate-file-name dir))))
    ;; The drag Behavior must outlive renders: see its docstring.
    (setf (etaf-image-dired--session-resize-behavior session)
          (etaf-image-dired--make-resize-behavior session))
    (etaf-image-dired--refresh session)
    (let ((buffer (etaf-app-open
                   name
                   (lambda () (etaf-image-dired--view session))
                   :state session
                   :cursor nil
                   :on-close (lambda ()
                               (etaf-image-dired--stop-timer session)
                               (etaf-image-dired--slideshow-stop session)))))
      (setf (etaf-image-dired--session-buffer session) buffer)
      (etaf-image-dired--disable-current-line-highlight buffer)
      (etaf-image-dired--install-pointer-tracking session buffer)
      ;; Adopt marks that already exist in a Dired buffer for the directory.
      (when-let* ((dired (etaf-image-dired--dired-buffer session)))
        (with-current-buffer dired
          (etaf-image-dired--dired-mode-setup))
        (etaf-image-dired--sync-marks-from-dired session))
      ;; Start with a selection so the frame marks a card as soon as the
      ;; gallery opens and arrow-key navigation begins from a known place.
      (when-let* ((runtime (etaf-runtime-for-buffer buffer))
                  (entries (etaf-value
                            (etaf-image-dired--session-entries-ref session))))
        (ignore-errors
          (etaf-focus runtime (plist-get (car entries) :ref))))
      buffer)))

(provide 'etaf-image-dired)

;;; etaf-image-dired.el ends here
