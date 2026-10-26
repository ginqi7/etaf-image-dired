;;; etaf-image-dired-test.el --- Buttercup tests for etaf-image-dired  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: multimedia, hypermedia

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

;; Tests for `etaf-image-dired'.
;;
;; Thumbnail handling is exercised against the real `image-dired' API: the
;; gallery is expected to keep using image-dired's thumbnail names, cache
;; directory and queue, so most assertions compare the gallery's behaviour
;; with image-dired's own functions instead of hard-coding the same rules a
;; second time.
;;
;; `image-dired-create-thumb' is stubbed everywhere: actually generating
;; thumbnails needs an external program, and scaling them needs a window
;; system.  A spec that needs a finished thumbnail writes the file itself and
;; lets the polling timer discover it, exactly as it would discover one that
;; image-dired created in the background.

;;; Code:

(require 'base64)
(require 'buttercup)
(require 'cl-lib)
(require 'seq)
(require 'image-dired)
(require 'etaf-image-dired)

;;;; Helpers

(defconst etaf-image-dired-test--png
  (base64-decode-string
   (concat "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4"
           "2mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="))
  "A 1x1 PNG used as a stand-in for real image files.")

(defun etaf-image-dired-test--write (file &optional contents)
  "Write CONTENTS (a PNG by default) to FILE and return its absolute name.
Parent directories are created as needed."
  (setq file (expand-file-name file))
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'binary))
    (write-region (or contents etaf-image-dired-test--png)
                  nil file nil 'silent))
  file)

(defun etaf-image-dired-test--set-mtime (file seconds)
  "Set FILE's modification time to SECONDS since the epoch."
  (set-file-times file (seconds-to-time seconds))
  file)

(defun etaf-image-dired-test--mtime (file)
  "Return FILE's modification time, or nil when it does not exist."
  (when-let* ((attributes (file-attributes file)))
    (file-attribute-modification-time attributes)))

(defun etaf-image-dired-test--make-session (directory &optional buffer)
  "Return a gallery session for DIRECTORY, optionally showing BUFFER."
  (etaf-image-dired--session--create
   :directory (file-name-as-directory (expand-file-name directory))
   :entries-ref (etaf-ref nil :name 'test-entries)
   :focus-ref (etaf-ref nil :name 'test-focus)
   :preview-ref (etaf-ref nil :name 'test-preview)
   :split-ref (etaf-ref nil :name 'test-split)
   :preview-width-ref (etaf-ref nil :name 'test-preview-width)
   :marks-ref (etaf-ref nil :name 'test-marks)
   :hidden-ref (etaf-ref nil :name 'test-hidden)
   :thumbs-per-row-ref (etaf-ref nil :name 'test-thumbs-per-row)
   :refs (make-hash-table :test #'eq)
   :frame (selected-frame)
   :buffer buffer))

(defun etaf-image-dired-test--entries (session)
  "Return SESSION's current entry list."
  (etaf-value (etaf-image-dired--session-entries-ref session)))

(defun etaf-image-dired-test--select (session index)
  "Select the thumbnail at INDEX in SESSION and return its file."
  (let* ((entries (etaf-image-dired-test--entries session))
         (entry (nth index entries)))
    (unless entry
      (error "No thumbnail at index %d" index))
    (etaf-image-dired--set-selection session (plist-get entry :ref))
    (plist-get entry :file)))

(defmacro etaf-image-dired-test--with-gallery (&rest body)
  "Run BODY in a temporary directory with an isolated thumbnail cache.
BODY sees `root', `image-dired-dir' and `default-directory' bound."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory
                 (make-temp-file "etaf-image-dired-test-" t)))
          (default-directory root)
          (image-dired-dir (expand-file-name "image-dired/" root))
          (image-dired-thumbnail-storage 'image-dired)
          (image-dired-thumb-naming 'sha1-filename)
          (image-dired-queue nil)
          (image-dired-queue-active-jobs 0)
          ;; image-dired announces the cache directory it creates; that is
          ;; noise in a test run.
          (inhibit-message t))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defmacro etaf-image-dired-test--with-session (&rest body)
  "Run BODY with a gallery `session' for the temporary directory."
  (declare (indent 0) (debug t))
  `(etaf-image-dired-test--with-gallery
     (let ((session (etaf-image-dired-test--make-session default-directory)))
       (unwind-protect (progn ,@body)
         (etaf-image-dired--stop-timer session)
         (etaf-image-dired--slideshow-stop session)
         (when-let* ((dired (etaf-image-dired--session-dired-buffer session)))
           (when (buffer-live-p dired)
             (kill-buffer dired)))
         (when-let* ((buffer (etaf-image-dired--session-buffer session)))
           (when (buffer-live-p buffer)
             (kill-buffer buffer)))))))

(defmacro etaf-image-dired-test--with-tick-session (&rest body)
  "Run BODY with a `session' whose buffer is live and runtime is faked.
The fake runtime lets `etaf-image-dired--tick' believe the gallery is
mounted, so it can test state transitions without rendering anything."
  (declare (indent 0) (debug t))
  `(etaf-image-dired-test--with-gallery
     (let* ((buffer (generate-new-buffer " *etaf-image-dired-test*"))
            (session (etaf-image-dired-test--make-session
                      default-directory buffer)))
       (spy-on 'etaf-runtime-for-buffer :and-return-value t)
       (unwind-protect (progn ,@body)
         (etaf-image-dired--stop-timer session)
         (etaf-image-dired--slideshow-stop session)
         (when-let* ((dired (etaf-image-dired--session-dired-buffer session)))
           (when (buffer-live-p dired)
             (kill-buffer dired)))
         (when (buffer-live-p buffer)
           (kill-buffer buffer))))))

;;;; Finding images

(describe "image file discovery"
  (it "finds only image files, sorted by name"
    (etaf-image-dired-test--with-gallery
      (etaf-image-dired-test--write "b.png")
      (etaf-image-dired-test--write "a.jpg")
      (etaf-image-dired-test--write "c.gif")
      (etaf-image-dired-test--write "notes.txt")
      ;; Like dired (and therefore image-dired), hidden files are listed.
      (etaf-image-dired-test--write ".hidden.png")
      (etaf-image-dired-test--write "subdir/nested.png")
      (expect (mapcar #'file-name-nondirectory
                      (etaf-image-dired--image-files root))
              :to-equal '(".hidden.png" "a.jpg" "b.png" "c.gif"))))

  (it "matches extensions like `image-file-name-regexp' does"
    (etaf-image-dired-test--with-gallery
      (etaf-image-dired-test--write "A.PNG")
      (etaf-image-dired-test--write "b.JPG")
      (etaf-image-dired-test--write "c.webp")
      (expect (mapcar #'file-name-nondirectory
                      (etaf-image-dired--image-files root))
              :to-equal '("A.PNG" "b.JPG" "c.webp"))))

  (it "returns nil for a directory without images"
    (etaf-image-dired-test--with-gallery
      (etaf-image-dired-test--write "notes.txt")
      (expect (etaf-image-dired--image-files root) :to-be nil))))

;;;; Thumbnail naming, cache and freshness

(describe "thumbnail cache compatibility with image-dired"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "uses `image-dired-thumb-name' for every image"
    (etaf-image-dired-test--with-session
      (let* ((file (etaf-image-dired-test--write "photo.png"))
             (entry (etaf-image-dired--entry session file)))
        (expect (plist-get entry :thumb)
                :to-equal (image-dired-thumb-name file)))))

  (it "keeps thumbnails in `image-dired-dir' under image-dired's hash name"
    (etaf-image-dired-test--with-session
      (let* ((file (etaf-image-dired-test--write "photo.png"))
             (thumb (plist-get (etaf-image-dired--entry session file) :thumb)))
        (expect (file-name-directory thumb)
                :to-equal (file-name-as-directory
                           (expand-file-name image-dired-dir)))
        (expect (file-name-nondirectory thumb)
                :to-equal (concat (sha1 file) ".jpg")))))

  (it "follows a non-default `image-dired-thumbnail-storage'"
    (etaf-image-dired-test--with-session
      (let* ((image-dired-thumbnail-storage 'standard)
             (file (etaf-image-dired-test--write "photo.png"))
             (entry (etaf-image-dired--entry session file)))
        (expect (plist-get entry :thumb)
                :to-equal (image-dired-thumb-name file)))))

  (it "copies an entry before marking it ready"
    (etaf-image-dired-test--with-session
      (let* ((file (etaf-image-dired-test--write "photo.png"))
             (thumb (etaf-image-dired-test--write "thumb.png"))
             (entry (list :file file :thumb thumb
                          :ref 'some-card :name "photo.png"))
             (ready (etaf-image-dired--entry-ready session entry)))
        (expect (plist-get entry :state) :to-be nil)
        (expect (plist-get ready :state) :to-equal 'ready)))))

(describe "thumbnail freshness"
  (it "treats a thumbnail at least as new as its image as current"
    (etaf-image-dired-test--with-gallery
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (thumb (etaf-image-dired-test--write "thumb.png")))
        (etaf-image-dired-test--set-mtime file 1000)
        (etaf-image-dired-test--set-mtime thumb 1000)
        (expect (etaf-image-dired--thumb-current-p file thumb) :to-be t)
        (etaf-image-dired-test--set-mtime thumb 2000)
        (expect (etaf-image-dired--thumb-current-p file thumb) :to-be t))))

  (it "treats an older thumbnail as stale"
    (etaf-image-dired-test--with-gallery
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (thumb (etaf-image-dired-test--write "thumb.png")))
        (etaf-image-dired-test--set-mtime file 2000)
        (etaf-image-dired-test--set-mtime thumb 1000)
        (expect (etaf-image-dired--thumb-current-p file thumb) :to-be nil))))

  (it "treats a missing thumbnail as stale"
    (etaf-image-dired-test--with-gallery
      (let ((file (etaf-image-dired-test--write "a.png")))
        (expect (etaf-image-dired--thumb-current-p
                 file (expand-file-name "missing.jpg" root))
                :to-be nil)))))

;;;; Queueing

(describe "thumbnail queueing"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "queues a missing thumbnail through image-dired"
    (etaf-image-dired-test--with-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (thumb (image-dired-thumb-name file))
             (entry (etaf-image-dired--entry session file)))
        (expect (plist-get entry :state) :to-equal 'pending)
        (expect (numberp (plist-get entry :queued-at)) :to-be-truthy)
        (expect 'image-dired-create-thumb
                :to-have-been-called-with file thumb)
        (expect (spy-calls-count 'image-dired-create-thumb)
                :to-equal 1))))

  (it "reports a current thumbnail as ready without queueing"
    (etaf-image-dired-test--with-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (thumb (etaf-image-dired-test--write
                     (image-dired-thumb-name file))))
        (etaf-image-dired-test--set-mtime file 1000)
        (etaf-image-dired-test--set-mtime thumb 2000)
        (let ((entry (etaf-image-dired--entry session file)))
          (expect (plist-get entry :state) :to-equal 'ready)
          (expect (plist-get entry :queued-at) :to-be nil)
          (expect (plist-get entry :thumb-mtime)
                  :to-equal (etaf-image-dired-test--mtime thumb))
          (expect 'image-dired-create-thumb
                  :not :to-have-been-called)))))

  (it "keeps showing a stale thumbnail while image-dired regenerates it"
    (etaf-image-dired-test--with-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (thumb (etaf-image-dired-test--write
                     (image-dired-thumb-name file))))
        (etaf-image-dired-test--set-mtime thumb 1000)
        (etaf-image-dired-test--set-mtime file 2000)
        (let ((entry (etaf-image-dired--entry session file)))
          (expect (plist-get entry :state) :to-equal 'ready)
          (expect (plist-get entry :queued-at) :to-be-truthy)
          (expect 'image-dired-create-thumb
                  :to-have-been-called-with file thumb)))))

  (it "does not queue a thumbnail job image-dired already has"
    (etaf-image-dired-test--with-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (thumb (image-dired-thumb-name file)))
        (let ((image-dired-queue (list (list file thumb))))
          (etaf-image-dired--entry session file)
          (expect 'image-dired-create-thumb
                  :not :to-have-been-called))))))

(describe "queueing a single thumbnail"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil))

  (it "delegates to `image-dired-create-thumb'"
    (etaf-image-dired-test--with-gallery
      (etaf-image-dired--queue-thumb "/tmp/a.png" "/tmp/a.jpg")
      (expect 'image-dired-create-thumb
              :to-have-been-called-with "/tmp/a.png" "/tmp/a.jpg")))

  (it "leaves an already queued job alone"
    (etaf-image-dired-test--with-gallery
      (let ((image-dired-queue '(("/tmp/a.png" "/tmp/a.jpg"))))
        (etaf-image-dired--queue-thumb "/tmp/a.png" "/tmp/a.jpg")
        (expect 'image-dired-create-thumb :not :to-have-been-called)))))

;;;; Scanning

(describe "refreshing a gallery"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "queues every image and starts the polling timer"
    (etaf-image-dired-test--with-session
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired-test--write "b.jpg")
      (etaf-image-dired-test--write "c.gif")
      (etaf-image-dired--refresh session)
      (expect (length (etaf-image-dired-test--entries session))
              :to-equal 3)
      (expect (spy-calls-count 'image-dired-create-thumb) :to-equal 3)
      (expect (timerp (etaf-image-dired--session-timer session))
              :to-be t)))

  (it "does not start the timer when every thumbnail is ready"
    (etaf-image-dired-test--with-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (thumb (etaf-image-dired-test--write
                     (image-dired-thumb-name file))))
        (etaf-image-dired-test--set-mtime file 1000)
        (etaf-image-dired-test--set-mtime thumb 2000)
        (etaf-image-dired--refresh session)
        (expect (plist-get (car (etaf-image-dired-test--entries session))
                           :state)
                :to-equal 'ready)
        (expect (etaf-image-dired--session-timer session) :to-be nil))))

  (it "registers a ref for every card"
    (etaf-image-dired-test--with-session
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired-test--write "b.png")
      (etaf-image-dired--refresh session)
      (let ((refs (etaf-image-dired--session-refs session)))
        (dolist (entry (etaf-image-dired-test--entries session))
          (expect (gethash (plist-get entry :ref) refs)
                  :to-equal (plist-get entry :file))))))

  (it "rescans and replaces the entry list"
    (etaf-image-dired-test--with-session
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired--refresh session)
      (expect (length (etaf-image-dired-test--entries session)) :to-equal 1)
      (etaf-image-dired-test--write "b.png")
      (etaf-image-dired--refresh session)
      (expect (length (etaf-image-dired-test--entries session)) :to-equal 2))))

;;;; Thumbnail jobs finishing

(describe "thumbnail job completion"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "turns a pending entry ready once the thumbnail exists"
    (etaf-image-dired-test--with-tick-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (entry (etaf-image-dired--entry session file))
             (thumb (plist-get entry :thumb)))
        (expect (plist-get entry :state) :to-equal 'pending)
        (etaf-image-dired-test--write thumb)
        (etaf-set-value (etaf-image-dired--session-entries-ref session)
                        (list entry))
        (setf (etaf-image-dired--session-timer session)
              (run-at-time 60 nil #'ignore))
        (etaf-image-dired--tick session)
        (let ((ready (car (etaf-image-dired-test--entries session))))
          (expect (plist-get ready :state) :to-equal 'ready)
          (expect (plist-get ready :queued-at) :to-be nil)
          (expect (plist-get ready :thumb-mtime)
                  :to-equal (etaf-image-dired-test--mtime thumb)))
        (expect (etaf-image-dired--session-timer session) :to-be nil))))

  (it "marks a pending entry failed when image-dired's queue drains"
    (etaf-image-dired-test--with-tick-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (entry (etaf-image-dired--entry session file))
             (stale (plist-put (copy-sequence entry)
                               :queued-at (- (float-time) 2))))
        (etaf-set-value (etaf-image-dired--session-entries-ref session)
                        (list stale))
        (setf (etaf-image-dired--session-timer session)
              (run-at-time 60 nil #'ignore))
        (etaf-image-dired--tick session)
        (expect (plist-get (car (etaf-image-dired-test--entries session))
                           :state)
                :to-equal 'failed)
        (expect (etaf-image-dired--session-timer session) :to-be nil))))

  (it "keeps waiting while image-dired still has the job queued"
    (etaf-image-dired-test--with-tick-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (entry (etaf-image-dired--entry session file))
             (running (plist-put (copy-sequence entry)
                                 :queued-at (- (float-time) 2))))
        (let ((image-dired-queue
               (list (list file (plist-get entry :thumb))))
              (image-dired-queue-active-jobs 1))
          (etaf-set-value (etaf-image-dired--session-entries-ref session)
                          (list running))
          (setf (etaf-image-dired--session-timer session)
                (run-at-time 60 nil #'ignore))
          (etaf-image-dired--tick session)
          (expect (plist-get (car (etaf-image-dired-test--entries session))
                             :state)
                  :to-equal 'pending)
          (expect (timerp (etaf-image-dired--session-timer session))
                  :to-be t)))))

  (it "waits one second before declaring a drained queue a failure"
    (etaf-image-dired-test--with-tick-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (entry (etaf-image-dired--entry session file))
             (fresh (plist-put (copy-sequence entry) :queued-at (float-time))))
        (etaf-set-value (etaf-image-dired--session-entries-ref session)
                        (list fresh))
        (setf (etaf-image-dired--session-timer session)
              (run-at-time 60 nil #'ignore))
        (etaf-image-dired--tick session)
        (expect (plist-get (car (etaf-image-dired-test--entries session))
                           :state)
                :to-equal 'pending)
        (expect (timerp (etaf-image-dired--session-timer session))
                :to-be t))))

  (it "notices a regenerated thumbnail of a ready entry"
    (etaf-image-dired-test--with-tick-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (thumb (etaf-image-dired-test--write
                     (image-dired-thumb-name file))))
        (etaf-image-dired-test--set-mtime file 1000)
        (etaf-image-dired-test--set-mtime thumb 2000)
        (let ((entry (etaf-image-dired--entry session file)))
          (expect (plist-get entry :state) :to-equal 'ready)
          (etaf-image-dired-test--write thumb "regenerated")
          (etaf-image-dired-test--set-mtime thumb 3000)
          (etaf-set-value (etaf-image-dired--session-entries-ref session)
                          (list entry))
          (etaf-image-dired--tick session)
          (let ((updated (car (etaf-image-dired-test--entries session))))
            (expect (plist-get updated :state) :to-equal 'ready)
            (expect (plist-get updated :thumb-mtime)
                    :to-equal (etaf-image-dired-test--mtime thumb)))))))

  (it "stops waiting on a stale thumbnail once image-dired gives up"
    (etaf-image-dired-test--with-tick-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (thumb (etaf-image-dired-test--write
                     (image-dired-thumb-name file))))
        (etaf-image-dired-test--set-mtime file 1000)
        (etaf-image-dired-test--set-mtime thumb 2000)
        (let ((entry (etaf-image-dired--entry session file)))
          (expect (plist-get entry :queued-at) :to-be nil)
          ;; Make the thumbnail stale, as if regeneration had been queued.
          (etaf-image-dired-test--set-mtime file 3000)
          (let ((waiting (plist-put (copy-sequence entry)
                                    :queued-at (- (float-time) 2))))
            (etaf-set-value (etaf-image-dired--session-entries-ref session)
                            (list waiting))
            (etaf-image-dired--tick session)
            (let ((updated (car (etaf-image-dired-test--entries session))))
              (expect (plist-get updated :state) :to-equal 'ready)
              (expect (plist-get updated :queued-at) :to-be nil)
              (expect (file-exists-p thumb) :to-be t)))))))

  (it "stops polling when the gallery buffer is gone"
    (etaf-image-dired-test--with-tick-session
      (setf (etaf-image-dired--session-timer session)
            (run-at-time 60 nil #'ignore))
      (kill-buffer buffer)
      (etaf-image-dired--tick session)
      (expect (etaf-image-dired--session-timer session) :to-be nil))))

;;;; Selection

(describe "card selection"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "resolves the selected ref to its file"
    (etaf-image-dired-test--with-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (entry (etaf-image-dired--entry session file))
             (ref (plist-get entry :ref)))
        (puthash ref file (etaf-image-dired--session-refs session))
        (etaf-set-value (etaf-image-dired--session-focus-ref session) ref)
        (expect (etaf-image-dired--selection-file session)
                :to-equal file))))

  (it "copies the selection into the preview while it is open"
    (etaf-image-dired-test--with-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (entry (etaf-image-dired--entry session file))
             (ref (plist-get entry :ref)))
        (puthash ref file (etaf-image-dired--session-refs session))
        (etaf-set-value (etaf-image-dired--session-split-ref session) t)
        (etaf-image-dired--set-selection session ref)
        (expect (etaf-value (etaf-image-dired--session-preview-ref session))
                :to-equal file)
        (etaf-image-dired--set-selection session nil)
        (expect (etaf-value (etaf-image-dired--session-preview-ref session))
                :to-be nil))))

  (it "leaves the preview untouched while it is closed"
    (etaf-image-dired-test--with-session
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (entry (etaf-image-dired--entry session file))
             (ref (plist-get entry :ref)))
        (etaf-set-value (etaf-image-dired--session-preview-ref session)
                        'sentinel)
        (etaf-image-dired--set-selection session ref)
        (expect (etaf-value (etaf-image-dired--session-preview-ref session))
                :to-equal 'sentinel))))

  (it "marks exactly one card's selection ref"
    (etaf-image-dired-test--with-session
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired-test--write "b.png")
      (etaf-image-dired--refresh session)
      (let ((entries (etaf-image-dired-test--entries session)))
        (etaf-image-dired-test--select session 0)
        (expect (seq-count (lambda (entry)
                             (etaf-value (plist-get entry :selected-ref)))
                           entries)
                :to-equal 1)
        (etaf-image-dired-test--select session 1)
        (expect (seq-count (lambda (entry)
                             (etaf-value (plist-get entry :selected-ref)))
                           entries)
                :to-equal 1)
        (expect (etaf-value (plist-get (nth 1 entries) :selected-ref))
                :to-be-truthy)
        (expect (etaf-value (plist-get (nth 0 entries) :selected-ref))
                :to-be nil))))

  (it "does nothing when the selected card is selected again"
    (etaf-image-dired-test--with-session
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired--refresh session)
      (let ((ref (plist-get (car (etaf-image-dired-test--entries session))
                            :ref)))
        (etaf-image-dired--set-selection session ref)
        (etaf-set-value (etaf-image-dired--session-split-ref session) t)
        (etaf-image-dired--set-selection session ref)
        (expect (etaf-value (etaf-image-dired--session-preview-ref session))
                :to-be nil))))

  (it "clears the selection when the pointer leaves a closed preview"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (etaf-set-value (etaf-image-dired--session-focus-ref session) 'card)
      (etaf-image-dired--clear-selection)
      (expect (etaf-value (etaf-image-dired--session-focus-ref session))
              :to-be nil)))

  (it "keeps the selection when the pointer leaves an open preview"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (etaf-set-value (etaf-image-dired--session-split-ref session) t)
      (etaf-set-value (etaf-image-dired--session-focus-ref session) 'card)
      (etaf-image-dired--clear-selection)
      (expect (etaf-value (etaf-image-dired--session-focus-ref session))
              :to-equal 'card)))

  (it "does nothing outside a gallery buffer"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value 'not-a-session)
      (expect (etaf-image-dired--clear-selection) :not :to-throw)))

  (it "rejects buffers that are not galleries"
    (etaf-image-dired-test--with-gallery
      (expect (etaf-image-dired--current-session) :to-throw 'user-error))))

(describe "pointer selection"
  (it "selects the card at a position"
    (etaf-image-dired-test--with-tick-session
      (spy-on 'etaf-host-ref-at :and-return-value 'card-ref)
      (puthash 'card-ref "/tmp/a.png"
               (etaf-image-dired--session-refs session))
      (etaf-image-dired--select-at-position session (point-min))
      (expect (etaf-value (etaf-image-dired--session-focus-ref session))
              :to-equal 'card-ref)))

  (it "clears the selection outside every card when the preview is closed"
    (etaf-image-dired-test--with-tick-session
      (spy-on 'etaf-host-ref-at :and-return-value nil)
      (etaf-set-value (etaf-image-dired--session-focus-ref session) 'old-card)
      (etaf-image-dired--select-at-position session (point-min))
      (expect (etaf-value (etaf-image-dired--session-focus-ref session))
              :to-be nil)))

  (it "keeps the last card selected outside every card when preview is open"
    (etaf-image-dired-test--with-tick-session
      (spy-on 'etaf-host-ref-at :and-return-value nil)
      (etaf-set-value (etaf-image-dired--session-split-ref session) t)
      (etaf-set-value (etaf-image-dired--session-focus-ref session) 'old-card)
      (etaf-image-dired--select-at-position session (point-min))
      (expect (etaf-value (etaf-image-dired--session-focus-ref session))
              :to-equal 'old-card)))

  (it "installs mouse-motion tracking in the gallery buffer"
    (etaf-image-dired-test--with-session
      (let ((buffer (generate-new-buffer " *etaf-image-dired-test*")))
        (unwind-protect
            (progn
              (etaf-image-dired--install-pointer-tracking session buffer)
              (with-current-buffer buffer
                (expect track-mouse :to-be t)
                (expect (assq 'etaf-image-dired--pointer-map
                              minor-mode-overriding-map-alist)
                        :to-be-truthy)))
          (kill-buffer buffer))))))

;;;; Preview geometry

(describe "preview width"
  (before-each
    (spy-on 'create-image :and-return-value nil))

  (it "keeps room for the thumbnail grid"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-image-dired--window-columns :and-return-value 100)
      (expect (etaf-image-dired--preview-width-bounds session)
              :to-equal '(10 . 76))))

  (it "never lets the preview starve the grid on a narrow window"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-image-dired--window-columns :and-return-value 30)
      (expect (etaf-image-dired--preview-width-bounds session)
              :to-equal '(10 . 10))
      (expect (etaf-image-dired--default-preview-width session)
              :to-equal 10)))

  (it "defaults to three fifths of the window"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-image-dired--window-columns :and-return-value 100)
      (expect (etaf-image-dired--default-preview-width session)
              :to-equal 60)
      (expect (etaf-image-dired--preview-panel-width session)
              :to-equal 60)))

  (it "uses and clamps a stored width"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-image-dired--window-columns :and-return-value 100)
      (let ((width-ref (etaf-image-dired--session-preview-width-ref session)))
        (etaf-set-value width-ref 42)
        (expect (etaf-image-dired--preview-panel-width session) :to-equal 42)
        (etaf-set-value width-ref 1000)
        (expect (etaf-image-dired--preview-panel-width session) :to-equal 76)
        (etaf-set-value width-ref 2)
        (expect (etaf-image-dired--preview-panel-width session)
                :to-equal 10))))

  (it "converts a drag gesture's pixels to columns"
    (expect (etaf-image-dired--preview-width-delta
             '(:delta (16 . 0) :window nil))
            :to-equal '(2 . 0))
    (expect (etaf-image-dired--preview-width-delta
             '(:delta (-24 . 0) :window nil))
            :to-equal '(-3 . 0))))

(describe "preview commands"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "widens and narrows by `etaf-image-dired-preview-width-step'"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'etaf-image-dired--window-columns :and-return-value 100)
      (let ((width-ref (etaf-image-dired--session-preview-width-ref session)))
        (etaf-image-dired-widen-preview)
        (expect (etaf-value width-ref) :to-equal 64)
        (etaf-image-dired-widen-preview 2)
        (expect (etaf-value width-ref) :to-equal 72)
        (etaf-image-dired-narrow-preview 3)
        (expect (etaf-value width-ref) :to-equal 60))))

  (it "clamps an adjusted width to the window"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'etaf-image-dired--window-columns :and-return-value 30)
      (etaf-image-dired-widen-preview)
      (expect (etaf-value (etaf-image-dired--session-preview-width-ref session))
              :to-equal 10)))

  (it "restores the automatic width"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'etaf-image-dired--window-columns :and-return-value 100)
      (etaf-set-value (etaf-image-dired--session-preview-width-ref session) 30)
      (etaf-image-dired-reset-preview-width)
      (expect (etaf-value (etaf-image-dired--session-preview-width-ref session))
              :to-be nil))))

;;;; Thumbnail boxes and preview scaling

(describe "thumbnail boxes"
  (before-each
    (spy-on 'create-image :and-return-value nil))

  (it "sizes every card from `image-dired-thumb-size'"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-image-dired--frame-metrics :and-return-value '(10 . 20))
      (let ((image-dired-thumb-size 128))
        (expect (etaf-image-dired--thumb-box session) :to-equal '(13 . 6)))
      (let ((image-dired-thumb-size 64))
        (expect (etaf-image-dired--thumb-box session) :to-equal '(6 . 3)))
      (let ((image-dired-thumb-size 1))
        (expect (etaf-image-dired--thumb-box session) :to-equal '(2 . 2)))))

  (it "fills the whole box when the thumbnail cannot be read"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-image-dired--frame-metrics :and-return-value '(8 . 17))
      (expect (etaf-image-dired--thumb-layout session "/nonexistent.png")
              :to-equal '(:columns 16 :rows 8
                          :inset (0 . 0) :pad-top 0))))

  (it "scales a picture to fit and centers it with pixel insets"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-image-dired--frame-metrics :and-return-value '(8 . 17))
      (spy-on 'image-size :and-return-value '(160 . 120))
      (spy-on 'create-image :and-return-value 'fake-image)
      (expect (etaf-image-dired--thumb-layout session "/fake.png")
              :to-equal '(:columns 16 :rows 6
                          :inset (3 . 3) :pad-top 1)))))

(describe "preview scaling"
  (it "reserves a short pane when nothing is selected"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-image-dired--preview-area :and-return-value '(40 . 20))
      (expect (etaf-image-dired--preview-cells session nil)
              :to-equal '(40 . 6))))

  (it "scales a large picture down to the pane"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-image-dired--frame-metrics :and-return-value '(8 . 17))
      (spy-on 'etaf-image-dired--preview-area :and-return-value '(40 . 20))
      (spy-on 'image-size :and-return-value '(800 . 400))
      (spy-on 'create-image :and-return-value 'fake-image)
      (expect (etaf-image-dired--preview-cells session "/fake.png")
              :to-equal '(40 . 9))))

  (it "does not enlarge a small picture"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-image-dired--frame-metrics :and-return-value '(10 . 20))
      (spy-on 'etaf-image-dired--preview-area :and-return-value '(40 . 20))
      (spy-on 'image-size :and-return-value '(100 . 60))
      (spy-on 'create-image :and-return-value 'fake-image)
      (expect (etaf-image-dired--preview-cells session "/fake.png")
              :to-equal '(10 . 3))))

  (it "falls back to the pane width for an unreadable picture"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-image-dired--preview-area :and-return-value '(40 . 20))
      (spy-on 'create-image :and-return-value nil)
      (expect (etaf-image-dired--preview-cells session "/broken.png")
              :to-equal '(40 . 6))))

  (it "derives the pane area from the window"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-image-dired--window-height :and-return-value 30)
      (spy-on 'etaf-image-dired--preview-panel-width :and-return-value 40)
      (expect (etaf-image-dired--preview-area session) :to-equal '(40 . 26)))))

;;;; Commands

(describe "gallery commands"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "refresh rescans the session directory"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired-refresh)
      (expect (length (etaf-image-dired-test--entries session)) :to-equal 1)))

  (it "regenerate deletes cached thumbnails before scanning again"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (thumb (etaf-image-dired-test--write
                     (image-dired-thumb-name file))))
        (etaf-image-dired-test--set-mtime file 1000)
        (etaf-image-dired-test--set-mtime thumb 2000)
        (etaf-image-dired--refresh session)
        (expect (plist-get (car (etaf-image-dired-test--entries session))
                           :state)
                :to-equal 'ready)
        (etaf-image-dired-regenerate)
        (expect (file-exists-p thumb) :to-be nil)
        (expect (plist-get (car (etaf-image-dired-test--entries session))
                           :state)
                :to-equal 'pending)
        (expect 'image-dired-create-thumb
                :to-have-been-called-with file thumb))))

  (it "opens the selected file in another window"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'find-file-other-window :and-return-value nil)
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (entry (etaf-image-dired--entry session file))
             (ref (plist-get entry :ref)))
        (puthash ref file (etaf-image-dired--session-refs session))
        (etaf-set-value (etaf-image-dired--session-focus-ref session) ref)
        (etaf-image-dired-open-at-point)
        (expect 'find-file-other-window
                :to-have-been-called-with file))))

  (it "complains when no card is selected"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (expect (etaf-image-dired-open-at-point) :to-throw 'user-error)))

  (it "toggles the preview pane for the selected image"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'etaf-image-dired--reveal-card)
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (entry (etaf-image-dired--entry session file))
             (ref (plist-get entry :ref)))
        (puthash ref file (etaf-image-dired--session-refs session))
        (etaf-set-value (etaf-image-dired--session-focus-ref session) ref)
        (etaf-image-dired-preview)
        (expect (etaf-value (etaf-image-dired--session-split-ref session))
                :to-be t)
        (expect (etaf-value (etaf-image-dired--session-preview-ref session))
                :to-equal file)
        (etaf-image-dired-preview)
        (expect (etaf-value (etaf-image-dired--session-split-ref session))
                :to-be nil)
        (expect (etaf-value (etaf-image-dired--session-preview-ref session))
                :to-be nil))))

  (it "refuses to preview when no image is selected"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (expect (etaf-image-dired-preview) :to-throw 'user-error)))

  (it "closes the application"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'etaf-app-close :and-return-value t)
      (etaf-image-dired-quit)
      (expect 'etaf-app-close :to-have-been-called))))

;;;; image-dired style commands

(describe "navigation commands"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "moves forward and backward by image"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (dotimes (index 4)
        (etaf-image-dired-test--write (format "%c.png" (+ ?a index))))
      (etaf-image-dired--refresh session)
      (let ((first (etaf-image-dired-test--select session 0)))
        (etaf-image-dired-forward-image)
        (expect (etaf-image-dired--current-file session)
                :to-equal (expand-file-name "b.png" default-directory))
        (etaf-image-dired-forward-image 2)
        (expect (etaf-image-dired--current-file session)
                :to-equal (expand-file-name "d.png" default-directory))
        (etaf-image-dired-forward-image)
        (expect (etaf-image-dired--current-file session)
                :to-equal (expand-file-name "d.png" default-directory))
        (etaf-image-dired-backward-image 3)
        (expect (etaf-image-dired--current-file session) :to-equal first))))

  (it "moves by rows once the grid has a fixed width"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (dotimes (index 6)
        (etaf-image-dired-test--write (format "%c.png" (+ ?a index))))
      (etaf-image-dired--refresh session)
      (etaf-set-value (etaf-image-dired--session-thumbs-per-row-ref session) 3)
      (etaf-image-dired-test--select session 0)
      (etaf-image-dired-next-line)
      (expect (etaf-image-dired--current-index session) :to-equal 3)
      (etaf-image-dired-move-end-of-line)
      (expect (etaf-image-dired--current-index session) :to-equal 5)
      (etaf-image-dired-move-beginning-of-line)
      (expect (etaf-image-dired--current-index session) :to-equal 3)
      (etaf-image-dired-previous-line)
      (expect (etaf-image-dired--current-index session) :to-equal 0)))

  (it "estimates the automatic columns from the window and preview"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-image-dired--window-columns :and-return-value 80)
      (spy-on 'etaf-image-dired--frame-metrics :and-return-value '(8 . 17))
      (let ((image-dired-thumb-size 128))
        ;; 16 columns per card, a 16px gap and 14px padding per side.
        (expect (etaf-image-dired--grid-columns session) :to-equal 4)
        (etaf-set-value (etaf-image-dired--session-split-ref session) t)
        (spy-on 'etaf-image-dired--preview-panel-width :and-return-value 40)
        (expect (etaf-image-dired--grid-columns session) :to-equal 1)))))

(describe "mark commands"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "marks, unmarks and flags files"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (dotimes (index 3)
        (etaf-image-dired-test--write (format "%c.png" (+ ?a index))))
      (etaf-image-dired--refresh session)
      (let ((first (etaf-image-dired-test--select session 0)))
        (etaf-image-dired-mark-thumb-original-file)
        (expect (etaf-image-dired--file-mark session first) :to-equal 'marked)
        ;; Marking moves to the next image, like image-dired.
        (expect (etaf-image-dired--current-index session) :to-equal 1)
        (etaf-image-dired-test--select session 1)
        (etaf-image-dired-flag-thumb-original-file)
        (expect (etaf-image-dired--file-mark session
                                              (expand-file-name "b.png"
                                                                default-directory))
                :to-equal 'flagged)
        (expect (length (etaf-image-dired--marked-files session)) :to-equal 1)
        (expect (length (etaf-image-dired--flagged-files session)) :to-equal 1)
        (etaf-image-dired-unmark-all-marks)
        (expect (etaf-image-dired--marks session) :to-be nil))))

  (it "deletes only the flagged files"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (let ((delete-by-moving-to-trash nil)
            ;; `dired-do-flagged-delete' asks through this confirmer.
            (dired-deletion-confirmer (lambda (&rest _) t)))
        (dotimes (index 3)
          (etaf-image-dired-test--write (format "%c.png" (+ ?a index))))
        (etaf-image-dired--refresh session)
        (let ((first (etaf-image-dired-test--select session 0))
              (second (etaf-image-dired-test--select session 1))
              (third (expand-file-name "c.png" default-directory)))
          (etaf-image-dired--set-file-mark session first 'flagged)
          (etaf-image-dired--set-file-mark session second 'flagged)
          (etaf-image-dired-do-flagged-delete)
          (expect (file-exists-p first) :to-be nil)
          (expect (file-exists-p second) :to-be nil)
          (expect (file-exists-p third) :to-be t)
          (expect (mapcar (lambda (entry) (plist-get entry :file))
                          (etaf-image-dired-test--entries session))
                  :to-equal (list third))
          (expect (etaf-image-dired--marks session) :to-be nil)))))

  (it "removes a thumbnail from the gallery without deleting the file"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (dotimes (index 3)
        (etaf-image-dired-test--write (format "%c.png" (+ ?a index))))
      (etaf-image-dired--refresh session)
      (let ((first (etaf-image-dired-test--select session 0)))
        (etaf-image-dired-delete-char)
        (expect (file-exists-p first) :to-be t)
        (expect (length (etaf-image-dired-test--entries session)) :to-equal 2)
        (expect (etaf-image-dired--hidden-files session)
                :to-equal (list first))
        ;; The next image takes the removed one's place.
        (expect (etaf-image-dired--current-file session)
                :to-equal (expand-file-name "b.png" default-directory))
        ;; Rescanning the directory restores it.
        (etaf-image-dired-refresh)
        (expect (length (etaf-image-dired-test--entries session)) :to-equal 3)
        (expect (etaf-image-dired--hidden-files session) :to-be nil)))))

(describe "dired binding"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "marks the original files in a matching Dired buffer"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (dotimes (index 3)
        (etaf-image-dired-test--write (format "%c.png" (+ ?a index))))
      (etaf-image-dired--refresh session)
      (let ((first (etaf-image-dired-test--select session 0)))
        (etaf-image-dired-mark-thumb-original-file)
        (let ((dired (etaf-image-dired--dired-buffer session)))
          (expect dired :to-be-truthy)
          (with-current-buffer dired
            ;; Dired itself sees the mark, so its own commands can act on it.
            (expect (dired-goto-file first) :to-be-truthy)
            (expect (image-dired-dired-file-marked-p) :to-be-truthy)
            (expect (dired-get-marked-files) :to-equal (list first)))))))

  (it "lets Dired undo a mark made in the gallery"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired-test--write "b.png")
      (etaf-image-dired--refresh session)
      (let ((first (etaf-image-dired-test--select session 0)))
        (etaf-image-dired-mark-thumb-original-file)
        (expect (etaf-image-dired--file-mark session first) :to-equal 'marked)
        (let ((dired (etaf-image-dired--dired-buffer session)))
          (with-current-buffer dired
            (dired-goto-file first)
            ;; The gallery changes the buffer outside Dired's command loop,
            ;; so it must delimit the change for `dired-undo' explicitly.
            (dired-undo)
            (expect (image-dired-dired-file-marked-p) :to-be nil))
          ;; Dired would run this from `post-command-hook' after the undo.
          (with-current-buffer dired
            (etaf-image-dired--sync-dired-buffer))
          (expect (etaf-image-dired--file-mark session first) :to-be nil)))))

  (it "picks up marks and point changes made in Dired"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (dotimes (index 3)
        (etaf-image-dired-test--write (format "%c.png" (+ ?a index))))
      (etaf-image-dired--refresh session)
      (let* ((second (nth 1 (etaf-image-dired--files session)))
             (dired (etaf-image-dired--ensure-dired-buffer session)))
        (with-current-buffer dired
          (dired-goto-file second)
          (dired-mark 1)
          ;; `dired-mark' moves to the next line; return to the marked image
          ;; so the point mirroring below is predictable.
          (dired-goto-file second)
          ;; A Dired command would run this through `post-command-hook'.
          (etaf-image-dired--sync-dired-buffer))
        (expect (etaf-image-dired--file-mark session second) :to-equal 'marked)
        ;; Movement tracking mirrors Dired's point into the gallery.
        (expect (etaf-image-dired--selection-file session) :to-equal second))))

  (it "unmarks everything in Dired as well"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired-test--write "b.png")
      (etaf-image-dired--refresh session)
      (etaf-image-dired-mark-thumb-original-file)
      (let ((dired (etaf-image-dired--dired-buffer session)))
        (etaf-image-dired-unmark-all-marks)
        (expect (etaf-image-dired--marks session) :to-be nil)
        (with-current-buffer dired
          (dired-goto-file (expand-file-name "a.png" default-directory))
          (expect (image-dired-dired-file-marked-p) :to-be nil)
          (expect (etaf-image-dired--dired-flagged-p) :to-be nil)))))

  (it "deletes files flagged in Dired"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (let ((delete-by-moving-to-trash nil)
            (dired-deletion-confirmer (lambda (&rest _) t)))
        (dotimes (index 3)
          (etaf-image-dired-test--write (format "%c.png" (+ ?a index))))
        (etaf-image-dired--refresh session)
        (let* ((first (nth 0 (etaf-image-dired--files session)))
               (second (nth 1 (etaf-image-dired--files session)))
               (third (nth 2 (etaf-image-dired--files session)))
               (dired (etaf-image-dired--ensure-dired-buffer session)))
          ;; Flag a file in Dired, not in the gallery.
          (with-current-buffer dired
            (dired-goto-file second)
            (dired-flag-file-deletion 1))
          (etaf-image-dired-do-flagged-delete)
          (expect (file-exists-p first) :to-be t)
          (expect (file-exists-p second) :to-be nil)
          (expect (file-exists-p third) :to-be t)
          (expect (mapcar (lambda (entry) (plist-get entry :file))
                          (etaf-image-dired-test--entries session))
                  :to-equal (list first third)))))))

(describe "display commands"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "opens the preview pane on the current image"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired--refresh session)
      (let ((file (etaf-image-dired-test--select session 0)))
        (etaf-image-dired-display-this)
        (expect (etaf-value (etaf-image-dired--session-split-ref session))
                :to-be t)
        (expect (etaf-value (etaf-image-dired--session-preview-ref session))
                :to-equal file))))

  (it "steps through images and wraps around"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (dotimes (index 3)
        (etaf-image-dired-test--write (format "%c.png" (+ ?a index))))
      (etaf-image-dired--refresh session)
      (etaf-image-dired-test--select session 0)
      (etaf-image-dired-display-next)
      (expect (etaf-image-dired--current-index session) :to-equal 1)
      (expect (etaf-value (etaf-image-dired--session-preview-ref session))
              :to-equal (expand-file-name "b.png" default-directory))
      (etaf-image-dired-display-previous)
      (expect (etaf-image-dired--current-index session) :to-equal 0)
      (etaf-image-dired-display-previous)
      (expect (etaf-image-dired--current-index session) :to-equal 2)))

  (it "starts the configured external viewer"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'start-process :and-return-value nil)
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired--refresh session)
      (let* ((image-dired-external-viewer "feh -t")
             (file (etaf-image-dired-test--select session 0)))
        (etaf-image-dired-thumbnail-display-external)
        (expect 'start-process
                :to-have-been-called-with
                "etaf-image-dired-thumb-external" nil "feh" "-t" file)))))

(describe "layout commands"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil))

  (it "switches between fixed and automatic line-up"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (let ((etaf-image-dired-thumbs-per-row 4))
        (etaf-image-dired-line-up)
        (expect (etaf-value (etaf-image-dired--session-thumbs-per-row-ref
                             session))
                :to-equal 4))
      (etaf-image-dired-line-up-interactive 2)
      (expect (etaf-value (etaf-image-dired--session-thumbs-per-row-ref session))
              :to-equal 2)
      (etaf-image-dired-line-up-dynamic)
      (expect (etaf-value (etaf-image-dired--session-thumbs-per-row-ref session))
              :to-be nil)))

  (it "rejects a non-positive interactive width"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (expect (etaf-image-dired-line-up-interactive 0)
              :to-throw 'user-error))))

(describe "metadata commands"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "tags the current image"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'read-string :and-return-value "trip")
      (spy-on 'image-dired-write-tags :and-return-value nil)
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired--refresh session)
      (let ((file (etaf-image-dired-test--select session 0)))
        (etaf-image-dired-tag-thumbnail)
        (expect 'image-dired-write-tags
                :to-have-been-called-with (list (cons file "trip"))))))

  (it "tags every marked image instead of the current one"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'read-string :and-return-value "trip")
      (spy-on 'image-dired-write-tags :and-return-value nil)
      (dotimes (index 2)
        (etaf-image-dired-test--write (format "%c.png" (+ ?a index))))
      (etaf-image-dired--refresh session)
      (let ((first (etaf-image-dired-test--select session 0))
            (second (etaf-image-dired-test--select session 1)))
        (etaf-image-dired--set-file-mark session first 'marked)
        (etaf-image-dired--set-file-mark session second 'marked)
        (etaf-image-dired-tag-thumbnail)
        (expect 'image-dired-write-tags
                :to-have-been-called-with
                (list (cons first "trip") (cons second "trip"))))))

  (it "removes a known tag"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'image-dired-list-tags :and-return-value '("trip" "family"))
      (spy-on 'completing-read :and-return-value "trip")
      (spy-on 'image-dired-remove-tag :and-return-value nil)
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired--refresh session)
      (let ((file (etaf-image-dired-test--select session 0)))
        (etaf-image-dired-tag-thumbnail-remove)
        (expect 'image-dired-remove-tag
                :to-have-been-called-with (list file) "trip"))))

  (it "writes an image comment"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'image-dired-read-comment :and-return-value "nice picture")
      (spy-on 'image-dired-write-comments :and-return-value nil)
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired--refresh session)
      (let ((file (etaf-image-dired-test--select session 0)))
        (etaf-image-dired-comment-thumbnail)
        (expect 'image-dired-write-comments
                :to-have-been-called-with
                (list (cons file "nice picture"))))))

  (it "writes the ImageDescription EXIF tag"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'exif-parse-file :and-return-value 'exif)
      (spy-on 'exif-field :and-return-value "old")
      (spy-on 'read-string :and-return-value "new description")
      (spy-on 'image-dired-set-exif-data :and-return-value 0)
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired--refresh session)
      (let ((file (etaf-image-dired-test--select session 0)))
        (etaf-image-dired-thumbnail-set-image-description)
        (expect 'image-dired-set-exif-data
                :to-have-been-called-with
                file "ImageDescription" "new description"))))

  (it "requires a non-empty tag"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'read-string :and-return-value "  ")
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired--refresh session)
      (etaf-image-dired-test--select session 0)
      (expect (etaf-image-dired-tag-thumbnail) :to-throw 'user-error))))

(describe "editing original images"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "rotates a JPEG through image-dired's rotation command"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'executable-find :and-return-value "/usr/bin/jpegtran")
      (spy-on 'image-type :and-return-value 'jpeg)
      (spy-on 'call-process :and-return-value 0)
      (spy-on 'copy-file :and-return-value nil)
      (let ((image-dired-rotate-original-ask-before-overwrite nil))
        (etaf-image-dired-test--write "a.jpg")
        (etaf-image-dired--refresh session)
        (let ((file (etaf-image-dired-test--select session 0)))
          (etaf-image-dired-rotate-original-right)
          (expect 'call-process :to-have-been-called)
          (expect 'copy-file
                  :to-have-been-called-with
                  image-dired-temp-rotate-image-file file t)))))

  (it "refuses to rotate a non-JPEG image"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'executable-find :and-return-value "/usr/bin/jpegtran")
      (spy-on 'image-type :and-return-value 'png)
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired--refresh session)
      (etaf-image-dired-test--select session 0)
      (expect (etaf-image-dired-rotate-original-left) :to-throw 'user-error)))

  (it "sets the wallpaper to the current image"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (spy-on 'wallpaper-set :and-return-value nil)
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired--refresh session)
      (let ((file (etaf-image-dired-test--select session 0)))
        (etaf-image-dired-wallpaper-set)
        (expect 'wallpaper-set :to-have-been-called-with file)))))

(describe "copying file names"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "copies all marked names"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (dotimes (index 3)
        (etaf-image-dired-test--write (format "%c.png" (+ ?a index))))
      (etaf-image-dired--refresh session)
      (let ((first (etaf-image-dired-test--select session 0))
            (third (etaf-image-dired-test--select session 2)))
        (etaf-image-dired--set-file-mark session first 'marked)
        (etaf-image-dired--set-file-mark session third 'marked)
        (etaf-image-dired-copy-filename-as-kill)
        (expect (car kill-ring) :to-equal "a.png\nc.png"))))

  (it "copies the current name, or absolute names with prefix 0"
    (etaf-image-dired-test--with-session
      (spy-on 'etaf-app-state :and-return-value session)
      (let ((file (etaf-image-dired-test--write "a.png")))
        (etaf-image-dired--refresh session)
        (etaf-image-dired-test--select session 0)
        (etaf-image-dired-copy-filename-as-kill)
        (expect (car kill-ring) :to-equal "a.png")
        (etaf-image-dired-copy-filename-as-kill 0)
        (expect (car kill-ring) :to-equal file)))))

(describe "slideshow"
  (before-each
    (spy-on 'image-dired-create-thumb :and-return-value nil)
    (spy-on 'create-image :and-return-value nil))

  (it "starts, pauses and stops"
    (etaf-image-dired-test--with-tick-session
      (spy-on 'etaf-app-state :and-return-value session)
      (dotimes (index 3)
        (etaf-image-dired-test--write (format "%c.png" (+ ?a index))))
      (etaf-image-dired--refresh session)
      (etaf-image-dired-test--select session 0)
      (etaf-image-dired-slideshow-start 3600)
      (expect (timerp (etaf-image-dired--session-slideshow-timer session))
              :to-be t)
      (expect (memq 'etaf-image-dired--slideshow-post-command
                    (with-current-buffer buffer post-command-hook))
              :to-be-truthy)
      (expect (etaf-value (etaf-image-dired--session-split-ref session))
              :to-be t)
      ;; RET pauses the slideshow but keeps the hook installed.
      (let ((this-command 'etaf-image-dired-display-this))
        (etaf-image-dired--slideshow-post-command))
      (expect (etaf-image-dired--session-slideshow-timer session) :to-be nil)
      (expect (memq 'etaf-image-dired--slideshow-post-command
                    (with-current-buffer buffer post-command-hook))
              :to-be-truthy)
      ;; Any other command stops it for good.
      (let ((this-command 'some-other-command))
        (etaf-image-dired--slideshow-post-command))
      (expect (memq 'etaf-image-dired--slideshow-post-command
                    (with-current-buffer buffer post-command-hook))
              :to-be nil)
      (expect (etaf-image-dired--session-slideshow-timer session) :to-be nil)))

  (it "stops when the start command runs again"
    (etaf-image-dired-test--with-tick-session
      (spy-on 'etaf-app-state :and-return-value session)
      (etaf-image-dired-test--write "a.png")
      (etaf-image-dired--refresh session)
      (etaf-image-dired-test--select session 0)
      (etaf-image-dired-slideshow-start 3600)
      (expect (etaf-image-dired--session-slideshow-timer session) :to-be-truthy)
      (etaf-image-dired-slideshow-start)
      (expect (etaf-image-dired--session-slideshow-timer session) :to-be nil))))

(describe "thumbnail mode key bindings"
  (it "uses only key sequences ETAF accepts"
    (expect (etaf--keys-validate etaf-image-dired--keys) :not :to-throw))

  (it "binds the image-dired thumbnail commands"
    (let ((bindings (mapcar (lambda (entry)
                              (cons (kbd (car entry)) (nth 1 entry)))
                            etaf-image-dired--keys)))
      (dolist (pair '(("f" . etaf-image-dired-forward-image)
                      ("b" . etaf-image-dired-backward-image)
                      ("n" . etaf-image-dired-next-line)
                      ("p" . etaf-image-dired-previous-line)
                      ("a" . etaf-image-dired-move-beginning-of-line)
                      ("e" . etaf-image-dired-move-end-of-line)
                      ("m" . etaf-image-dired-mark-thumb-original-file)
                      ("u" . etaf-image-dired-unmark-thumb-original-file)
                      ("U" . etaf-image-dired-unmark-all-marks)
                      ("d" . etaf-image-dired-flag-thumb-original-file)
                      ("x" . etaf-image-dired-do-flagged-delete)
                      ("C-d" . etaf-image-dired-delete-char)
                      ("RET" . etaf-image-dired-display-this)
                      ("SPC" . etaf-image-dired-display-next)
                      ("DEL" . etaf-image-dired-display-previous)
                      ("L" . etaf-image-dired-rotate-original-left)
                      ("R" . etaf-image-dired-rotate-original-right)
                      ("D" . etaf-image-dired-thumbnail-set-image-description)
                      ("S" . etaf-image-dired-slideshow-start)
                      ("c" . etaf-image-dired-comment-thumbnail)
                      ("w" . etaf-image-dired-copy-filename-as-kill)
                      ("W" . etaf-image-dired-wallpaper-set)
                      ("t" . etaf-image-dired-tag-thumbnail)
                      ("T" . etaf-image-dired-tag-thumbnail-remove)
                      ("G" . etaf-image-dired-line-up-dynamic)
                      ("F" . etaf-image-dired-line-up)
                      ("I" . etaf-image-dired-line-up-interactive)
                      ("J" . etaf-image-dired-jump-original-dired-buffer)
                      ("." . etaf-image-dired-track-original-file)))
        (expect (cdr (assoc (kbd (car pair)) bindings))
                :to-equal (cdr pair))))))

;;;; The application itself

(describe "the etaf-image-dired command"
  (it "opens a gallery buffer for the current directory"
    (etaf-image-dired-test--with-gallery
      (spy-on 'image-dired-create-thumb :and-return-value nil)
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (expected-name
              (format "*etaf-image-dired: %s*"
                      (abbreviate-file-name default-directory)))
             (buffer (etaf-image-dired default-directory))
             session)
        (unwind-protect
            (with-current-buffer buffer
              (setq session (etaf-image-dired--current-session))
              (expect (buffer-name) :to-equal expected-name)
              (expect (type-of session)
                      :to-equal 'etaf-image-dired--session)
              (let ((entries (etaf-image-dired-test--entries session)))
                (expect (length entries) :to-equal 1)
                (expect (plist-get (car entries) :file) :to-equal file)
                (expect (plist-get (car entries) :thumb)
                        :to-equal (image-dired-thumb-name file)))
              (expect (string-match-p "a\\.png" (buffer-string))
                      :to-be-truthy)
              (expect (string-match-p "1 生成中" (buffer-string))
                      :to-be-truthy))
          (when (buffer-live-p buffer)
            (etaf-app-close buffer))
          (when session
            (expect (etaf-image-dired--session-timer session) :to-be nil))))))

  (it "picks up new images when the refresh command runs"
    (etaf-image-dired-test--with-gallery
      (spy-on 'image-dired-create-thumb :and-return-value nil)
      (etaf-image-dired-test--write "a.png")
      (let ((buffer (etaf-image-dired default-directory)))
        (unwind-protect
            (with-current-buffer buffer
              (etaf-image-dired-test--write "b.png")
              (etaf-image-dired-refresh)
              (expect (length (etaf-image-dired-test--entries
                               (etaf-image-dired--current-session)))
                      :to-equal 2))
          (when (buffer-live-p buffer)
            (etaf-app-close buffer))))))

  (it "adopts marks from an existing Dired buffer"
    (etaf-image-dired-test--with-gallery
      (spy-on 'image-dired-create-thumb :and-return-value nil)
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (dired (dired-noselect default-directory)))
        (unwind-protect
            (progn
              (with-current-buffer dired
                (dired-goto-file file)
                (dired-mark 1))
              (let ((buffer (etaf-image-dired default-directory)))
                (unwind-protect
                    (with-current-buffer buffer
                      (expect (etaf-image-dired--file-mark
                               (etaf-image-dired--current-session) file)
                              :to-equal 'marked))
                  (when (buffer-live-p buffer)
                    (etaf-app-close buffer)))))
          (when (buffer-live-p dired)
            (kill-buffer dired))))))

  (it "does not highlight the current line"
    (etaf-image-dired-test--with-gallery
      (spy-on 'image-dired-create-thumb :and-return-value nil)
      (require 'hl-line)
      (let ((was-global global-hl-line-mode)
            buffer)
        (unwind-protect
            (progn
              (global-hl-line-mode 1)
              (setq buffer (etaf-image-dired default-directory))
              (with-current-buffer buffer
                ;; `hl-line' documents a buffer-local nil as the way to opt
                ;; out of the global mode.
                (expect (buffer-local-value 'global-hl-line-mode buffer)
                        :to-be nil)
                (expect hl-line-mode :to-be nil)
                (expect (overlayp global-hl-line-overlay) :to-be nil)
                (expect (overlayp hl-line-overlay) :to-be nil)))
          (when (and buffer (buffer-live-p buffer))
            (etaf-app-close buffer))
          (global-hl-line-mode (if was-global 1 -1))))))

  (it "opens the selected image from the mounted gallery"
    (etaf-image-dired-test--with-gallery
      (spy-on 'image-dired-create-thumb :and-return-value nil)
      (spy-on 'find-file-other-window :and-return-value nil)
      (let* ((file (etaf-image-dired-test--write "a.png"))
             (buffer (etaf-image-dired default-directory)))
        (unwind-protect
            (with-current-buffer buffer
              (let* ((session (etaf-image-dired--current-session))
                     (entry (car (etaf-image-dired-test--entries session))))
                (etaf-set-value (etaf-image-dired--session-focus-ref session)
                                (plist-get entry :ref))
                (etaf-image-dired-open-at-point)
                (expect 'find-file-other-window
                        :to-have-been-called-with file)))
          (when (buffer-live-p buffer)
            (etaf-app-close buffer)))))))

;;; etaf-image-dired-test.el ends here
