;;; hugo.el --- An interactive interface for the Hugo static site generator.

;; Copyright (C) 2019 Aaron Bieber

;; Author: Aaron Bieber <aaron@aaronbieber.com>
;; Version: 1.0
;; Package-Requires ((cl-lib "0.5"))
;; Keywords: hugo, blog
;; URL: https://github.com/aaronbieber/hugo.el

;;; Commentary:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defface hugo-option-on
  '((t (:inherit 'font-lock-string-face)))
  "An Hugo interactive option when on."
  :group 'hugo)

(defface hugo-option-off
  '((t (:inherit 'font-lock-warning-face)))
  "An Hugo interactive option when off."
  :group 'hugo)

(defface hugo-highlight-line-face
  '((((background dark)) :background "#323878")
    (((background light)) :background "#C7CAF2"))
  "Face used to highlight the active line."
  :group 'hugo)

(defvar hugo-highlight-current-line-overlay
  ;; Dummy initialization
  (make-overlay 1 1)
  "Overlay for highlighting the current line.")

(overlay-put hugo-highlight-current-line-overlay
             'face 'hugo-highlight-line-face)

(defvar hugo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'hugo-toggle-command-window)
    (define-key map "q" 'hugo-status-quit)
    (define-key map "s" 'hugo-start-stop-server)
    (define-key map "g" 'hugo-refresh-status)
    (define-key map "c" 'hugo-create-thing)
    ;;(define-key map "d" 'hugo-deploy) ;; unsupported
    (define-key map "b" 'hugo-build)
    (define-key map "$" 'hugo-show-server)
    (define-key map "!" 'hugo-show-process)
    (define-key map "n" 'hugo-move-to-next-thing)
    (define-key map "p" 'hugo-move-to-previous-thing)
    (define-key map (kbd "C-n") 'hugo-move-to-next-heading)
    (define-key map (kbd "C-p") 'hugo-move-to-previous-heading)
    (define-key map (kbd "TAB") 'hugo-maybe-toggle-visibility)
    (define-key map (kbd "RET") 'hugo-open-at-point)
    map)
  "Get the keymap for the Hugo status buffer.")

(defvar hugo-server-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'hugo-server-quit)
    map)
  "Get the keymap for the Hugo server buffer.")

(defvar hugo-process-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'hugo-process-quit)
    map)
  "Get the keymap for the Hugo process buffer.")

(defvar hugo-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h s") 'hugo-status)
    (define-key map (kbd "C-c h p") 'hugo-insert-post-url)
    (define-key map (kbd "C-c h i") 'hugo-insert-image-url)
    (define-key map (kbd "C-c h b") 'hugo-browse)
    map)
  "A minor mode for interacting with Hugo.")

(defvar hugo-content-filename-rx
  (rx bol
      (not (any "."))
      (or (group (*? nonl) ".md" eol)
          (group (*? nonl) ".markdown" eol)
          (group (*? nonl) ".goldmark" eol)
          (group (*? nonl) ".mmark" eol)
          (group (*? nonl) ".org" eol)
          (group (*? nonl) ".asciidoc" eol)
          (group (*? nonl) ".adoc" eol)
          (group (*? nonl) ".ad" eol)
          (group (*? nonl) ".rst" eol)
          (group (*? nonl) ".pandoc" eol)
          (group (*? nonl) ".pdc" eol)
          (group (*? nonl) ".html" eol)
          (group (*? nonl) ".htm" eol)))
  "A regular expression matching any Hugo content format filename.")

(defvar hugo-last-image-path ""
  "The fully-qualified path of the last image inserted.")

(defvar hugo-server-address
  "http://localhost:1313"
  "The base address of the Hugo development server.")

(define-minor-mode hugo-minor-mode
  "A minor mode for interacting with Hugo.

Enable this mode when you are editing blog files and wish to use these
Hugo-specific convenience functions. For example, you may want to
enable this mode in `markdown-mode'."
  nil " Hugo"
  hugo-minor-mode-map)

;;; Customization
(defcustom hugo-bin "hugo"
  "Hugo binary."
  :group 'hugo
  :type 'string)

(defcustom hugo-posts-directory
  "content/posts"
  "Directory containing posts, relative to /path/to/hugo-site/.

The Hugo package will assume that the final segment of this path is
your post `type' and provide that to the `hugo new' command, unless
you have configured `hugo-post-type'."
  :type 'string
  :group 'hugo)

(defcustom hugo-static-image-path
  "static"
  "The relative path to your static image location.

This path is relative to the site root and should correspond to the
value of, or value of one entry in, the Hugo `staticDir' configuration
variable.

If you haven't configured `staticDir', you shouldn't need to touch
this, either."
  :type 'string
  :group 'hugo)

(defcustom hugo-post-type
  nil
  "The default `type' for a post.

This value is used to tell Hugo which archetype to use when creating a
new post (if it exists).  If nil, the type will be discerned from the
last path segment of `hugo-posts-directory' in keeping with the
standard behavior."
  :type 'string
  :group 'hugo)

(defcustom hugo-post-extension
  ".md"
  "The default extension to use for new content files."
  :type 'string
  :group 'hugo)

(defcustom hugo-default-build-flags
  '()
  "The default flags to pass to `hugo'.

Each option is a type of post that is normally excluded from a Hugo
build.  The checked options will be enabled by default in the
interactive prompt."
  :type    '(set (const :tag "Drafts" drafts)
                 (const :tag "Posts with future dates" future)
                 (const :tag "Expired posts" expired))
  :group   'hugo)

(defcustom hugo-default-server-flags
  '(drafts)
  "The default flags to pass to `jekyll serve'.

Each option is a type of post that is normally ignored by the Hugo
server.  The checked options will be enabled by default in the
interactive prompt to start the server."
  :type    '(set (const :tag "Drafts" drafts)
                 (const :tag "Posts with future dates" future)
                 (const :tag "Expired posts" expired))
  :group   'hugo)

(defcustom hugo-blog-root
  ""
  "The default location of your Hugo site.

This variable is optional and, if you have more than one Hugo
site, not recommended.  If a value is supplied, it will be used as the
location of your Hugo site every time `hugo' is initialized
in a new Emacs session.  You will never be prompted for a site location
and the location of any currently open buffer will be ignored."
  :type 'string
  :group 'hugo)

;;; "Public" functions

;;;###autoload
(defun hugo-status ()
  "The main entry point into hugo."
  (interactive)
  (let ((hugo-buffer (hugo--setup)))
    (if hugo-buffer
        (progn (hugo--draw-status hugo-buffer)
               (pop-to-buffer hugo-buffer)))))

(defun hugo-refresh-status ()
  "Refresh the status display."
  (interactive)
  (hugo-toggle-command-window t)
  (hugo--maybe-redraw-status))

(defun hugo-create-thing ()
  "Present a menu through which the user may create a new thing."
  (interactive)
  (hugo--new-post))

(defun hugo-status-quit ()
  "Quit the Hugo status window, preserving its buffer."
  (interactive)
  (hugo-toggle-command-window t)
  (quit-window t))

(defun hugo-server-quit ()
  "Quit the Hugo Server window, preserving its buffer."
  (interactive)
  (quit-window))

(defun hugo-process-quit ()
  "Quit the Hugo Process window, preserving its buffer."
  (interactive)
  (quit-window))

(defun hugo-move-to-next-thing ()
  "Move point to the next item with property 'thing."
  (interactive)
  (hugo--move-to-next-visible-thing))

(defun hugo-move-to-next-heading ()
  "Move point to the next item with property 'heading."
  (interactive)
  (hugo--move-to-next-prop 'heading))

(defun hugo-move-to-previous-thing ()
  "Move to the previous item with property 'thing."
  (interactive)
  (hugo--move-to-next-visible-thing t))

(defun hugo-move-to-previous-heading ()
  "Move to the previous item with property 'heading."
  (interactive)
  (hugo--move-to-previous-prop 'heading))

(defun hugo-open-at-point ()
  "Open the file at point, if there is one."
  (interactive)
  (let* ((type (hugo--get-line-type))
         (filename (hugo--get-line-filename))
         (full-filename (hugo--expand-path-for-type filename type)))
    (if (and type
             (file-exists-p full-filename))
        (pop-to-buffer (find-file full-filename)))))

(defun hugo-maybe-toggle-visibility ()
  "If point is on something that can be shown or hidden, do so."
  (interactive)
  (let ((hidden (get-text-property (line-beginning-position) 'hidden)))
    (if hidden
        (if (memq hidden buffer-invisibility-spec)
            (remove-from-invisibility-spec hidden)
          (add-to-invisibility-spec hidden))))
  (force-window-update (current-buffer)))

(defun hugo-insert-image-url ()
  "Read the file name of an image and insert its relative path."
  (interactive)
  (let* ((root (hugo--get-root))
         (static-root (concat (file-name-as-directory root)
                              (file-name-as-directory hugo-static-image-path)))
         (browse-root (concat (file-name-as-directory root)
                              (if (string= hugo-last-image-path "")
                                  (file-name-as-directory hugo-static-image-path)
                                (file-name-directory hugo-last-image-path))))
         (fname (read-file-name "Insert path to: " browse-root)))
    (if fname
        (progn
          (setq hugo-last-image-path fname)
          (insert (concat "/" (file-relative-name fname static-root))))
      (message "No file selected!"))))

(defun hugo-insert-post-url ()
  "Read the filename of a post and insert a ref shortcode for it."
  (interactive)
  (let* ((root (hugo--get-root))
         (browse-root (concat (file-name-as-directory root)
                              (file-name-as-directory hugo-posts-directory)))
         (fname (read-file-name "Insert permalink to: " browse-root)))
    (if fname
        (insert (concat "{{< ref \"" (file-name-base fname) "\" >}}"))
      (message "No file selected!"))))

(defun hugo-toggle-command-window (&optional hide)
  "Toggle the display of a helpful command window.

If the optional HIDE argument is not nil, hide the command window if
it exists and do nothing otherwise."
  (interactive)
  (let* ((buffer-name (hugo--buffer-name-for-type "command"))
         (command-buffer (get-buffer-create buffer-name))
         (command-window (get-buffer-window command-buffer)))
    (if command-window
        (delete-window command-window)
      (if (not hide)
          (progn
            (hugo--draw-command-help command-buffer)
            (split-window-below)
            (set-window-buffer (next-window) command-buffer)
            (fit-window-to-buffer (next-window)))))))

(defun hugo-start-stop-server ()
  "Start or stop the server based on user input."
  (interactive)
  (let* ((config (hugo--read-char-with-toggles
                  "[s] Server, [k] Kill, [q] Abort"
                  '(?s ?k ?q)
                  hugo-default-server-flags))
         (choice (cdr (assoc 'choice config)))
         (drafts (cdr (assoc 'drafts config)))
         (future (cdr (assoc 'future config)))
         (expired (cdr (assoc 'expired config))))
    (if choice
        (cond ((eq choice ?s)
               (hugo-toggle-command-window t)
               (hugo--start-server-process drafts future expired))
              ((eq choice ?k)
               (progn (hugo-toggle-command-window t)
                      (message "Stopping server...")
                      (hugo--stop-server-process)))))))

(defun hugo-browse ()
  "Open the current Hugo blog in a browser."
  (interactive)
  (if (not (hugo--server-status))
      (message "The Hugo server is not running.")
    (if (not hugo-server-address)
        (message "Could not find the server's address; try restarting the server.")
      (browse-url hugo-server-address))))

(defun hugo-build ()
  "Initiate a Hugo build upon interactive confirmation."
  (interactive)
  (let* ((config (hugo--read-char-with-toggles
                  "[b] Build, [q] Abort"
                  '(?b ?q)
                  hugo-default-build-flags))
         (choice (cdr (assoc 'choice config)))
         (drafts (cdr (assoc 'drafts config)))
         (future (cdr (assoc 'future config)))
         (expired (cdr (assoc 'expired config))))
    (when (eq choice ?b)
      (progn
        (hugo-toggle-command-window t)
        (hugo--start-build-process drafts future expired)))))

(defun hugo-show-server ()
  "Pop to the server output buffer."
  (interactive)
  (hugo-toggle-command-window t)
  (pop-to-buffer (hugo--prepare-server-buffer)))

(defun hugo-show-process ()
  "Pop to the process output buffer."
  (interactive)
  (hugo-toggle-command-window t)
  (pop-to-buffer (hugo--prepare-process-buffer)))

;;; "Private" functions
(defun hugo--setup ()
  "Stuff that has to happen before anything else can happen."
  ;; Only set up if we have to...
  (let ((hugo-buffer (get-buffer (hugo--buffer-name-for-type "status"))))
    (if (hugo--buffer-is-configured hugo-buffer)
        hugo-buffer
      (setq hugo-root (hugo--get-root))
      (let* ((hugo-buffer (hugo--prepare-status-buffer)))
        (if (and hugo-buffer hugo-root)
            (progn (with-current-buffer hugo-buffer
                     (make-local-variable 'hugo-root))
                   hugo-buffer)
          (progn (kill-buffer hugo-buffer)
                 nil))))))

(defun hugo--buffer-name-for-type (type)
  "Return a buffer name for the provided TYPE."
  (concat "*hugo-" type "*"))

(defun hugo--buffer-is-configured (buffer)
  "Return t if BUFFER is configured properly for Hugo."
  (and (bufferp buffer)
       (let ((vars (buffer-local-variables
                    (get-buffer buffer))))
         (and (assoc 'hugo-root vars)
              (string= (cdr (assoc 'major-mode vars)) "hugo-mode")))))

(defun hugo--get-root ()
  "Maybe return the root of the Hugo site.

If `hugo-blog-root' has a value, it is assumed to be the correct
blog root.

Otherwise, we assume we are running from a buffer editing a file
somewhere within the site.  If we are running from some other kind of
buffer, or a buffer with no file, the user will be prompted to enter
the path to an Hugo site."
    (let ((status-buffer (get-buffer (hugo--buffer-name-for-type "status")))
          (this-dir (if (and (boundp 'dired-directory) dired-directory)
                        dired-directory
                      (if (buffer-file-name (current-buffer))
                          (file-name-directory (buffer-file-name (current-buffer)))))))
      (if (and (bufferp status-buffer)
               (assoc 'hugo-root (buffer-local-variables status-buffer))
               (buffer-local-value 'hugo-root status-buffer))
          (buffer-local-value 'hugo-root status-buffer)
        (or (and this-dir
                 (let ((candidate-dir (vc-find-root this-dir "config.toml")))
                   (if candidate-dir (expand-file-name candidate-dir) nil)))
            (and (not (string= "" hugo-blog-root)) hugo-blog-root)
            (let ((candidate-dir (read-directory-name "Hugo site root: ")))
              (if (file-exists-p (expand-file-name "config.toml" candidate-dir))
                  (expand-file-name candidate-dir)
                (prog2 (message "Could not find config.toml in `%s'." candidate-dir)
                    nil)))))))

(defun hugo--prepare-status-buffer ()
  "Return the Hugo (\"status\") buffer.

If the buffer doesn't exist yet, it will be created and prepared."
  (let ((buffer-name (hugo--buffer-name-for-type "status")))
    (or (get-buffer buffer-name)
        (let ((status-buffer (hugo--prepare-buffer-for-type "status" 'hugo-mode)))
          (with-current-buffer status-buffer
            (add-to-invisibility-spec 'posts)
            (add-to-invisibility-spec 'exiled))
          status-buffer))))

(defun hugo--prepare-buffer-for-type (type &optional mode-function)
  "Prepare an empty buffer for TYPE and optionally run MODE-FUNCTION."
  (let ((buffer-name (hugo--buffer-name-for-type type)))
    (if (bufferp buffer-name)
        (get-buffer buffer-name)
      (let ((buf (get-buffer-create buffer-name)))
        (with-current-buffer buf
          (setq buffer-read-only t)
          (kill-all-local-variables)
          (if (functionp mode-function)
              (funcall mode-function)))
        buf))))

(defun hugo--draw-status (buffer)
  "Draw a display of STATUS in BUFFER.

STATUS is an alist of status names and their printable values."
  (let ((status (hugo--get-status-data buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (window (get-buffer-window))
            (pos (point)))
        (erase-buffer)
        (insert
         (propertize "Hugo Status\n" 'face '(:inherit font-lock-constant-face :height 160))
         "\n"
         (propertize " " 'thing t 'heading t)
         (propertize "   Blog root: " 'face 'font-lock-function-name-face)
         hugo-root "\n"

         (propertize " " 'thing t 'heading t)
         (propertize "      Server: " 'face 'font-lock-function-name-face)
         (cdr (assoc 'server-status status)) "\n"

         (propertize " " 'thing t 'hidden 'drafts 'heading t)
         (propertize "      Drafts: " 'face 'font-lock-function-name-face)
         (number-to-string (cdr (assoc 'drafts-count status))) "\n"
         (hugo--get-display-list (cdr (assoc 'drafts-list status)) 'drafts)

         (propertize " " 'thing t 'hidden 'posts 'heading t)
         (propertize "       Posts: " 'face 'font-lock-function-name-face)
         (number-to-string (cdr (assoc 'posts-count status))) "\n"
         (hugo--get-display-list (cdr (assoc 'posts-list status)) 'posts)

         "\n"
         "Press `?' for help.")
        (goto-char (if (< pos (point-max))
                       pos
                     (point-min)))
        (if window
            (force-window-update window))))))

(defun hugo--draw-command-help (buffer)
  "Output the help menu into BUFFER.

Note that BUFFER's contents will be destroyed."
  (with-current-buffer buffer
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (hugo--legend-item "C-n" "Next section" 18)
       (hugo--legend-item "C-p" "Prev section" 18)
       (hugo--legend-item "n" "Next thing" 18)
       (hugo--legend-item "p" "Prev thing" 18) "\n"
       (hugo--legend-item "TAB" "Toggle thing" 18)
       (hugo--legend-item "RET" "Open thing" 18) "\n\n"
       (hugo--legend-item "c" "Create" 18)
       (hugo--legend-item "s" "Server" 18)
       (hugo--legend-item "b" "Build" 18)
       ;;(hugo--legend-item "d" "Deploy" 18) ;; unsupported
       (hugo--legend-item "g" "Refresh" 18) "\n"
       (hugo--legend-item "!" "Show Process" 18)
       (hugo--legend-item "$" "Show Server" 18)
       (hugo--legend-item "q" "Quit" 18))
      (goto-char (point-min)))))

(defun hugo--legend-item (key label column-width)
  "Format a KEY with LABEL in a COLUMN-WIDTH column, for use in the legend."
  (let ((pad (- column-width (+ (length key) (length label) 2))))
    (concat
     (propertize key 'face 'font-lock-keyword-face) ": "
     label
     (make-string pad ? ))))

(defun hugo--get-status-data (buffer)
  "Return statistics about the Hugo site linked to BUFFER.

This function can only be called after `hugo-status' has been run
and must be passed the resulting BUFFER."
  (hugo--setup)
  (let* ((post-items (hugo--get-post-items))
         (posts-list (hugo--get-posts post-items))
         (drafts-list (hugo--get-drafts post-items)))
    (with-current-buffer buffer
      `((posts-count . ,(length posts-list))
        (posts-list . ,posts-list)
        (drafts-count . ,(length drafts-list))
        (drafts-list . ,drafts-list)
        (server-status . ,(hugo--server-status-string))))))

(defun hugo--get-display-list (things visibility-name)
  "A helper to create a text column of THINGS.

VISIBILITY-NAME will be applied to the `invisible' property of all
items in this list, allowing them to be shown or hidden as a group."
  (let ((thing-list ""))
    (cl-loop for thing in things do
             (setq thing-list
                   (concat thing-list
                           (propertize " " 'thing t)
                           (make-string 10 ? ) thing "\n")))
    (propertize thing-list 'invisible visibility-name)))

(defun hugo--get-content-items ()
  "Get all content items as structured data."
  (hugo--setup)
  (let* ((default-directory (hugo--get-root))
         (source (split-string
                  (with-temp-buffer
                    (let ((ret (call-process-shell-command (concat hugo-bin " list all") nil t)))
                      (unless (zerop ret)
                        (error (concat "'" hugo-bin " list all' exited abnormally: " (buffer-string))))
                      (buffer-string)))
                  "\n"))
         (items
          (mapcar (lambda (line) (cons
                                  (concat (file-name-directory (car line))
                                          (file-name-nondirectory (car line)))
                                  (cdr line)))
                  (cdr (mapcar (lambda (line) (split-string line ",")) source)))))
    items))

(defun hugo--get-post-items ()
  "Get all post/draft items as lists of bare filenames."
  (let* ((content-list (hugo--get-content-items))
         (valid-posts-list (seq-filter
                            (lambda (item) (and (equal (file-name-as-directory hugo-posts-directory)
                                                       (file-name-directory (nth 0 item)))))
                            content-list)))
    (list (list 'drafts (mapcar
                         (lambda (item) (file-name-nondirectory (car item)))
                         (seq-filter
                          (lambda (item) (equal (nth 6 item) "true"))
                          valid-posts-list)))
          (list 'posts (mapcar
                        (lambda (item) (file-name-nondirectory (car item)))
                        (seq-filter (lambda (item) (equal (nth 6 item) "false"))
                                    valid-posts-list))))))

(defun hugo--get-posts (content-list)
  "Get a list of posts files from the CONTENT-LIST assoc."
  (cadr (assoc 'posts content-list)))

(defun hugo--get-drafts (content-list)
  "Get a list of drafts files from the CONTENT-LIST assoc."
  (cadr (assoc 'drafts content-list)))

(defun hugo--get-articles-in-dir-by-date-desc (dir)
  "Get files in the blog subdir DIR in descending order by date."
  (mapcar #'car
          (sort (directory-files-and-attributes
                 (expand-file-name dir hugo-root)
                 nil
                 hugo-content-filename-rx)
                #'(lambda (f1 f2) (time-less-p (nth 6 f2) (nth 6 f1))))))

(defun hugo--server-status-string ()
  "Return the server's status as an English word.

Returns 'Running' or 'Stopped', appropriately."
  (if (hugo--server-status)
      "Running"
    "Stopped"))

(defun hugo--server-status ()
  "Return the status of the server (whether it is running).

Function returns t if the server appears to be running, nil
otherwise."
  (let ((server-process (get-buffer-process (hugo--buffer-name-for-type "server"))))
    (and (processp server-process)
         (string= (process-status server-process) "run"))))

(define-derived-mode hugo-mode nil "Hugo"
  "The major mode for interacting with a Jekyll site.

The following keys are available in `hugo-mode':

  \\{hugo-mode-map}"
  (setq truncate-lines t)
  (add-hook 'post-command-hook 'hugo--highlight-current-line nil t))

(define-derived-mode hugo-server-mode nil "Hugo[Server]"
  "The major mode for interacting with a Jekyll server process.

The following keys are available in `hugo-server-mode':

  \\{hugo-server-mode-map}"
  (setq truncate-lines t))

(define-derived-mode hugo-process-mode nil "Hugo[Process]"
  "The major mode for interacting with Hugo and Jekyll shell commands.

The following keys are available in `hugo-process-mode':

  \\{hugo-server-mode-map}"
  (setq truncate-lines t))

(defun hugo--read-char-with-toggles (prompt-suffix choices &optional default-to-on)
  "Toggle options on and off interactively.

Display a fixed menu of toggles followed by PROMPT-SUFFIX.  Accept any of
the default choices (d, f, e, q) as well as the supplied CHOICES, which
should be provided as a list of characters (not strings).

If any of the symbols `drafts', `future', or `expired' are present in
the DEFAULT-TO-ON list, those toggles will be turned on initially.

This function returns the char value from CHOICES selected by the user."
  (let ((choices (append choices '(?d ?f ?e ?q)))
        (drafts (memq 'drafts default-to-on))
        (future (memq 'future default-to-on))
        (expired (memq 'expired default-to-on))
        return done)
    (while (not done)
      (let* ((prompt (concat (propertize "(" 'face 'default)
                             (propertize "[d]rafts " 'face (if drafts 'hugo-option-on 'hugo-option-off))
                             (propertize "[f]uture " 'face (if future 'hugo-option-on 'hugo-option-off))
                             (propertize "[e]expired" 'face (if expired 'hugo-option-on 'hugo-option-off))
                             ") " prompt-suffix))
             (choice (read-char-choice prompt choices)))
        (cond ((eq choice ?d)
               (setq drafts (not drafts)
                     done nil))
              ((eq choice ?f)
               (setq future (not future)
                     done nil))
              ((eq choice ?e)
               (setq expired (not expired)
                     done nil))
              ((eq choice ?q)
               (setq done t)
               (message "Aborted."))
              (t (setq return `((choice . ,choice)
                                (drafts . ,drafts)
                                (future . ,future)
                                (expired . ,expired))
                       done t)))))
  return))

(defun hugo--new-post ()
  "Call the new post command."
  (hugo-toggle-command-window t)
  (let ((name (read-string "Post name: ")))
    (hugo--run-hugo-command
     (concat "new -k " (hugo--get-post-type) " "
             (file-name-as-directory hugo-posts-directory)
             (hugo--create-content-filename name)))))

(defun hugo--get-post-type ()
  "Get the default type for a post."
  (or hugo-post-type
      (progn (string-match (rx (group (* (not (any "\\/")))) eol) hugo-posts-directory)
             (match-string 1 hugo-posts-directory))))

(defun hugo--create-content-filename (name)
  "Create a Hugo content filename from a human-readable NAME.

For example, given the name `My First Post', return `my-first-post'."
       (concat (replace-regexp-in-string
                "^[^a-z0-9]" ""
                (replace-regexp-in-string "[^a-z0-9-]" "-" (downcase name)))
               hugo-post-extension))

(defun hugo--run-hugo-command (command)
  "Run an Hugo COMMAND."
  (message "Running Hugo...")
  (hugo--run-command (concat "hugo " command)))

(defun hugo--run-command (command)
  "Run an Hugo-related COMMAND, sending output to the process buffer.

Returns the process object."
  (hugo--setup)
  (let ((pbuffer (hugo--prepare-process-buffer)))
    (with-current-buffer pbuffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (concat "Running `" command "'...\n\n") 'face 'font-lock-variable-name-face))))
    (let* ((default-directory (hugo--get-root))
           (process (start-process-shell-command
                     "hugo"
                     pbuffer
                     command)))
      (set-process-sentinel process 'hugo--hugo-sentinel)
      (set-process-filter process 'hugo--generic-process-filter)
      process)))

(defun hugo--hugo-sentinel (process event)
  "Process sentinel for the Hugo program.

See `set-process-sentinel' for PROCESS and EVENT details."
  (let ((program (process-name process))
        (event (replace-regexp-in-string "\n$" "" event))
        (buffer (get-buffer (hugo--buffer-name-for-type "process"))))
    (cond ((string-prefix-p "finished" event)
           (progn (hugo--handle-hugo-output buffer)
                  (with-current-buffer buffer
                    (let ((inhibit-read-only t))
                      (insert (concat (propertize (make-string 80 ?-) 'face 'font-lock-comment-face) "\n\n"))
                      (set-marker (process-mark process) (point))))
                  (message "Hugo has completed.")
                  (hugo--maybe-redraw-status)))
          ((string-prefix-p "exited" event)
           (message "Hugo exited abnormally; check the process output for information.")
           (hugo--handle-hugo-output buffer)))))

(defun hugo--generic-process-filter (proc string)
  "Filter PROC output of STRING and manipulate the buffer."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc)))
            (window (get-buffer-window))
            (inhibit-read-only t))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert (replace-regexp-in-string "" "" string))
          (set-marker (process-mark proc) (point))
          (hugo--find-server-address))
        (when moving
          (goto-char (process-mark proc))
          (if window
              (with-selected-window window
                (goto-char (process-mark proc)))))))))

(defun hugo--find-server-address ()
  "Try to find a Jekyll server address in the current buffer."
  (save-excursion
    (if (re-search-backward "\\(http://.*\\)/" nil t)
        (setq hugo-server-address (match-string 1)))))

(defun hugo--handle-hugo-output (buffer)
  "Attempt to do something reasonable based on output in BUFFER.

This is 'cheater mode' for not having callbacks in elisp and to avoid creating
different output buffers for different operations to figure out what to do with
each kind of output."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (re-search-backward "^[A-Z]" (point-min) t)
      (let ((output (buffer-substring (line-beginning-position) (line-end-position)))
            (default-directory (hugo--get-root)))
        (cond ((or (string-suffix-p "created" output))
               (let* ((filename (hugo--find-filename-in-output output)))
                 (if (file-exists-p filename)
                     (find-file filename)))))))))

(defun hugo--find-filename-in-output (output)
  "Find the filename in an Hugo OUTPUT line.

This helper function will extract a filename with preceding path
components, if present, from a single line of Hugo output.  Used
by `hugo--handle-hugo-output'.

If the string PREFIX is given, the filename is assumed to begin with
it.  For example, call with '_posts' or '_drafts' to find the
corresponding paths in the output line."
  (string-match (rx bol (group (*? (not (syntax whitespace)))) " created") output)
  (match-string 1 output))

(defun hugo--prepare-server-buffer ()
  "Return the Hugo Server buffer.

If the buffer doesn't exist yet, it will be created and prepared."
  (hugo--prepare-buffer-for-type "server" 'hugo-server-mode))

(defun hugo--prepare-process-buffer ()
  "Return the Hugo Process buffer.

If the buffer doesn't exist yet, it will be created and prepared."
  (hugo--prepare-buffer-for-type "process" 'hugo-process-mode))

(defun hugo--maybe-redraw-status ()
  "If the status buffer exists, redraw it with current information."
  (let ((status-buffer (get-buffer (hugo--buffer-name-for-type "status"))))
    (if (bufferp status-buffer)
        (hugo--draw-status status-buffer))))

(defun hugo--move-to-next-visible-thing (&optional reverse)
  "Move point to the next item with property 'thing that is visible.

If REVERSE is not nil, move to the previous visible 'thing."
  (goto-char (or (let ((start (point)))
                   (if reverse
                       (beginning-of-line)
                     (end-of-line))
                   (let* (destination)
                     (while (not destination)
                       (let ((next-candidate (if reverse
                                                 (previous-single-property-change (point) 'thing)
                                               (next-single-property-change (point) 'thing))))
                         (if next-candidate
                             (if (memq (get-text-property next-candidate 'invisible)
                                       buffer-invisibility-spec)
                                 (goto-char next-candidate)
                               (setq destination next-candidate))
                           (setq destination start))))
                     destination))
                 (point)))
  (beginning-of-line))

(defun hugo--thing-on-this-line-p ()
  "Determine whether there is a thing on this line."
  (get-text-property (line-beginning-position) 'thing))

(defun hugo--highlight-current-line ()
  "Create a highlight effect on the current line using overlays."
  (if (hugo--thing-on-this-line-p)
      (let ((end (save-excursion
                   (forward-line 1)
                   (point))))
        (move-overlay hugo-highlight-current-line-overlay (line-beginning-position) end))
    (delete-overlay hugo-highlight-current-line-overlay)))

(defun hugo--move-to-next-prop (prop-name)
  "Move to the next item with property PROP-NAME."
  (goto-char
   (or (save-excursion
         (goto-char (line-end-position))
         (let ((thing (next-single-property-change (point) prop-name)))
           (if thing
               (let ((type (get-text-property thing 'invisible)))
                 (if (and type (memq type buffer-invisibility-spec))
                     (remove-from-invisibility-spec type))
                 thing))))
       (point))))

(defun hugo--move-to-previous-prop (prop-name)
  "Move to the previous item with property PROP-NAME."
  (goto-char
   (or (save-excursion
         (goto-char (line-beginning-position))
         (let ((thing (previous-single-property-change (point) prop-name)))
           (if thing
               (let ((type (get-text-property thing 'invisible)))
                 (if (or (not type)
                         (not (memq type buffer-invisibility-spec)))
                     thing
                   nil)))))
       (point)))
  (goto-char (line-beginning-position)))

(defun hugo--get-line-type ()
  "Get the 'line type' property of the current line.

This function makes the assumption that the line's type is saved in a
text property at position zero."
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) 'invisible)))

(defun hugo--get-line-filename ()
  "Get the filename at point."
  (save-excursion
    (back-to-indentation)
    (thing-at-point 'filename)))

(defun hugo--expand-path-for-type (filename type)
  "Given a FILENAME and line TYPE, expand the file's path.

This function assumes that the base paths for every TYPE have been
defined in the configuration."
  (let ((type-dir (cdr (assoc type `((posts . ,hugo-posts-directory)
                                     (drafts . ,hugo-posts-directory))))))
    (and filename
         type-dir
         (expand-file-name
          filename (expand-file-name
                    type-dir (hugo--get-root))))))

(defun hugo--start-server-process (&optional with-drafts with-future with-expired)
  "Run the server start command.

Options WITH-DRAFTS, WITH-FUTURE, and WITH-EXPIRED correspond to
the Hugo flags `buildDrafts', `buildFuture', and `buildExpired'."
  (hugo--setup)
  (let* ((default-directory (hugo--get-root))
         (buffer (hugo--prepare-server-buffer))
         (drafts-opt (if with-drafts " --buildDrafts" nil))
         (future-opt (if with-future " --buildFuture" nil))
         (expired-opt (if with-expired " --buildExpired" nil))
         (command (concat "hugo server" drafts-opt future-opt expired-opt)))
    (if (processp (get-buffer-process (hugo--buffer-name-for-type "server")))
        (message "Server already running!")
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize (format "Running `%s'...\n\n" command) 'face 'font-lock-variable-name-face))))
      (let ((process
            (start-process-shell-command
             "hugo-server"
             buffer
             command)))
      (message "Server started!")
      (set-process-sentinel process 'hugo--server-sentinel)
      (set-process-filter process 'hugo--generic-process-filter))
      (hugo--maybe-redraw-status))))

(defun hugo--start-build-process (&optional with-drafts with-future with-expired)
  "Run the build command.

Options WITH-DRAFTS, WITH-FUTURE, and WITH-EXPIRED correspond to
the Hugo flags `buildDrafts', `buildFuture', `buildExpired'."
  (hugo--setup)
  (let* ((process-buffer (hugo--prepare-process-buffer))
         (drafts-opt (if with-drafts " --buildDrafts" nil))
         (future-opt (if with-future " --buildFuture" nil))
         (expired-opt (if with-expired " --buildExpired" nil))
         (root (hugo--get-root))
         (command (string-trim-left (concat drafts-opt future-opt expired-opt))))
    (hugo--run-hugo-command command)))

(defun hugo--stop-server-process ()
  "Call the stop server command."
  (let ((server-process (get-buffer-process (hugo--buffer-name-for-type "server"))))
    (if (processp server-process)
        (delete-process server-process)
      (error "The Hugo server is not running"))))

(defun hugo--server-sentinel (process event)
  "Handle Hugo process output.

Standard arguments PROCESS and EVENT correspond to those documented in
`set-process-sentinel'."
  (hugo--maybe-redraw-status)
  (let ((program (process-name process))
        (event (replace-regexp-in-string "\n$" "" event)))
    (cond ((or (string-prefix-p "finished" event)
               (string-prefix-p "killed" event))
           (progn (message "Hugo server has finished.")
                  (with-current-buffer (hugo--prepare-server-buffer)
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert (propertize "Hugo server has finished.\n\n" 'face 'font-lock-warning-face))
                      (goto-char (point-max)))))))))

(provide 'hugo)
;;; hugo.el ends here
