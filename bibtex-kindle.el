;;; bibtex-kinde.el --- Kindle utilities for BibTeX

;; Copyleft 2011 Hannes Mehnert <hannes@mehnert.org>
;; Available via https://github.com/hannesm/bibtex-kindle
;; This code is public domain; all rights reversed.

(require 'sha1)

;; TODO
;; annotate authors as last1, first1 & last2, first2,... for better sorting!
;; handle special characters correctly (how is it on kindle? how in pdf metadata?)
;; maybe collections for authors?
;; multiple bib files
;; proper merge of collections?

;; prefix of files to push onto kindle
(defvar bibtex-kindle-prefix "~/kindle")

;; path to pdftk binary
(defvar bibtex-kindle-pdftk "/opt/local/bin/pdftk")

(defun mtime-for-kindle (filename)
  "returns modification time of file (as seconds from epoch with two additional digits)"
  (let ((fn (concat bibtex-kindle-prefix "/" filename)))
    (if (file-exists-p fn)
        (let ((ttt (nth 5 (file-attributes fn 'string))))
          ; hate 30 bits integers
          (let ((pr (number-to-string (+ (* (float (lsh 1 16)) (car ttt)) (cadr ttt)))))
            (store-substring pr (- (length pr) 2) "1")
            pr))
      "0")))

(defun bibtex-kindle-export-collections (&optional arg)
  "Exports collections.json file from a bib file"
  (interactive "P")
  (let (collections)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^@[a-zA-Z0-9]+{" nil t)
	(goto-char (match-beginning 0))
	(let ((entry (bibtex-parse-entry t)))
          (if (assoc "kindle-file" entry)
              (let* ((filename (cdr (assoc "kindle-file" entry)))
                     (mtime (mtime-for-kindle filename))
                     (reftype (cdr (assoc "=type=" entry))))
                (flet ((add-to-collection
                        (key)
                        (if (assoc key collections)
                            (let* ((lastmtime (pop (cdr (assoc key collections))))
                                   (nmtime (if (< (length lastmtime) (length mtime))
                                               mtime
                                             (if (< (length mtime) (length lastmtime))
                                                 lastmtime
                                               (if (string< lastmtime mtime)
                                                 mtime
                                                 lastmtime)))))
                              (let ((col (cdr (assoc key collections))))
                              (setcdr (assoc key collections) (cons nmtime (cons filename col)))))
                          (setq collections (cons (cons key (cons mtime (cons filename ()))) collections)))))
                  (if (string-equal reftype "inproceedings")
                      (let* ((book (cdr (assoc "booktitle" entry)))
                             (year (cdr (assoc "year" entry)))
                             (key (concat book "-" year)))
                        (add-to-collection key)))
                  (if (string-equal reftype "article")
                      (let* ((journal (cdr (assoc "journal" entry)))
                             (volume (cdr (assoc "volume" entry)))
                             (key (concat journal volume)))
                        (add-to-collection key)))
                  (let ((keywords (cdr (assoc "keywords" entry))))
                    (if (not (or (null keywords) (string-equal keywords "")))
                        (mapc
                         'add-to-collection
                         (split-string keywords ",[ \n]*"))))))))))

    (with-temp-file (concat bibtex-kindle-prefix "/collections.json")
      (insert "{")
      (flet ((output-l
              (x)
              (insert
               (mapconcat (lambda(f)
                            (concat "\"*" (sha1 (concat "/mnt/us/documents/bib/" f)) "\""))
                          x ","))))
        (let ((first 1))
          (flet ((output-c
                  (c)
                  (if (= first 1) (setq first 2) (insert ","))
                  (insert (concat "\"" (car c) "@en-US\":{\"items\":["))
                  (output-l (cddr c))
                  ;; replace with last atime/mtime!
                  (insert (concat "], \"lastAccess\":" (cadr c) "}"))))
            (mapc 'output-c collections))))
      (insert "}")))
  (message (concat "Wrote output to " bibtex-kindle-prefix "/collections.json")))

(defun get-pdf (ask-message default-location)
  ""
  (let (filename)
    (while (progn
             (setq filename (read-file-name ask-message default-location))
             (setq ask-message "File not readable or not a .pdf: ")
             (setq default-location (file-name-directory filename))
             (not (and (file-readable-p filename)
                       (string-equal (file-name-extension filename) "pdf")))))
    (list filename)))

(defun bibtex-make-field-kindle-file (&optional arg)
  "Make a file field. That's enriched with metadata to put on the kindle."
  (interactive "P")
  (let ((entry (save-excursion (bibtex-beginning-of-entry) (bibtex-parse-entry t))))
    (if (assoc "kindle-file" entry)
        (message "kindle-file field already present in this entry")
      (progn
        (bibtex-make-field "kindle-file" t nil)
        (let ((filename (car (get-pdf "Path to pdf: " nil)))
              (year (cdr (assoc "year" entry)))
              (title (cdr (assoc "title" entry)))
              (author (cdr (assoc "author" entry)))
              (citekey (cdr (assoc "=key=" entry))))
          (setq title (replace-regexp-in-string "\n" " " title))
          (setq title (replace-regexp-in-string "\[ \]+" " " title))
          (make-directory (concat bibtex-kindle-prefix "/" year) t)
          (let* ((target (format "%s/%s.pdf" year title))
                 (ftarget (concat bibtex-kindle-prefix "/" target)))
            (if (file-exists-p ftarget)
                (message (format "File with name %s already exists" ftarget))
              (progn
                (let* ((pdftk (start-process "pdftk" "*pdftk*" bibtex-kindle-pdftk (expand-file-name filename) "update_info" "-" "output" (expand-file-name ftarget)))
                       (pdftkn (process-name pdftk)))
                  (process-send-string pdftkn "InfoKey: Author\n")
                  (process-send-string pdftkn (format "InfoValue: %s\n" author))
                  (process-send-string pdftkn "InfoKey: Title\n")
                  (process-send-string pdftkn (format "InfoValue: %s\n" title))
                  (process-send-eof pdftkn))))
            (skip-chars-backward "}")
            (insert target)))))))

(global-set-key (kbd "C-c f") 'bibtex-make-field-kindle-file)
(global-set-key (kbd "C-c e") 'bibtex-kindle-export-collections)

(provide 'bibtex-kindle)


