;;; bibtex-kinde.el --- Kindle utilities for BibTeX

(require 'sha1)

;; prefix of files to push onto kindle
(defvar bibtex-kindle-prefix "~/kindle")

;; path to pdftk binary
(defvar bibtex-kindle-pdftk "/opt/local/bin/pdftk")


(defun bibtex-kindle-export-collections (&optional arg)
  "Exports collections.json file from a bib file"
  (interactive "P")
  (let ((collections (cons () ())))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^@[a-zA-Z0-9]+{" nil t)
	(goto-char (match-beginning 0))
	(let ((entry (bibtex-parse-entry t)))
          (if (assoc "kindle-file" entry)
              (let* ((filename (cdr (assoc "kindle-file" entry)))
                     (reftype (cdr (assoc "=type=" entry)))
                     (add-to-collection
                      '(lambda (key)
                        (if (assoc key collections)
                            (add-to-list (cdr (assoc key collections)) filename)
                          (setq collections cons((key . (list filename)) collections))))))
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
              (if (not (string-equal keywords ""))
                  (mapc
                   'add-to-collection
                   (split-string keywords ",[ \n]*")))))))))
    (with-temp-file (concat bibtex-kindle-prefix "/collections.json")
      (insert "{")
      (let ((output-l
             (lambda (x)
               (insert
                (mapconcat (lambda(f)
                             (concat "\"*" (sha1 (concat "/mnt/us/documents/bib/" f)) "\""))
                           x ",")))))
        (let* ((output-c
                (lambda (x)
                  (insert (concat "\"" (car x) "@en-US\":{\"items\":["))
                  (output-l (cdr x))
                  ;; replace with last atime/mtime!
                  (insert (concat "], \"lastAccess\":131863907336}")))))
          (mapc output-c collections)))
      (insert "}"))))

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
          (make-directory (concat bibtex-kindle-prefix year) t)
          (let* ((target (format "%s/%s.pdf" year citekey))
                 (ftarget (concat bibtex-kindle-prefix target)))
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

(flet ((f (x) (* x x)))
  (f 2))
