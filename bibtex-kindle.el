;;; bibtex-kinde.el --- Kindle utilities for BibTeX

(require 'sha1)

;; prefix of files to push onto kindle
(defvar bibtex-kindle-prefix "~/kindle")

;; path to pdftk binary
(defvar bibtex-kindle-pdftk "/opt/local/bin/pdftk")


(defun bibtex-kindle-export-collections ()
  "Exports collections.json file from a bib file"
  (interactive "P")
  ; strategy: move at start of buffer
  ; select all entries that have a kindle-file
  ; use booktitle + year of InProceedings
  ; use journal + issue of article
  ; plus keywords
  ; thus collections is a alist: collection -> [item]
  (let (collections)
    for x in bib*
      if entry contains kindle-file key
        if entry is inproceedings
          collections[booktitle + year] ::= file
        if entry is article
          collections[journal] ::= file
        if entry contains keywords
          collections[keywords] ::= file
    output {
    for x in collections
      output { + \" + car + @en-US\": { items:[
      for y in cdr : output ,\"* sha1(/mnt/us/documents/bib/y) \"
      output ],\"lastAccess\": now? + }}
    output }
))

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

(defun bibtex-make-field-file (&optional arg)
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
              (author (cdr (assoc "author" entry))))
          (setq title (replace-regexp-in-string "\n" " " title))
          (setq title (replace-regexp-in-string "\[ \]+" " " title))
          (let ((target-dir (format "%s/%s" bibtex-kindle-prefix year)))
            (make-directory target-dir t)
            (let ((target (format "%s/%s.pdf" target-dir (sha1 title))))
              ; in a more perfect world, this would be firstauthor-title or similar
              (if (file-exists-p target)
                  (message (format "File with name %s already exists" target))
                (progn
                  (let* ((pdftk (start-process "pdftk" "*pdftk*" bibtex-kindle-pdftk (expand-file-name filename) "update_info" "-" "output" (expand-file-name target)))
                         (pdftkn (process-name pdftk)))
                    (process-send-string pdftkn "InfoKey: Author\n")
                    (process-send-string pdftkn (format "InfoValue: %s\n" author))
                    (process-send-string pdftkn "InfoKey: Title\n")
                    (process-send-string pdftkn (format "InfoValue: %s\n" title))
                    (process-send-eof pdftkn))))
                (skip-chars-backward "}")
                (insert (format "file:{%s/%s.pdf}" year (sha1 title))))))))))

(global-set-key (kbd "C-c f") 'bibtex-make-field-file)

(provide 'bibtex-kindle)
