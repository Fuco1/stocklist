;;; stocklist.el --- Grab and display stocks information -*- lexical-binding: t -*-

;; Copyright (C) 2016 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 16th February 2016
;; Package-requires: ((dash "2.10.0"))
;; Keywords: news

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; http://www.jarloo.com/yahoo_finance/

;;; Code:

(require 'dash)
(require 's)
(require 'parse-csv)

(require 'url)
(require 'org-table)
(require 'org-element)

(defgroup stocklist ()
  "Stocklist."
  :group 'comm)

(defcustom stocklist-instruments nil
  "List of tracked instruments.

Each instrument has some optional properties:

- :face is a face used to highlight this instrument's row.  Stocklist
  package comes with some predefined faces for common \"states\", but
  you can use any face you wish.
- :tags is a list of tags.  You can filter/group instruments by these."
  :type '(alist :key-type (string :tag "Stock symbol")
                :value-type (plist :key-type symbol
                                   :value-type sexp
                                   :options ((:face face)
                                             (:tags (repeat string)))))
  :group 'stocklist)

(defun stocklist-instruments ()
  "Return the tracked instruments.

This extracts the symbols from `stocklist-instruments' variable."
  (-map 'car stocklist-instruments))

(defcustom stocklist-quandl-api-key "74j1sBFqMF1_hsKgSC8x"
  "API Key for Quandl database."
  :type 'string
  :group 'stocklist)

;; TODO: generate the defstruct based on the code somehow?
;; p - last close
;; g - day low
;; h - day high
(defcustom stocklist-query-code-yahoo "nsbaryde"
  "Query code for Yahoo API."
  :type 'string
  :group 'stocklist)

(defcustom stocklist-quandl-cache-directory "~/.cache/emacs/stocklist"
  "Cache directory for quandl data."
  :type 'string
  :group 'stocklist)

(defun stocklist-url-retrieve-body (url)
  "Retrieve content from URL and return the body of the http response."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (s-trim (buffer-substring (point) (point-max))))))

(defun stocklist-get-data-yahoo (stocks)
  "Retrieve the raw data for the STOCKS.

STOCKS is a list of strings where each string is a ticker
symbol."
  (stocklist-url-retrieve-body
   (format "http://finance.yahoo.com/d/quotes.csv?s=%s&f=%s"
           (s-join "+" stocks) stocklist-query-code-yahoo)))

(defun stocklist-get-data-quandl (ticker)
  "Retrieve stock data for TICKER from quandl.

Historical data is cached."
  (unless (file-directory-p stocklist-quandl-cache-directory)
    (make-directory stocklist-quandl-cache-directory t))
  (let ((cache-file (concat stocklist-quandl-cache-directory "/" ticker ".el")))
    ;; TODO: check the date of cache creation and re-download newer
    ;; data if needed
    (if (file-exists-p cache-file)
        (with-temp-buffer
          (insert-file-contents cache-file)
          (goto-char (point-min))
          (read (current-buffer)))
      (let* ((raw-data (stocklist-url-retrieve-body
                        (format "https://www.quandl.com/api/v3/datasets/WIKI/%s/data.csv?api_key=74j1sBFqMF1_hsKgSC8x" ticker)))
             (raw-rows (cdr (parse-csv-string-rows raw-data ?\, ?\" "\n")))
             (rows (mapcar
                    (lambda (row)
                      (cons
                       (car row)
                       (mapcar 'string-to-number (cdr row))))
                    raw-rows)))
        (with-temp-file cache-file
          (insert (format "%S" rows))
          rows)))))

;; TODO: add alerts on the values
(cl-defstruct stocklist-instrument name symbol bid ask pe yield dps eps payout)

(defun stocklist-parse-data-yahoo (data)
  "Parse the raw DATA into elisp datastuctures."
  (let ((raw-rows (parse-csv-string-rows data ?\, ?\" "\n"))
        (rows nil))
    (-map
     (lambda (items)
       (make-stocklist-instrument
        :name (nth 0 items)
        :symbol (nth 1 items)
        :bid (nth 2 items)
        :ask (nth 3 items)
        :pe (nth 4 items)
        :yield (nth 5 items)
        :dps (nth 6 items)
        :eps (nth 7 items)
        :payout (format "%.2f"
                        (/ (string-to-number (nth 6 items))
                           (string-to-number (nth 7 items))))))
     raw-rows)))

(defun stocklist-export-to-org-table (data)
  "Export stocklist DATA as `org-mode' table."
  (let* ((fields (-flatten
                  (cdr (plist-get
                        (symbol-plist 'stocklist-instrument)
                        'cl-struct-slots))))
         (field-names (--map (s-upper-camel-case (symbol-name it)) fields)))
    (with-current-buffer (get-buffer-create "*stocklist*")
      (erase-buffer)
      (insert "| " (s-join " | " field-names) " |\n")
      (insert "|-")
      (-each data
        (lambda (row)
          (insert "\n| ")
          (-each fields
            (lambda (field)
              (insert (funcall (intern (concat "stocklist-instrument-" (symbol-name field))) row) " | ")))))
      (stocklist-mode)
      (org-table-align)
      (current-buffer))))

(defun stocklist-get-buffer ()
  "Get buffer with updated data."
  (let* ((raw-data (stocklist-get-data-yahoo (stocklist-instruments)))
         (processed-data (stocklist-parse-data-yahoo raw-data))
         (export-buffer (stocklist-export-to-org-table processed-data)))
    export-buffer))

;; TODO: extract to a general "org" helper package
(defun stocklist-goto-cell-beginning ()
  "Move point to the first non-whitespace char in current cell."
  (re-search-backward "|")
  (forward-char 1)
  (skip-syntax-forward " "))

;; TODO: extract to a general "org" helper package
(defun stocklist-goto-column (column-name)
  "Go to column COLUMN-NAME."
  (goto-char (point-min))
  (unless (re-search-forward column-name nil t)
    (error "Column %s not found" column-name))
  (stocklist-goto-cell-beginning))

;; TODO: extract to a general "org" helper package
(defun stocklist-with-column (column-name fn)
  "Run FN with point in each cell of column COLUMN-NAME."
  (declare (indent 1))
  (save-excursion
    (stocklist-goto-column column-name)
    (let ((cc (org-table-current-column)))
      (forward-line 1)
      (while (and (= 0 (forward-line 1))
                  (not (eobp)))
        (org-table-goto-column cc)
        (-let* (((_ (&plist :contents-begin cb :contents-end ce)) (org-element-table-cell-parser))
                ;; TODO: don't pass number right away?
                (content (buffer-substring-no-properties cb ce)))
          (funcall fn cb ce content))))))

(defun stocklist-fontify ()
  "Fontify the buffer using stocklist rules."
  (setq-local font-lock-keywords nil)
  (font-lock-mode -1)
  (font-lock-mode 1)
  (variable-pitch-mode -1)
  (put-text-property (point-min) (point-max) 'font-lock-face 'org-table)
  (stocklist-with-column "payout"
    (lambda (cb ce content)
      (let* ((content (string-to-number content)))
        (when (> content 0.6)
          (put-text-property cb ce 'font-lock-face 'font-lock-warning-face))
        (when (< content 0.4)
          (put-text-property cb ce 'font-lock-face 'font-lock-keyword-face)))))
  (stocklist-with-column "yield"
    (lambda (cb ce content)
      (let ((content (string-to-number content)))
        (when (> content 3)
          (put-text-property cb ce 'font-lock-face 'font-lock-keyword-face))
        (when (< content 1.5)
          (put-text-property cb ce 'font-lock-face 'font-lock-warning-face)))))
  (stocklist-with-column "eps"
    (lambda (cb ce content)
      (let ((content (string-to-number content)))
        (when (<= content 0)
          (put-text-property cb ce 'font-lock-face 'font-lock-warning-face)))))
  (stocklist-with-column "Symbol"
    (lambda (_ _ symbol)
      (-when-let ((&alist symbol (&plist :face face)) stocklist-instruments)
        (font-lock-prepend-text-property
         (line-beginning-position)
         (line-end-position)
         'font-lock-face face)))))

;; TODO: pass the environment automagically
;; TODO: pass the current stocklist state and restore if we are reverting
(defun stocklist-show ()
  "Show formatted data for tracked stocks."
  (interactive)
  (async-start
   `(lambda ()
      ,(async-inject-variables (concat "\\`load-path\\'"))
      (require 'stocklist)
      ,(async-inject-variables (concat "\\`" (regexp-opt '("stocklist-instruments" "stocklist-query-code-yahoo")) "\\'"))
      (with-current-buffer (stocklist-get-buffer)
        (buffer-substring-no-properties (point-min) (point-max))))
   (lambda (result)
     (with-current-buffer (get-buffer-create "*stocklist*")
       (erase-buffer)
       (insert result)
       (stocklist-mode)
       (stocklist-fontify)
       (goto-char (point-min))
       (pop-to-buffer (current-buffer))))))

;; TODO: add automatic reverting
(defun stocklist-revert ()
  "Revert stocklist."
  (interactive)
  (let ((row (line-number-at-pos))
        (col (org-table-current-column)))
    (stocklist-show)
    (goto-char (point-min))
    (forward-line (1- row))
    (org-table-goto-column col)))

;; TODO: add persistent ordering between reloads
(defvar stocklist-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map "g" 'stocklist-revert)
    (define-key map "s" 'org-table-sort-lines)
    map))

(define-derived-mode stocklist-mode org-mode "Stocklist"
  "Stocklist mode."
  (use-local-map stocklist-mode-map))

(provide 'stocklist)
;;; stocklist.el ends here
