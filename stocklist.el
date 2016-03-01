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
  "List of tracked instruments."
  :type '(repeat string)
  :group 'stocklist)

;; TODO: generate the defstruct based on the code somehow?
;; p - last close
;; g - day low
;; h - day high
(defcustom stocklist-query-code-yahoo "nsbaryde"
  "Query code for Yahoo API."
  :type 'string
  :group 'stocklist)

(defun stocklist-get-data-yahoo (stocks)
  "Retrieve the raw data for the STOCKS.

STOCKS is a list of strings where each string is a ticker
symbol."
  (with-current-buffer (url-retrieve-synchronously
                        (format "http://finance.yahoo.com/d/quotes.csv?s=%s&f=%s"
                                (s-join "+" stocks) stocklist-query-code-yahoo))
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (s-trim (buffer-substring (point) (point-max))))))

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
  (let* ((raw-data (stocklist-get-data-yahoo stocklist-instruments))
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
                (content (string-to-number (buffer-substring-no-properties cb ce))))
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
      (when (> content 0.6)
        (put-text-property cb ce 'font-lock-face 'font-lock-warning-face))
      (when (< content 0.4)
        (put-text-property cb ce 'font-lock-face 'font-lock-keyword-face))))
  (stocklist-with-column "yield"
    (lambda (cb ce content)
      (when (> content 3)
        (put-text-property cb ce 'font-lock-face 'font-lock-keyword-face))
      (when (< content 1.5)
        (put-text-property cb ce 'font-lock-face 'font-lock-warning-face))))
  (stocklist-with-column "eps"
    (lambda (cb ce content)
      (when (<= content 0)
        (put-text-property cb ce 'font-lock-face 'font-lock-warning-face)))))

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
