;;; stocklist.el --- Grab and display stocks information -*- lexical-binding: t -*-

;; Copyright (C) 2016 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 16th February 2016
;; Package-requires: ((dash "2.12.1") (s "1.9") (parse-csv "0.3") (async "1.6"))
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

(require 'async)
(require 'dash)
(require 's)
(require 'parse-csv)

(require 'url)
(require 'org-table)
(require 'org-element)

(defgroup stocklist ()
  "Stocklist."
  :group 'comm)

(define-widget 'stocklist-columns 'lazy
  "Stocklist columns"
  :tag "Column"
  :type '(choice
          (const :tag "Close" close)
          (const :tag "Ask" ask)
          (const :tag "PE" pe)
          (const :tag "Yield" yield)
          (const :tag "DPS" dps)
          (const :tag "EPS" eps)
          (const :tag "Payout" payout)))

(define-widget 'stocklist-signal 'lazy
  "Stocklist signal"
  :tag "Signal"
  :type '(choice (list :tag "Value less than"
                   stocklist-columns
                   (const <)
                   (number :tag "Value"))
                 (list :tag "Value greater than"
                   stocklist-columns
                   (const >)
                   (number :tag "Value"))
                 (function :tag "Function")))

(defcustom stocklist-instruments nil
  "List of tracked instruments.

Each instrument has some optional properties:

- :face is a face used to highlight this instrument's row.  Stocklist
  package comes with some predefined faces for common \"states\", but
  you can use any face you wish.
- :tags is a list of tags.  You can filter/group instruments by these.
  Tags are strings and can not contain the - character."
  :type '(alist :key-type (string :tag "Stock symbol")
                :value-type (plist :key-type symbol
                                   :value-type sexp
                                   :options ((:face face)
                                             (:tags (repeat string))
                                             (:signals (repeat stocklist-signal)))))
  :group 'stocklist)

(defcustom stocklist-tag-to-face nil
  "Map tags to faces."
  :type '(alist :key-type (string :tag "Tag")
                :value-type face)
  :group 'stocklist)

(defcustom stocklist-default-sort nil
  "Default sort."
  :type '(radio
          (const nil)
          (cons stocklist-columns
                (radio
                 (const asc)
                 (const desc))))
  :group 'stocklist)

(defun stocklist-instruments (&optional query)
  "Return the tracked instruments.

QUERY is a query to select instruments by tags.  You can join
multiple tags with + to select instruments with all the tags.
You can join multiple tags with | to select tags with at least
one of the tags in the group.  You can prefix a tag with ! to
select instruments without this tag.  Logical and (+) binds
stronger than OR (|).

Examples:
  a+b   : select all instruments with both tags \"a\" and \"b\"
  a|b   : select all instruments with either tags \"a\" or \"b\"
  a+b|c : select all instruments with tags (\"a\" and \"b\") or \"c\"
  !a    : select all instruments without the tag \"a\""
  (setq query (or query ""))
  (setq query (replace-regexp-in-string "-" "+!" query))
  (let* ((or-groups (split-string query "|" t))
         (and-groups (--map (split-string it "+") or-groups))
         (form (if (equal query "") t
                 `(or ,@(-map
                         (lambda (g)
                           `(and ,@(--map (if (= (aref it 0) ?!)
                                              `(not (member ,(substring it 1) tags))
                                            `(member ,it tags)) g)))
                         and-groups)))))
    (-map 'car (--filter (eval `(let ((tags ',(plist-get (cdr it) :tags))) ,form)) stocklist-instruments))))

(defcustom stocklist-quandl-api-key "74j1sBFqMF1_hsKgSC8x"
  "API Key for Quandl database."
  :type 'string
  :group 'stocklist)

;; TODO: generate the defstruct based on the code somehow?
;; p - last close
;; g - day low
;; h - day high
(defcustom stocklist-query-code-yahoo "nsparde"
  "Query code for Yahoo API."
  :type 'string
  :group 'stocklist)

(defcustom stocklist-quandl-cache-directory "~/.cache/emacs/stocklist"
  "Cache directory for quandl data."
  :type 'string
  :group 'stocklist)

(defcustom stocklist-column-fontifiers nil
  "Column fontifiers."
  :type '(alist
          :key-type string
          :value-type function)
  :group 'stocklist)

(defface stocklist-owned
  '((t (:background "black")))
  "Face used to highlight owned stocks."
  :group 'stocklist)

(defface stocklist-alert
  '((t (:background "grey")))
  "Face used to highlight stocks on alert.

What alert means is up to the user."
  :group 'stocklist)

(defface stocklist-signal-cell
  '((t (:background "blue")))
  "Face used to highlight cell on signal alert."
  :group 'stocklist)

(defface stocklist-watched-cell
  '((t (:background "indian red")))
  "Face used to highlight cell with a defined signal.

If the signal triggered, `stocklist-signal-cell' face is used
instead."
  :group 'stocklist)

(cl-defstruct stocklist-buffer-state query ordering)

(defvar stocklist-state nil
  "Stocklist state.")

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
  (let ((parts (-partition-all 50 stocks)))
    (mapconcat
     (lambda (p)
       (stocklist-url-retrieve-body
        (format "http://finance.yahoo.com/d/quotes.csv?s=%s&f=%s"
                (s-join "+" p) stocklist-query-code-yahoo)))
     parts
     "\n")))

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
(cl-defstruct stocklist-instrument name symbol close ask pe yield dps eps payout)

(defun stocklist-parse-data-yahoo (data)
  "Parse the raw DATA into elisp datastuctures."
  (let ((raw-rows (parse-csv-string-rows data ?\, ?\" "\n"))
        (rows nil))
    (-map
     (lambda (items)
       (make-stocklist-instrument
        :name (nth 0 items)
        :symbol (nth 1 items)
        :close (nth 2 items)
        :ask (nth 3 items)
        :pe (nth 4 items)
        :yield (let ((div (string-to-number (nth 5 items))))
                 (if (< div 0.001) "0.00"
                   (format "%.2f" (/ div (string-to-number (nth 3 items)) 0.01))))
        :dps (nth 5 items)
        :eps (nth 6 items)
        :payout (format "%.2f"
                        (/ (string-to-number (nth 5 items))
                           (string-to-number (nth 6 items))))))
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

(defun stocklist-get-buffer (&optional query)
  "Get buffer with updated data.

QUERY is a query string to select a subset of instruments.  See
function `stocklist-instruments'."
  (let* ((raw-data (stocklist-get-data-yahoo (stocklist-instruments query)))
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
(defun stocklist-goto-column (column-name &optional goto-data)
  "Go to column COLUMN-NAME.

If GOTO-DATA is non-nil, go to first row with data."
  (goto-char (point-min))
  (unless (re-search-forward column-name nil t)
    (error "Column %s not found" column-name))
  (when goto-data
    (let ((cc (org-table-current-column)))
      (forward-line 2)
      (org-table-goto-column cc)))
  (stocklist-goto-cell-beginning))

(defun stocklist-get-cell (&optional column)
  "Get cell data of cell under point.

If optional argument COLUMN is set, get data of that column at
current row."
  (save-excursion
    (when column (stocklist-goto-current-column column))
    (-let* (((_ (&plist :contents-begin cb :contents-end ce)) (org-element-table-cell-parser)))
      (list :beg cb :end ce :content (buffer-substring-no-properties cb ce)))))

(defun stocklist-get (property)
  "Get value of PROPERTY at current row."
  (let* ((name (substring (symbol-name property) 1))
         (accessor (intern (concat "stocklist-instrument-" name))))
    (funcall
     accessor
     (get-text-property (line-beginning-position) 'stocklist-row))))

(defun stocklist-goto-current-column (column)
  "Go to column COLUMN at current row."
  (let ((cn (save-excursion
              (stocklist-goto-column column)
              (org-table-current-column))))
    (org-table-goto-column cn)))

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

(defun stocklist-fontify-numeric-cell (high low &optional reverse)
  "Fontify numeric cell"
  (lambda (cb ce content)
    (let* ((content (string-to-number content)))
      ;; TODO: extract the ratios
      (when (if reverse (< content low) (> content high))
        (put-text-property cb ce 'face 'font-lock-warning-face))
      (when (if reverse (> content high) (< content low))
        (put-text-property cb ce 'face 'font-lock-keyword-face)))))

(defun stocklist--fontify-payout (cb ce content)
  "Fontify the payout cell."
  (funcall (stocklist-fontify-numeric-cell 0.6 0.4) cb ce content))

(defun stocklist--fontify-yield (cb ce content)
  "Fontify the yield cell."
  (funcall (stocklist-fontify-numeric-cell 3 1.5 :reverse) cb ce content))

(defun stocklist--fontify-eps (cb ce content)
  "Fontify the eps cell."
  (funcall (stocklist-fontify-numeric-cell 1000 0.001 :reverse) cb ce content))

(defun stocklist--fontify-pe (cb ce content)
  "Fontify the pe cell."
  (funcall (stocklist-fontify-numeric-cell 25 0.001) cb ce content))

(defun stocklist-run-column-fontifiers (list)
  "Run all the fontifiers in LIST.

LIST is an alist mapping column name to a FONTIFIER.  FONTIFIER
is a function taking three values: CB and CE are beginning and
end points of the cell and CONTENT is the string value of the
cell.  Each fontifier function should operate on a single cell
only."
  (-each list
    (-lambda ((col . fontifier))
      (stocklist-with-column col fontifier))))

(defun stocklist--add-help-echo (beg end sig verb)
  "Add help echo on watched/signal fields.

BEG and END are buffer positions.

SIG is the signal.

VERB is either \"is\" or \"should be\""
  (let ((column (symbol-name (car sig))))
    (add-text-properties
     beg end
     `(help-echo
       ,(format
         "%s %s %s than %.2f.%s"
         column
         verb
         (if (eq '< (cadr sig)) "less" "more")
         (nth 2 sig)
         (cond
          ((equal column "yield")
           (format
            "  That gives price target of %.2f"
            (* (stocklist-get :ask)
               (/ (stocklist-get :yield)
                  (nth 2 sig)))))
          ((equal column "pe")
           (format
            "  That gives price target of %.2f"
            (* (stocklist-get :ask)
               (/ (nth 2 sig)
                  (stocklist-get :pe)))))
          (t "")))))))

(defun stocklist--fontify-more-or-less-signal (sig more-or-less)
  "Fontify cell where < or > signal triggered.

SIG is the signal data.

MORE-OR-LESS is one of '< or '>."
  (-let* ((column (symbol-name (car sig)))
          ((&plist :beg beg :end end :content content) (stocklist-get-cell column)))
    (if (funcall more-or-less (string-to-number content) (nth 2 sig))
        (progn
          (font-lock-prepend-text-property beg end 'face 'stocklist-signal-cell)
          (stocklist--add-help-echo beg end sig "is"))
      (font-lock-prepend-text-property beg end 'face 'stocklist-watched-cell)
      (stocklist--add-help-echo beg end sig "should be"))))

(defun stocklist--fontify-signals (signals)
  "Fontify current row's cells with triggered SIGNALS."
  (-each signals
    (lambda (sig)
      (cond
       ((and (listp sig)
             (eq (cadr sig) '<))
        (stocklist--fontify-more-or-less-signal sig '<))
       ((and (listp sig)
             (eq (cadr sig) '>))
        (stocklist--fontify-more-or-less-signal sig '>))))))

(defun stocklist--fontify ()
  "Fontify the buffer using stocklist rules."
  (setq-local font-lock-keywords nil)
  (variable-pitch-mode -1)
  (put-text-property (point-min) (point-max) 'face 'org-table)
  (stocklist-run-column-fontifiers stocklist-column-fontifiers)
  (stocklist-with-column "Symbol"
    (lambda (_ _ symbol)
      (-let* (((&alist symbol (&plist :face face :tags tags :signals signals)) stocklist-instruments)
              (face (or face (cdr (--some (assoc it stocklist-tag-to-face) tags)))))
        (when face
          (font-lock-prepend-text-property
           (line-beginning-position)
           (line-end-position)
           'face face))
        (when signals
          (stocklist--fontify-signals signals))))))

(defun stocklist--sort (how)
  "Sort lines in the stocklist listing."
  (save-excursion
    (when how
      (-let (((col . direction) how))
        (stocklist-goto-column (symbol-name col) t)
        (org-table-sort-lines nil (if (eq direction 'asc) ?n ?N))))))

(defun stocklist--parse-table ()
  "Parse resulting table rows back into stocklist structure.

Put the structure as text property to the beginning of each row.

Numbers are already parsed and saved as numbers."
  (save-excursion
    (goto-char (point-min))
    (forward-line)
    (while (= (forward-line) 0)
      (let ((items (-map
                    's-trim
                     (split-string
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))
                      "|" t))))
        (when items
          (put-text-property
           (point) (1+ (point))
           'stocklist-row
            (make-stocklist-instrument
             :name (nth 0 items)
             :symbol (nth 1 items)
             :close (let ((v (nth 2 items)))
                    (if (equal v "N/A") nil (string-to-number v)))
             :ask (let ((v (nth 3 items)))
                    (if (equal v "N/A") nil (string-to-number v)))
             :pe (let ((v (nth 4 items)))
                   (if (equal v "N/A") nil (string-to-number v)))
             :yield (let ((v (nth 5 items)))
                      (if (equal v "N/A") nil (string-to-number v)))
             :dps (let ((v (nth 6 items)))
                    (if (equal v "N/A") nil (string-to-number v)))
             :eps (let ((v (nth 7 items)))
                    (if (equal v "N/A") nil (string-to-number v)))
             :payout (let ((v (nth 8 items)))
                       (if (equal v "N/A") nil (string-to-number v))))))))))

(defun stocklist-read-query (&optional initial)
  "Read a stocklist query.

Optional argument INITIAL specifies initial content."
  (let* ((keymap (copy-keymap minibuffer-local-map))
         (tags (-uniq (--mapcat (plist-get (cdr it) :tags) stocklist-instruments)))
         (minibuffer-completion-table
          (completion-table-dynamic
           (lambda (string)
             (let* ((tags-in-query (split-string string "[+|]"))
                    (active-tag (-last-item tags-in-query)))
               (-map
                (lambda (s)
                  (let* ((slices (s-slice-at "[+|]" string))
                         (suffix (if (= 1 (length slices))
                                     (car slices)
                                   (substring (-last-item slices) 1)))
                         (prefix (s-chop-suffix suffix string)))
                    (concat prefix s)))
                (all-completions active-tag tags)))))))
    (define-key keymap (kbd "<tab>") 'minibuffer-complete)
    (read-from-minibuffer (format
                           "Query%s: "
                           (if (and initial
                                    (< 0 (length initial)))
                               (format " [default: %s]" initial) ""))
                          initial keymap nil nil initial)))

;; TODO: pass the environment automagically
;; TODO: pass the current stocklist state and restore if we are reverting
(defun stocklist-show (&optional query)
  "Show formatted data for tracked stocks."
  (interactive
   (list (stocklist-read-query)))
  (async-start
   `(lambda ()
      ,(async-inject-variables (concat "\\`load-path\\'"))
      (require 'stocklist)
      ,(async-inject-variables (concat "\\`" (regexp-opt '("stocklist-instruments" "stocklist-query-code-yahoo")) "\\'"))
      (with-current-buffer (stocklist-get-buffer ,query)
        (buffer-substring-no-properties (point-min) (point-max))))
   (lambda (result)
     (stocklist-handle-result result query))))

(defun stocklist-handle-result (result query)
  "Handle RESULT of QUERY."
  (with-current-buffer (get-buffer-create "*stocklist*")
    (let ((state (when (local-variable-p 'stocklist-state)
                   stocklist-state)))
      (erase-buffer)
      (insert result)
      (stocklist-mode)
      (set (make-local-variable 'stocklist-state)
           (make-stocklist-buffer-state
            :query query
            :ordering stocklist-default-sort))
      (when state
        (setf (stocklist-buffer-state-ordering stocklist-state)
              (stocklist-buffer-state-ordering state)))
      (font-lock-mode -1)
      (stocklist--parse-table)
      (stocklist--fontify)
      (stocklist--sort (stocklist-buffer-state-ordering stocklist-state))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;; TODO: add automatic reverting
(defun stocklist-revert (&optional query)
  "Revert stocklist."
  (interactive (list (stocklist-read-query
                      (stocklist-buffer-state-query stocklist-state))))
  (let ((row (line-number-at-pos))
        (col (org-table-current-column)))
    (stocklist-show query)
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
  (use-local-map stocklist-mode-map)
  (font-lock-mode -1))

(provide 'stocklist)
;;; stocklist.el ends here
