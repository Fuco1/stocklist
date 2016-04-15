;; -*- lexical-binding: t -*-

(require 'f)
(let ((project-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path project-dir))
(require 'stocklist)

(describe "Stocklist parser"

  (spy-on 'parse-csv-string-rows :and-return-value
          '(("Apple Inc." "AAPL" "108.57" "108.80" "11.56" "2.08" "9.40")
            ("Wells Fargo & Company Common St" "WFC" "47.05" "47.10" "11.42" "1.50" "4.12")))

  (it "should parse csv into stocklist-instruments structure"
    (expect (stocklist-parse-data-yahoo "") :to-equal
            (list [cl-struct-stocklist-instrument "Apple Inc." "AAPL" "108.57" "108.80" "11.56" "1.91" "2.08" "9.40" "0.22"]
                  [cl-struct-stocklist-instrument "Wells Fargo & Company Common St" "WFC" "47.05" "47.10" "11.42" "3.18" "1.50" "4.12" "0.36"]))))

(describe "Stocklist renderer"

  (it "should render data as org table"
    (let ((data
           (list
            (make-stocklist-instrument :name "foo corp" :symbol "foo" :bid "1.0" :ask "1.1" :pe "10" :yield "4.5" :dps "2.2" :eps "2.2" :payout "0.22")
            (make-stocklist-instrument :name "bar inc" :symbol "bar" :bid "10.0" :ask "10.10" :pe "11" :yield "4.2" :dps "2.1" :eps "2.0" :payout "0.99"))))
      (with-current-buffer (stocklist-export-to-org-table data)
        (expect
         (buffer-substring-no-properties (point-min) (point-max)) :to-equal
         "| Name     | Symbol |  Bid |   Ask | Pe | Yield | Dps | Eps | Payout |
|----------+--------+------+-------+----+-------+-----+-----+--------|
| foo corp | foo    |  1.0 |   1.1 | 10 |   4.5 | 2.2 | 2.2 |   0.22 |
| bar inc  | bar    | 10.0 | 10.10 | 11 |   4.2 | 2.1 | 2.0 |   0.99 |
")))))

(defmacro with-test-buffer (initial &rest forms)
  "Setup buffer with INITIAL then run FORMS."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,initial)
     (goto-char (point-min))
     (when (re-search-forward "X" nil t)
       (delete-char -1))
     ,@forms))

(defun buffer-content ()
  "Return buffer content with X where the point is."
  (insert "X")
  (buffer-substring-no-properties (point-min) (point-max)))

(describe "Org table utilities"

  (it "Go to beginning of cell"
    (expect (with-test-buffer "|   foXoo |"
              (stocklist-goto-cell-beginning)
              (buffer-content))
            :to-equal "|   Xfooo |")

    (expect (with-test-buffer "| X  fooo |"
              (stocklist-goto-cell-beginning)
              (buffer-content))
            :to-equal "|   Xfooo |"))

  (it "Get information about cell"
    (expect (with-test-buffer "| foo | X  bar |"
              (stocklist-get-cell))
            :to-equal (list :beg 11 :end 14 :content "bar"))))

(describe "Query"

  (it "should select everything with empty query"
    (let ((stocklist-instruments (list
                                  (list "foo" :tags (list "a" "b"))
                                  (list "bar" :tags (list "a" "b"))
                                  (list "baz" :tags (list "a"))
                                  (list "qux" :tags (list "b")))))
      (expect (stocklist-instruments "") :to-equal '("foo" "bar" "baz" "qux"))))

  (it "should select instruments with both tags"
    (let ((stocklist-instruments (list
                                  (list "foo" :tags (list "a" "b"))
                                  (list "bar" :tags (list "a" "b"))
                                  (list "baz" :tags (list "a"))
                                  (list "qux" :tags (list "b")))))
      (expect (stocklist-instruments "a+b") :to-equal '("foo" "bar"))))

  (it "should select instruments with some tags"
    (let ((stocklist-instruments (list
                                  (list "foo" :tags (list "a" "b"))
                                  (list "bar" :tags (list "c" "d"))
                                  (list "baz" :tags (list "a"))
                                  (list "qux" :tags (list "b")))))
      (expect (stocklist-instruments "a|b") :to-equal '("foo" "baz" "qux"))))

  (it "should bind AND more strongly than OR"
    (let ((stocklist-instruments (list
                                  (list "foo" :tags (list "a" "b"))
                                  (list "bar" :tags (list "a" "d"))
                                  (list "baz" :tags (list "a"))
                                  (list "qux" :tags (list "c")))))
      (expect (stocklist-instruments "a+b|c") :to-equal '("foo" "qux"))
      (expect (stocklist-instruments "c|a+b") :to-equal '("foo" "qux"))
      ))

  (it "should not select negative tags"
    (let ((stocklist-instruments (list
                                  (list "foo" :tags (list "a" "b"))
                                  (list "bar" :tags (list "a" "d"))
                                  (list "baz" :tags (list "a"))
                                  (list "qux" :tags (list "c")))))
      (expect (stocklist-instruments "!a") :to-equal '("qux"))))

  (it "should combine negative and positive selection"
    (let ((stocklist-instruments (list
                                  (list "foo" :tags (list "a" "b"))
                                  (list "bar" :tags (list "a" "d"))
                                  (list "baz" :tags (list "b" "d"))
                                  (list "qux" :tags (list "c")))))
      (expect (stocklist-instruments "d+!a") :to-equal '("baz"))
      (expect (stocklist-instruments "!a+d") :to-equal '("baz"))))

  (it "should allow - as sugar for +!"
    (let ((stocklist-instruments (list
                                  (list "foo" :tags (list "a" "b"))
                                  (list "bar" :tags (list "a" "d"))
                                  (list "baz" :tags (list "b" "d"))
                                  (list "qux" :tags (list "d")))))
      (expect (stocklist-instruments "d-a") :to-equal '("baz" "qux")))))
