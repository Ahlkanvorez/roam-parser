(ns roam.parser.peg-util)

;; Memoizing the internal functions produces a packrat parser, which
;; will use far more memory but perform better in edge cases.
;; Not memoizing the internal functions results in faster parses in
;; most cases, though some edge cases will have longer parse times.

(defmacro defparser [parser-name [& cfg] & body]
  (let [do-parse-name (symbol (str "run-" parser-name))]
    `(do
       (def ~do-parse-name
         (fn* [~@cfg] ~@body))
       (def ~parser-name
         ~(let [args (rest cfg)
                params (if (seq args)
                         `[~@args]
                         [])]
            `(fn* ~params
                  ~(if (seq params)
                     `(Parser. ~do-parse-name ~@args)
                     `(Parser. ~do-parse-name nil))))))))
