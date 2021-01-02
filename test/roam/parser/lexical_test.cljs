(ns roam.parser.lexical-test
  (:require [cljs.test :refer-macros [deftest is]]
            [roam.parser.lexical :as parser]
            [roam.parser.tree :as tree]))

(deftest tokenize-test
  (is (= (list "a" "^" "^" "*" "*" "b" "*" "*" "^" "^" "c")
         (parser/tokenize "a^^**b**^^c"))))

(deftest parse-test
  (is (= (tree/Node. :tree [])
         (parser/parse "")))

  (is (= (tree/Node.
          :bracket
          [(tree/Node.
            :bracket
            [(tree/Node. :text ["a"])])])
         (parser/parse "[[a]]")))

  (is (= (tree/Node.
          :paren
          [(tree/Node.
            :paren
            [(tree/Node. :text ["a"])])])
         (parser/parse "((a))")))

  (is (= (tree/Node.
          :code
          [(tree/Node.
            :code
            [(tree/Node. :text ["a"])])])
         (parser/parse "{{a}}")))

  (is (= (tree/Node. :carrot
                       [(tree/Node. :carrot-body
                                      [(tree/Node. :text ["a"])])])
         (parser/parse "^^a^^")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :syntax-e [])
           (tree/Node. :syntax [(tree/Node. :syntax-body
                                                [(tree/Node. :text ["a"])])])
           (tree/Node. :syntax-e [])])
         (parser/parse "```a```")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :bracket [(tree/Node. :text ["a"])])
           (tree/Node. :paren [(tree/Node. :text ["b"])])])
         (parser/parse "[a](b)")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :bracket
                         [(tree/Node. :bracket
                                        [(tree/Node. :text ["a"])])])
           (tree/Node. :paren [(tree/Node. :text ["b"])])])
         (parser/parse "[[a]](b)")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :bracket [(tree/Node. :text ["a"])])
           (tree/Node. :paren
                         [(tree/Node. :paren
                                        [(tree/Node. :text ["b"])])])])
         (parser/parse "[a]((b))")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :bracket
                         [(tree/Node. :bracket
                                        [(tree/Node. :text ["a"])])])
           (tree/Node. :paren
                         [(tree/Node. :paren
                                        [(tree/Node. :text ["b"])])])])
         (parser/parse "[[a]]((b))")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :bracket
                         [(tree/Node. :bracket
                                        [(tree/Node. :text ["a"])])
                          (tree/Node. :bracket
                                        [(tree/Node. :text ["b"])])])
           (tree/Node. :paren
                         [(tree/Node. :paren
                                        [(tree/Node. :text ["c"])])])])
         (parser/parse "[[a][b]]((c))")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :bracket [(tree/Node. :text ["a"])])
           (tree/Node. :paren [(tree/Node. :text ["b"])])])
         (parser/parse "[a](b)")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :bracket
                         [(tree/Node. :bracket
                                        [(tree/Node. :text ["a"])])
                          (tree/Node. :paren
                                        [(tree/Node. :text ["b"])])])
           (tree/Node. :paren
                         [(tree/Node. :paren
                                        [(tree/Node. :text ["c"])])])])
         (parser/parse "[[a](b)]((c))")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :bracket
                         [(tree/Node. :bracket
                                        [(tree/Node. :text ["a"])])
                          (tree/Node. :paren
                                        [(tree/Node. :text ["b"])])])
           (tree/Node. :paren
                         [(tree/Node. :bracket
                                        [(tree/Node. :text ["A"])])
                          (tree/Node. :paren
                                        [(tree/Node. :text ["B"])])])])
         (parser/parse "[[a](b)]([A](B))"))))

(deftest syntax-code-test
  (is (= (tree/Node. :code [(tree/Node. :text ["hello"])])
         (parser/parse "{hello}")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :text ["This is text before the code "])
           (tree/Node. :code [(tree/Node. :text ["2 + 2"])])
           (tree/Node. :text [" This is text after the code"])])
         (parser/parse "This is text before the code {2 + 2} This is text after the code")))

  (is (= (tree/Node.
          :code
          [(tree/Node. :text ["Does it make sense to nest code? "])
           (tree/Node. :code [(tree/Node. :text ["Not really. "])])
           (tree/Node.
            :code
            [(tree/Node. :text ["But we can anyway "])
             (tree/Node. :code [(tree/Node. :text ["2 + 2 + 2"])])])])
         (parser/parse "{Does it make sense to nest code? {Not really. }{But we can anyway {2 + 2 + 2}}}"))))

(deftest terminal-tokens-as-text-test
  ;; Parsing will fail subtly if a terminal token is used as a normal string in text.
  ;; These tests need to exercise that.

  (comment
    (is (= (tree/Node. :text "2 * 2")
           (parser/parse "2 * 2")))

    (is (= (tree/Node. :paren [(tree/Node. :text "the character [ is a left-bracket")])
           (parser/parse "(the character [ is a left-bracket)")))))
