(ns roam.parser.lexical-test
  (:require [cljs.test :refer-macros [deftest is]]
            [roam.parser.lexical :as lexical]
            [roam.parser.tree :as tree]))

(deftest tokenize-test
  (is (= (list "a" "^^" "**" "b" "**" "^^" "c")
         (lexical/tokenize "a^^**b**^^c"))))

(deftest parse-test
  (is (= (tree/Node. :tree [])
         (lexical/parse "")))

  (is (= (tree/Node.
          :bracket
          [(tree/Node.
            :bracket
            [(tree/Node. :text ["a"])])])
         (lexical/parse "[[a]]")))

  (is (= (tree/Node.
          :paren
          [(tree/Node.
            :paren
            [(tree/Node. :text ["a"])])])
         (lexical/parse "((a))")))

  (is (= (tree/Node.
          :code
          [(tree/Node.
            :code
            [(tree/Node. :text ["a"])])])
         (lexical/parse "{{a}}")))

  (is (= (tree/Node. :carrot
                     [(tree/Node. :carrot-body
                                  [(tree/Node. :text ["a"])])])
         (lexical/parse "^^a^^")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :syntax-e [])
           (tree/Node. :syntax [(tree/Node. :syntax-body
                                            [(tree/Node. :text ["a"])])])
           (tree/Node. :syntax-e [])])
         (lexical/parse "```a```")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :bracket [(tree/Node. :text ["a"])])
           (tree/Node. :paren [(tree/Node. :text ["b"])])])
         (lexical/parse "[a](b)")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :bracket
                       [(tree/Node. :bracket
                                    [(tree/Node. :text ["a"])])])
           (tree/Node. :paren [(tree/Node. :text ["b"])])])
         (lexical/parse "[[a]](b)")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :bracket [(tree/Node. :text ["a"])])
           (tree/Node. :paren
                       [(tree/Node. :paren
                                    [(tree/Node. :text ["b"])])])])
         (lexical/parse "[a]((b))")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :bracket
                       [(tree/Node. :bracket
                                    [(tree/Node. :text ["a"])])])
           (tree/Node. :paren
                       [(tree/Node. :paren
                                    [(tree/Node. :text ["b"])])])])
         (lexical/parse "[[a]]((b))")))

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
         (lexical/parse "[[a][b]]((c))")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :bracket [(tree/Node. :text ["a"])])
           (tree/Node. :paren [(tree/Node. :text ["b"])])])
         (lexical/parse "[a](b)")))

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
         (lexical/parse "[[a](b)]((c))")))

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
         (lexical/parse "[[a](b)]([A](B))"))))

(deftest syntax-code-test
  (is (= (tree/Node. :code [(tree/Node. :text ["hello"])])
         (lexical/parse "{hello}")))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :text ["This is text before the code "])
           (tree/Node. :code [(tree/Node. :text ["2 + 2"])])
           (tree/Node. :text [" This is text after the code"])])
         (lexical/parse "This is text before the code {2 + 2} This is text after the code")))

  (is (= (tree/Node.
          :code
          [(tree/Node. :text ["Does it make sense to nest code? "])
           (tree/Node. :code [(tree/Node. :text ["Not really. "])])
           (tree/Node.
            :code
            [(tree/Node. :text ["But we can anyway "])
             (tree/Node. :code [(tree/Node. :text ["2 + 2 + 2"])])])])
         (lexical/parse "{Does it make sense to nest code? {Not really. }{But we can anyway {2 + 2 + 2}}}"))))

(deftest ungrammatical-tokens-test
  (is (= (tree/Node. :text ["(a ] b)"])
         (lexical/parse "(a ] b)")))

  (is (= (tree/Node. :text ["(a [ b)"])
         (lexical/parse "(a [ b)")))

  (is (= (tree/Node. :text ["([ b)"])
         (lexical/parse "([ b)")))

  (is (= (tree/Node. :text ["[[a]"])
         (lexical/parse "[[a]"))))
