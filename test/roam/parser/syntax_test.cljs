(ns roam.parser.syntax-test
  (:require [cljs.test :refer-macros [deftest is]]
            [roam.parser.tree :as tree]
            [roam.parser.lexical :as lexical]
            [roam.parser.syntax :as syntax]))

(deftest raw-text-test
  (is (= (tree/Node. :tree [])
         (syntax/analyze (lexical/parse ""))))
  (is (= (tree/Node. :text [" "])
         (syntax/analyze (lexical/parse " "))))
  (is (= (tree/Node. :text ["abc"])
         (syntax/analyze (lexical/parse "abc"))))
  (is (= (tree/Node. :text ["123"])
         (syntax/analyze (lexical/parse "123"))))
  (is (= (tree/Node. :text ["abc-123"])
         (syntax/analyze (lexical/parse "abc-123")))))

(deftest basic-test
  (is (= (tree/Node. :link [(tree/Node. :text ["a"])])
         (syntax/analyze
          (lexical/parse "[[a]]"))))

  (is (= (tree/Node. :ref [(tree/Node. :text ["a"])])
         (syntax/analyze
          (lexical/parse "((a))"))))

  (is (= (tree/Node. :render [(tree/Node. :text ["a"])])
         (syntax/analyze
          (lexical/parse "{{a}}"))))

  (is (= (tree/Node. :block [(tree/Node. :text ["a"])])
         (syntax/analyze
          (lexical/parse "```a```"))))

  (is (= (tree/Node. :syntax [(tree/Node. :text ["a"])])
         (syntax/analyze
          (lexical/parse "`a`"))))

  (is (= (tree/Node. :alias
                        [[(tree/Node. :text ["a"])]
                         [(tree/Node. :text ["b"])]])
         (syntax/analyze
          (lexical/parse "[a](b)"))))

  (is (= (tree/Node.
          :alias
          [[(tree/Node. :alias
                           [[(tree/Node. :text ["a"])]
                            [(tree/Node. :text ["b"])]])]
           [(tree/Node. :alias
                           [[(tree/Node. :text ["A"])]
                            [(tree/Node. :text ["B"])]])]])
         (syntax/analyze
          (lexical/parse "[[a](b)]([A](B))"))))

  (is (= (tree/Node.
          :alias
          [[(tree/Node. :text ["W"])
            (tree/Node. :alias
                           [[(tree/Node. :text ["a"])]
                            [(tree/Node. :text ["b"])]])
            (tree/Node. :text ["X"])]
           [(tree/Node. :text ["Y"])
            (tree/Node. :alias
                           [[(tree/Node. :text ["A"])]
                            [(tree/Node. :text ["B"])]])
            (tree/Node. :text ["Z"])]])
         (syntax/analyze
          (lexical/parse "[W[a](b)X](Y[A](B)Z)"))))

  (is (= (tree/Node. :link [(tree/Node. :text ["one "])
                               (tree/Node. :link [(tree/Node. :text ["two "])
                                                     (tree/Node. :link [(tree/Node. :text ["three"])])])])
         (syntax/analyze
          (lexical/parse "[[one [[two [[three]]]]]]"))))

  (is (= (tree/Node.
          :link
          [(tree/Node.
            :link
            [(tree/Node. :link
                            [(tree/Node. :text ["three"])])
             (tree/Node. :text [" two"])])
           (tree/Node. :text [" one"])])
         (syntax/analyze
          (lexical/parse "[[[[[[three]] two]] one]]"))))

  (is (= (tree/Node. :link
                        [(tree/Node. :link [(tree/Node. :text ["a"])])
                         (tree/Node. :text ["b"])])
         (syntax/analyze
          (lexical/parse "[[[[a]]b]]"))))

  (is (= (tree/Node. :render [(tree/Node. :text ["hello roam "])
                                 (tree/Node. :render [(tree/Node. :text ["world"])])])
         (syntax/analyze
          (lexical/parse "{{hello roam {{world}}}}"))))

  (is (= (tree/Node. :tree [(tree/Node. :text ["a"])
                              (tree/Node. :latex [(tree/Node. :text ["b"])])
                              (tree/Node. :text ["c"])])
         (syntax/analyze
          (lexical/parse "a$$b$$c"))))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :text ["a"])
           (tree/Node.
            :block
            [(tree/Node. :text ["b"])
             (tree/Node.
              :render
              [(tree/Node. :render [(tree/Node. :text ["h"])])])
             (tree/Node. :text ["m"])])])
         (syntax/analyze
          (lexical/parse "a```b{{{{h}}}}m```"))))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :text ["a"])
           (tree/Node. :link [(tree/Node. :text ["b"])
                                 (tree/Node. :link [(tree/Node. :text ["c"])])])
           (tree/Node. :text ["d"])
           (tree/Node. :highlight [(tree/Node. :bold [(tree/Node. :text ["e"])])])
           (tree/Node. :text ["f"])
           (tree/Node.
            :syntax
            [(tree/Node.
              :alias
              [[(tree/Node. :text ["g"])]
               [(tree/Node. :link [(tree/Node. :text ["h"])])]])])
           (tree/Node. :text ["i"])])
         (syntax/analyze
          (lexical/parse "a[[b[[c]]]]d^^**e**^^f`[g]([[h]])`i"))))

  (is (= (tree/Node. :tree [(tree/Node. :alias
                                              [[(tree/Node. :text ["abc"])]
                                               [(tree/Node. :text ["123"])]])
                               (tree/Node. :alias
                                              [[(tree/Node. :link [(tree/Node. :text ["Cheddar"])])]
                                               [(tree/Node. :ref [(tree/Node. :text ["Cheese"])])]])
                               (tree/Node. :alias
                                              [[(tree/Node. :render [(tree/Node. :text ["hot"])])]
                                               [(tree/Node. :ref [(tree/Node. :text ["dogs"])])]])])
         (syntax/analyze
          (lexical/parse "[abc](123)[[[Cheddar]]](((Cheese)))[{{hot}}](((dogs)))")))))

(deftest roam-examples-test
  (is (= (tree/Node.
          :tree
          [(tree/Node. :text ["The parser can parse all of Roam's major syntax at least "])
           (tree/Node. :link [(tree/Node. :text ["Nested "])
                                 (tree/Node. :link [(tree/Node. :text ["Links"])])])
           (tree/Node. :text [" and "])
           (tree/Node. :highlight [(tree/Node. :bold [(tree/Node. :text ["bold highlights"])])])
           (tree/Node. :text [" and "])
           (tree/Node. :syntax
                          [(tree/Node. :alias
                                          [[(tree/Node. :text ["html roam"])]
                                           [(tree/Node. :link [(tree/Node. :text ["Aliases"])])]])])
           (tree/Node. :text ["all the ones we haven't done yet as well"])])
         (syntax/analyze
          (lexical/parse "The parser can parse all of Roam's major syntax at least [[Nested [[Links]]]] and ^^**bold highlights**^^ and `[html roam]([[Aliases]])`all the ones we haven't done yet as well"))))

  (is (= (tree/Node. :alias [[(tree/Node. :text ["!"])
                                 (tree/Node. :alias [[(tree/Node. :text ["img"])]
                                                        [(tree/Node. :text ["image-as-alias.com"])]])]
                                [(tree/Node. :text ["www.roamresearch.com"])]])
         (syntax/analyze
          (lexical/parse "[![img](image-as-alias.com)](www.roamresearch.com)"))))

  (is (= (tree/Node. :render [(tree/Node. :render [(tree/Node. :text ["curly braces"])])
                                 (tree/Node. :text [" in"])
                                 (tree/Node. :render [(tree/Node. :text ["side"])])
                                 (tree/Node. :text [" of "])
                                 (tree/Node. :render [(tree/Node. :text ["curly braces"])])])
         (syntax/analyze
          (lexical/parse "{{{{curly braces}} in{{side}} of {{curly braces}}}}"))))

  (is (= (tree/Node. :tree
                        [(tree/Node. :text ["Specifically "])
                         (tree/Node. :block
                                        [(tree/Node. :text ["javascript\n\nAliases inside aliases\n>   \n  "])
                                         (tree/Node. :alias
                                                        [[(tree/Node. :text ["!"])
                                                          (tree/Node. :alias
                                                                         [[(tree/Node. :text ["img"])]
                                                                          [(tree/Node. :text ["image-as-alias.com"])]])]
                                                         [(tree/Node. :text ["www.roamresearch.com"])]])
                                         (tree/Node. :text ["\n\n"])
                                         (tree/Node. :render
                                                        [(tree/Node. :render [(tree/Node. :text ["curly braces"])])
                                                         (tree/Node. :text [" in"])
                                                         (tree/Node. :render [(tree/Node. :text ["side"])])
                                                         (tree/Node. :text [" of "])
                                                         (tree/Node. :render [(tree/Node. :text ["curly braces"])])])
                                         (tree/Node. :text ["\n\n\n"])])])
         (syntax/analyze
          (lexical/parse "Specifically ```javascript

Aliases inside aliases
>   
  [![img](image-as-alias.com)](www.roamresearch.com)

{{{{curly braces}} in{{side}} of {{curly braces}}}}


```")))))

(defn double-bracket-test-cases [open close kind]
  {(str open "hello" close)
   (tree/Node. kind [(tree/Node. :text ["hello"])])

   (str "abc " open "hello" close)
   (tree/Node.
    :tree
    [(tree/Node. :text ["abc "])
     (tree/Node. kind [(tree/Node. :text ["hello"])])])

   (str open "hello" close " 123")
   (tree/Node.
    :tree
    [(tree/Node. kind [(tree/Node. :text ["hello"])])
     (tree/Node. :text [" 123"])])

   (str "abc " open "hello" close " 123")
   (tree/Node.
    :tree
    [(tree/Node. :text ["abc "])
     (tree/Node. kind [(tree/Node. :text ["hello"])])
     (tree/Node. :text [" 123"])])})

(defn nested-double-bracket-test-cases [open close kind]
  (merge
   (double-bracket-test-cases open close kind)
   {(str open open "hello" close " roam world" close)
    (tree/Node.
     kind
     [(tree/Node. kind [(tree/Node. :text ["hello"])])
      (tree/Node. :text [" roam world"])])

    (str open "hello " open "roam" close " world" close)
    (tree/Node.
     kind
     [(tree/Node. :text ["hello "])
      (tree/Node. kind [(tree/Node. :text ["roam"])])
      (tree/Node. :text [" world"])])

    (str open "hello roam " open "world" close close)
    (tree/Node.
     kind
     [(tree/Node. :text ["hello roam "])
      (tree/Node. kind [(tree/Node. :text ["world"])])])

    (str open open open "three" close " two" close " one" close)
    (tree/Node.
     kind
     [(tree/Node.
       kind
       [(tree/Node. kind [(tree/Node. :text ["three"])])
        (tree/Node. :text [" two"])])
      (tree/Node. :text [" one"])])

    (str open open "two " open "three" close close " one" close)
    (tree/Node.
     kind
     [(tree/Node.
       kind
       [(tree/Node. :text ["two "])
        (tree/Node. kind [(tree/Node. :text ["three"])])])
      (tree/Node. :text [" one"])])

    (str open "one " open "two " open "three" close close close)
    (tree/Node.
     kind
     [(tree/Node. :text ["one "])
      (tree/Node.
       kind
       [(tree/Node. :text ["two "])
        (tree/Node. kind [(tree/Node. :text ["three"])])])])

    (str "Nesting! " open "one " open "two " open "three" close close close)
    (tree/Node.
     :tree
     [(tree/Node. :text ["Nesting! "])
      (tree/Node.
       kind
       [(tree/Node. :text ["one "])
        (tree/Node.
         kind
         [(tree/Node. :text ["two "])
          (tree/Node. kind [(tree/Node. :text ["three"])])])])])}))

(deftest syntax-link-test
  (doseq [[text tree] (nested-double-bracket-test-cases "[[" "]]" :link)]
    (is (= tree (syntax/analyze (lexical/parse text))))))

(deftest syntax-ref-test
  (doseq [[text tree] (nested-double-bracket-test-cases "((" "))" :ref)]
    (is (= tree (syntax/analyze (lexical/parse text))))))

(deftest syntax-roam-render-test
  (doseq [[text tree] (nested-double-bracket-test-cases "{{" "}}" :render)]
    (is (= tree (syntax/analyze (lexical/parse text))))))

(deftest syntax-latex-test
  (doseq [[text tree] (double-bracket-test-cases "$$" "$$" :latex)]
    (is (= tree (syntax/analyze (lexical/parse text))))))

(deftest syntax-highlight-test
  (doseq [[text tree] (double-bracket-test-cases "^^" "^^" :highlight)]
    (is (= tree (syntax/analyze (lexical/parse text))))))

(deftest syntax-bold-test
  (doseq [[text tree] (double-bracket-test-cases "**" "**" :bold)]
    (is (= tree (syntax/analyze (lexical/parse text))))))

(deftest syntax-italic-test
  (doseq [[text tree] (double-bracket-test-cases "__" "__" :italic)]
    (is (= tree (syntax/analyze (lexical/parse text))))))

(deftest syntax-syntax-test
  (doseq [[text tree] (double-bracket-test-cases "`" "`" :syntax)]
    (is (= tree (syntax/analyze (lexical/parse text))))))

(deftest syntax-quote-test
  (doseq [[text tree] (double-bracket-test-cases "\"" "\"" :quote)]
    (is (= tree (syntax/analyze (lexical/parse text))))))

(deftest syntax-block-test
  (doseq [[text tree] (double-bracket-test-cases "```" "```" :block)]
    (is (= tree (syntax/analyze (lexical/parse text)))))

  (is (= (tree/Node. :block
                        [(tree/Node. :text ["abc "])
                         (tree/Node. :link
                                        [(tree/Node. :text ["123"])])
                         (tree/Node. :text [" def"])])
         (syntax/analyze (lexical/parse "```abc [[123]] def```"))))

  (is (= (tree/Node. :tree [(tree/Node. :text ["x"])
                               (tree/Node. :block
                                              [(tree/Node. :text ["y"])])
                               (tree/Node. :text ["z"])])
         (syntax/analyze (lexical/parse "x```y```z")))))

(defn syntax-nesting-test-cases [group1 group2 group3]
  (let [[open1 close1 kind1] group1
        [open2 close2 kind2] group2
        [open3 close3 kind3] group3]
    {(str open1 open2 "hello" close2 " roam world" close1)
     (tree/Node.
      kind1
      [(tree/Node. kind2 [(tree/Node. :text ["hello"])])
       (tree/Node. :text [" roam world"])])

     (str open1 "hello " open2 "roam" close2 " world" close1)
     (tree/Node.
      kind1
      [(tree/Node. :text ["hello "])
       (tree/Node. kind2 [(tree/Node. :text ["roam"])])
       (tree/Node. :text [" world"])])

     (str open1 "hello roam " open2 "world" close2 close1)
     (tree/Node.
      kind1
      [(tree/Node. :text ["hello roam "])
       (tree/Node. kind2 [(tree/Node. :text ["world"])])])

     (str open1 open2 open3 "three" close3 " two" close2 " one" close1)
     (tree/Node.
      kind1
      [(tree/Node.
        kind2
        [(tree/Node. kind3 [(tree/Node. :text ["three"])])
         (tree/Node. :text [" two"])])
       (tree/Node. :text [" one"])])

     (str open1 open2 "two " open3 "three" close3 close2 " one" close1)
     (tree/Node.
      kind1
      [(tree/Node.
        kind2
        [(tree/Node. :text ["two "])
         (tree/Node. kind3 [(tree/Node. :text ["three"])])])
       (tree/Node. :text [" one"])])

     (str open1 "one " open2 "two " open3 "three" close3 close2 close1)
     (tree/Node.
      kind1
      [(tree/Node. :text ["one "])
       (tree/Node.
        kind2
        [(tree/Node. :text ["two "])
         (tree/Node. kind3 [(tree/Node. :text ["three"])])])])

     (str "Nesting! " open1 "one " open2 "two " open3 "three" close3 close2 close1)
     (tree/Node.
      :tree
      [(tree/Node. :text ["Nesting! "])
       (tree/Node.
        kind1
        [(tree/Node. :text ["one "])
         (tree/Node.
          kind2
          [(tree/Node. :text ["two "])
           (tree/Node. kind3
                          [(tree/Node. :text ["three"])])])])])}))

(def combination-test-inputs
  (let [latex ["$$" "$$" :latex]
        highlight ["^^" "^^" :highlight]
        bold ["**" "**" :bold]
        italic ["__" "__" :italic]
        link ["[[" "]]" :link]
        ref ["((" "))" :ref]
        render ["{{" "}}" :render]
        syntax ["`" "`" :syntax]
        quote ["\"" "\"" :quote]
        block ["```" "```" :block]
        groups [latex highlight bold italic link ref render quote]
        ;; It doesn't make sense to nest syntax & blocks together,
        ;; so split test cases into two groups, one with each & not the other.
        groups-a (conj groups syntax)
        groups-b (conj groups block)]
    (map first
         (flatten
          (for [groups [groups-a groups-b]]
            (for [a groups]
              (for [b (remove #{a} groups)]
                (for [c (remove #{a b} groups)]
                  #{[a b c]}))))))))

(deftest syntax-nesting-test
  (doseq [[a b c] combination-test-inputs]
    (doseq [[text tree] (syntax-nesting-test-cases a b c)]
      (is (= tree (syntax/analyze (lexical/parse text)))))))

(deftest all-together-test
  (is (= (tree/Node.
          :tree
          [(tree/Node. :text ["hello "])
           (tree/Node.
            :link
            [(tree/Node. :text ["world "])
             (tree/Node.
              :ref
              [(tree/Node. :text ["lots "])
               (tree/Node.
                :render
                [(tree/Node. :text ["of "])
                 (tree/Node. :bold
                                [(tree/Node. :text ["nested"])])])
               (tree/Node. :text [" "])
               (tree/Node.
                :highlight
                [(tree/Node. :italic
                                [(tree/Node. :text ["stuff"])])
                 (tree/Node. :text [" here"])])])
             (tree/Node. :text [" "])
             (tree/Node.
              :latex
              [(tree/Node. :text ["really, "])
               (tree/Node. :bold [(tree/Node. :text ["lots"])])])])
           (tree/Node. :text ["!"])])
         (syntax/analyze
          (lexical/parse "hello [[world ((lots {{of **nested**}} ^^__stuff__ here^^)) $$really, **lots**$$]]!")))))

(deftest syntax-alias-test
  (is (= (tree/Node.
          :alias
          [[(tree/Node. :text ["hello"])]
           [(tree/Node. :text ["roam"])]])
         (syntax/analyze
          (lexical/parse "[hello](roam)"))))

  (is (= (tree/Node.
          :alias
          [[(tree/Node. :text ["hello "])
            (tree/Node. :alias [[(tree/Node. :text ["roam"])]
                                   [(tree/Node. :text ["world"])]])]
           [(tree/Node. :text ["cheese"])]])
         (syntax/analyze
          (lexical/parse "[hello [roam](world)](cheese)"))))

  (is (= (tree/Node.
          :alias
          [[(tree/Node. :text ["Oh, "])
            (tree/Node. :alias [[(tree/Node. :text ["hello, "])]
                                   [(tree/Node. :text ["world"])]])
            (tree/Node. :text [" "])]
           [(tree/Node. :text ["the-end"])]])
         (syntax/analyze
          (lexical/parse "[Oh, [hello, ](world) ](the-end)"))))

  (is (= (tree/Node.
          :alias
          [[(tree/Node. :alias [[(tree/Node. :text ["hello"])]
                                   [(tree/Node. :text ["there"])]])]
           [(tree/Node. :text ["aliases"])]])
         (syntax/analyze
          (lexical/parse "[[hello](there)](aliases)"))))

  (is (= (tree/Node.
          :alias
          [[(tree/Node. :text ["Hi"])]
           [(tree/Node. :alias [[(tree/Node. :text ["its an"])]
                                   [(tree/Node. :text ["alias"])]])]])
         (syntax/analyze
          (lexical/parse "[Hi]([its an](alias))"))))

  (is (= (tree/Node.
          :alias
          [[(tree/Node. :alias [[(tree/Node. :text ["Not a"])]
                                   [(tree/Node. :text ["link"])]])]
           [(tree/Node. :text ["its "])
            (tree/Node. :alias [[(tree/Node. :text ["an"])]
                                   [(tree/Node. :text ["alias"])]])]])
         (syntax/analyze
          (lexical/parse "[[Not a](link)](its [an](alias))"))))

  (is (= (tree/Node.
          :alias
          [[(tree/Node.
             :alias
             [[(tree/Node.
                :alias
                [[(tree/Node. :text ["a"])]
                 [(tree/Node. :text ["b"])]])]
              [(tree/Node. :text ["c"])]])]
           [(tree/Node. :text ["d"])]])
         (syntax/analyze
          (lexical/parse "[[[a](b)](c)](d)")))))


(deftest complex-inputs-test
  (is (= (tree/Node.
          :render
          [(tree/Node.
            :italic
            [(tree/Node.
              :link
              [(tree/Node.
                :render
                [(tree/Node. :text ["\\\""])
                 (tree/Node.
                  :alias
                  [[(tree/Node. :code [(tree/Node. :text ["A"])])]
                   [(tree/Node. :text ["\\\""])
                    (tree/Node.
                     :alias
                     [[(tree/Node. :text ["B"])]
                      [(tree/Node.
                        :alias
                        [[(tree/Node.
                           :alias
                           [[(tree/Node.
                              :highlight
                              [(tree/Node.
                                :italic
                                [(tree/Node.
                                  :ref
                                  [(tree/Node.
                                    :latex
                                    [(tree/Node.
                                      :link
                                      [(tree/Node.
                                        :render
                                        [(tree/Node. :text ["C"])])])])])])])]
                            [(tree/Node. :text ["D"])]])]
                         [(tree/Node.
                           :link
                           [(tree/Node.
                             :italic
                             [(tree/Node.
                               :highlight
                               [(tree/Node.
                                 :latex
                                 [(tree/Node. :text ["E"])])])])])]])]])
                    (tree/Node. :text ["\\\""])]])
                 (tree/Node. :text ["\\\""])])])])])
         (syntax/analyze
          (lexical/parse
           "{{__[[{{\\\"[{A}](\\\"[B]([[^^__(($$[[{{C}}]]$$))__^^](D)]([[__^^$$E$$^^__]]))\\\")\\\"}}]]__}}")))))

(deftest node->str-test
    (is (= "abc"
           (lexical/node->str (tree/Node. :text ["abc"]))))
    (is (= "[hello]"
           (lexical/node->str (tree/Node. :text ["[hello]"]))))
    (is (= "[[hello links]]"
           (lexical/node->str
            (tree/Node. :link
                           [(tree/Node. :text ["hello links"])]))))
    (is (= "[[hello [[nested [[links]]]]]]"
           (lexical/node->str
            (tree/Node.
             :link
             [(tree/Node. :text ["hello "])
              (tree/Node.
               :link
               [(tree/Node. :text ["nested "])
                (tree/Node. :link
                               [(tree/Node. :text ["links"])])])]))))
    (is (= "this is [[probably [[[[enough]] linking]] for]] now."
           (lexical/node->str
            (tree/Node.
             :tree
             [(tree/Node. :text ["this is "])
              (tree/Node.
               :link
               [(tree/Node. :text ["probably "])
                (tree/Node.
                 :link
                 [(tree/Node. :link [(tree/Node. :text ["enough"])])
                  (tree/Node. :text [" linking"])])
                (tree/Node. :text [" for"])])
              (tree/Node. :text [" now."])]))))
    (is (= "hello [[world ((lots {{of **nested**}} ^^__stuff__ here^^)) $$really, **lots**$$]]!"
           (lexical/node->str
            (tree/Node.
             :tree
             [(tree/Node. :text ["hello "])
              (tree/Node.
               :link
               [(tree/Node. :text ["world "])
                (tree/Node.
                 :ref
                 [(tree/Node. :text ["lots "])
                  (tree/Node.
                   :render
                   [(tree/Node. :text ["of "])
                    (tree/Node. :bold
                                   [(tree/Node. :text ["nested"])])])
                  (tree/Node. :text [" "])
                  (tree/Node.
                   :highlight
                   [(tree/Node. :italic
                                   [(tree/Node. :text ["stuff"])])
                    (tree/Node. :text [" here"])])])
                (tree/Node. :text [" "])
                (tree/Node.
                 :latex
                 [(tree/Node. :text ["really, "])
                  (tree/Node. :bold [(tree/Node. :text ["lots"])])])])
              (tree/Node. :text ["!"])])))))

(deftest invertability-test
  (doseq [[a b c] combination-test-inputs]
    (doseq [[text _tree] (syntax-nesting-test-cases a b c)]
      (is (= text
             (-> text lexical/parse syntax/analyze lexical/node->str))))))

(deftest syntax-escapes-test
  (is (= (tree/Node.
          :link
          [(tree/Node. :text ["hello "])
           (tree/Node.
            :syntax
            [(tree/Node. :text ["quoted \\\"and \\`escaped\\`\\\" world"])])
           (tree/Node. :text [" with "])
           (tree/Node. :italic [(tree/Node. :text ["links"])])])
         (syntax/analyze
          (lexical/parse
           "[[hello `quoted \\\"and \\`escaped\\`\\\" world` with __links__]]"))))

  (is (= (tree/Node.
          :block
          [(tree/Node. :text ["Syntax quotes have "])
           (tree/Node. :bold [(tree/Node. :text ["\\`"])])
           (tree/Node. :text [" before and after them "])
           (tree/Node. :bold [(tree/Node. :text ["\\`"])])
           (tree/Node. :text [" like that"])])
         (syntax/analyze
          (lexical/parse
           "```Syntax quotes have **\\`** before and after them **\\`** like that```")))))
