(ns roam.parser.roam-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [roam.parser.roam :as roam]))

(deftest parse-test
  (is (= {:text [""]} (roam/parse "")))

  (is (= {:text ["abc"]} (roam/parse "abc")))

  (is (= {:text ["abc\n123"]} (roam/parse "abc
123")))

  (is (= {:text [{:escape ["\""]}]} (roam/parse "\\\"")))

  (is (= {:link [{:text ["a"]}]} (roam/parse "[[a]]")))

  (is (= {:ref [{:text ["a"]}]} (roam/parse "((a))")))

  (is (= {:render [{:text ["a"]}]} (roam/parse "{{a}}")))

  (is (= {:code [{:text ["a"]}]} (roam/parse "{a}")))

  (is (= {:highlight [{:text ["a"]}]} (roam/parse "^^a^^")))

  (is (= {:block [{:text ["a"]}]} (roam/parse "```a```")))

  (is (= {:alias [{:text ["a"]} {:text ["b"]}]}
         (roam/parse "[a](b)")))

  (is (= {:link [{:text ["a" {:escape ["\""]} "b"]}]}
         (roam/parse "[[a\\\"b]]")))

  (is (= {:link [{:text ["a" {:escape ["]"]} {:escape ["]"]} "b"]}]}
         (roam/parse "[[a\\]\\]b]]")))

  (is (= {:alias [{:bracket [{:text ["a"]}]}
                  {:paren [{:text ["b"]}]}]}
         (roam/parse "[[a]]((b))")))

  (is (= {:tree [{:link [{:text ["a"]}]}
                 {:text [" "]}
                 {:ref [{:text ["b"]}]}]}
         (roam/parse "[[a]] ((b))")))

  (is (= {:alias [{:tree [{:link [{:text ["a"]}]}
                          {:link [{:text ["b"]}]}]}
                  {:text ["c"]}]}
         (roam/parse "[[[a]][[b]]](c)")))

  (is (= {:alias [{:alias [{:text ["a"]} {:text ["b"]}]}
                  {:ref [{:text ["c"]}]}]}
         (roam/parse "[[a](b)](((c)))")))

  (is (= {:alias [{:alias [{:text ["a"]} {:text ["b"]}]}
                  {:alias [{:text ["A"]} {:text ["B"]}]}]}
         (roam/parse "[[a](b)]([A](B))")))

  (is (= {:alias [{:tree [{:text ["W"]}
                          {:alias [{:text ["a"]} {:text ["b"]}]}
                          {:text ["X"]}]}
                  {:tree [{:text ["Y"]}
                          {:alias [{:text ["A"]} {:text ["B"]}]}
                          {:text ["Z"]}]}]}
         (roam/parse "[W[a](b)X](Y[A](B)Z)")))

  (is (= {:link
          [{:tree
            [{:text ["one "]}
             {:link [{:tree [{:text ["two "]}
                             {:link [{:text ["three"]}]}]}]}]}]}
         (roam/parse "[[one [[two [[three]]]]]]")))

  (is (= {:link [{:tree [{:link [{:tree [{:link [{:text ["three"]}]}
                                         {:text [" two"]}]}]}
                         {:text [" one"]}]}]}
         (roam/parse "[[[[[[three]] two]] one]]")))

  (is (= {:tree [{:text ["a"]}
                 {:latex [{:text ["b"]}]}
                 {:text ["c"]}]}
         (roam/parse "a$$b$$c")))

  (is (= {:tree [{:text ["a"]}
                 {:block [{:tree [{:text ["b"]}
                                  {:render [{:render [{:text ["h"]}]}]}
                                  {:text ["m"]}]}]}]}
         (roam/parse "a```b{{{{h}}}}m```")))

  (is (= {:tree [{:text ["a"]}
                 {:link [{:tree [{:text ["b"]}
                                 {:link [{:text ["c"]}]}]}]}
                 {:text ["d"]}
                 {:highlight [{:bold [{:text ["e"]}]}]}
                 {:text ["f"]}
                 {:syntax [{:alias [{:text ["g"]}
                                    {:link [{:text ["h"]}]}]}]}
                 {:text ["i"]}]}
         (roam/parse "a[[b[[c]]]]d^^**e**^^f`[g]([[h]])`i")))

  (is (= {:tree [{:alias [{:text ["abc"]} {:text ["123"]}]}
                 {:alias [{:link [{:text ["Cheddar"]}]}
                          {:ref [{:text ["Cheese"]}]}]}
                 {:alias [{:render [{:text ["hot"]}]}
                          {:ref [{:text ["dogs"]}]}]}]}
         (roam/parse
          "[abc](123)[[[Cheddar]]](((Cheese)))[{{hot}}](((dogs)))")))

  (testing "Failed parses should return the whole string as a text node"
    (is (= {:text ["w[[x[[y]]]]z^^**q**^^w`[e]([[r]])`t```y[[a[[s```))))"]}
           (roam/parse "w[[x[[y]]]]z^^**q**^^w`[e]([[r]])`t```y[[a[[s```))))")))))

 (deftest roam-examples-test
   (is (= {:tree [{:text ["The parser can parse all of Roam's major syntax at least "]}
                  {:link [{:tree [{:text ["Nested "]}
                                  {:link [{:text ["Links"]}]}]}]}
                  {:text [" and "]}
                  {:highlight [{:bold [{:text ["bold highlights"]}]}]}
                  {:text [" and "]}
                  {:syntax [{:alias [{:text ["html roam"]}
                                     {:link [{:text ["Aliases"]}]}]}]}
                  {:text ["all the ones we haven't done yet as well"]}]}
          (roam/parse "The parser can parse all of Roam's major syntax at least [[Nested [[Links]]]] and ^^**bold highlights**^^ and `[html roam]([[Aliases]])`all the ones we haven't done yet as well")))

   (is (= {:alias [{:tree [{:text ["!"]}
                           {:alias [{:text ["img"]}
                                    {:text ["image-as-alias.com"]}]}]}
                   {:text ["www.roamresearch.com"]}]}
          (roam/parse
           "[![img](image-as-alias.com)](www.roamresearch.com)")))

   (is (= {:render [{:tree [{:render [{:text ["curly braces"]}]}
                            {:text [" in"]}
                            {:render [{:text ["side"]}]}
                            {:text [" of "]}
                            {:render [{:text ["curly braces"]}]}]}]}
          (roam/parse
           "{{{{curly braces}} in{{side}} of {{curly braces}}}}")))

   (is (= {:tree
           [{:text ["Specifically "]}
            {:block
             [{:tree
               [{:text
                 ["javascript\n\nAliases inside aliases\n>   \n  "]}
                {:alias
                 [{:tree [{:text ["!"]}
                          {:alias [{:text ["img"]}
                                   {:text ["image-as-alias.com"]}]}]}
                  {:text ["www.roamresearch.com"]}]}
                {:text ["\n\n"]}
                {:render
                 [{:tree [{:render [{:text ["curly braces"]}]}
                          {:text [" in"]}
                          {:render [{:text ["side"]}]}
                          {:text [" of "]}
                          {:render [{:text ["curly braces"]}]}]}]}
                {:text ["\n\n\n"]}]}]}]}
          (roam/parse "Specifically ```javascript

Aliases inside aliases
>   
  [![img](image-as-alias.com)](www.roamresearch.com)

{{{{curly braces}} in{{side}} of {{curly braces}}}}


```"))))

 (defn double-bracket-test-cases [open close kind]
   {(str open "hello" close)
    {kind [{:text ["hello"]}]}

    (str "abc " open "hello" close)
    {:tree [{:text ["abc "]} {kind [{:text ["hello"]}]}]}

    (str open "hello" close " 123")
    {:tree [{kind [{:text ["hello"]}]} {:text [" 123"]}]}

    (str "abc " open "hello" close " 123")
    {:tree [{:text ["abc "]}
            {kind [{:text ["hello"]}]}
            {:text [" 123"]}]}})

 (defn nested-double-bracket-test-cases [open close kind]
   (merge
    (double-bracket-test-cases open close kind)
    {(str open open "hello" close " roam world" close)
     {kind [{:tree [{kind [{:text ["hello"]}]}
                    {:text [" roam world"]}]}]}

     (str open "hello " open "roam" close " world" close)
     {kind [{:tree [{:text ["hello "]}
                    {kind [{:text ["roam"]}]}
                    {:text [" world"]}]}]}

     (str open "hello roam " open "world" close close)
     {kind [{:tree [{:text ["hello roam "]}
                    {kind [{:text ["world"]}]}]}]}

     (str open open open "three" close " two" close " one" close)
     {kind [{:tree [{kind [{:tree [{kind [{:text ["three"]}]}
                                   {:text [" two"]}]}]}
                    {:text [" one"]}]}]}

     (str open open "two " open "three" close close " one" close)
     {kind [{:tree [{kind [{:tree [{:text ["two "]}
                                   {kind [{:text ["three"]}]}]}]}
                    {:text [" one"]}]}]}

     (str open "one " open "two " open "three" close close close)
     {kind [{:tree [{:text ["one "]}
                    {kind [{:tree [{:text ["two "]}
                                   {kind [{:text ["three"]}]}]}]}]}]}

     (str "Nesting! " open "one " open "two " open "three" close close close)
     {:tree
      [{:text ["Nesting! "]}
       {kind [{:tree
               [{:text ["one "]}
                {kind [{:tree
                        [{:text ["two "]}
                         {kind [{:text ["three"]}]}]}]}]}]}]}}))

 (deftest syntax-link-test
   (doseq [[text tree] (nested-double-bracket-test-cases "[[" "]]" :link)]
     (is (= tree (roam/parse text)))))

 (deftest syntax-ref-test
   (doseq [[text tree] (nested-double-bracket-test-cases "((" "))" :ref)]
     (is (= tree (roam/parse text)))))

 (deftest syntax-roam-render-test
   (doseq [[text tree] (nested-double-bracket-test-cases "{{" "}}" :render)]
     (is (= tree (roam/parse text)))))

 (deftest syntax-latex-test
   (doseq [[text tree] (double-bracket-test-cases "$$" "$$" :latex)]
     (is (= tree (roam/parse text)))))

 (deftest syntax-highlight-test
   (doseq [[text tree] (double-bracket-test-cases "^^" "^^" :highlight)]
     (is (= tree (roam/parse text)))))

 (deftest syntax-bold-test
   (doseq [[text tree] (double-bracket-test-cases "**" "**" :bold)]
     (is (= tree (roam/parse text)))))

 (deftest syntax-italic-test
   (doseq [[text tree] (double-bracket-test-cases "__" "__" :italic)]
     (is (= tree (roam/parse text)))))

 (deftest syntax-syntax-test
   (doseq [[text tree] (double-bracket-test-cases "`" "`" :syntax)]
     (is (= tree (roam/parse text)))))

 (deftest syntax-quote-test
   (doseq [[text tree] (double-bracket-test-cases "\"" "\"" :quote)]
     (is (= tree (roam/parse text)))))

 (deftest syntax-block-test
   (doseq [[text tree] (double-bracket-test-cases "```" "```" :block)]
     (is (= tree (roam/parse text))))

   (is (= {:block [{:tree [{:text ["abc "]}
                           {:link [{:text ["123"]}]}
                           {:text [" def"]}]}]}
          (roam/parse "```abc [[123]] def```")))

   (is (= {:tree [{:text ["x"]}
                  {:block [{:text ["y"]}]}
                  {:text ["z"]}]}
          (roam/parse "x```y```z"))))

 (deftest syntax-code-test
   (is (= {:tree [{:text ["abc "]}
                  {:code [{:text ["2 + 2"]}]}
                  {:text [" 123"]}]}
          (roam/parse "abc {2 + 2} 123")))

   (is (= {:code
           [{:tree
             [{:text ["abc "]}
              {:code [{:text ["123"]}]}
              {:code
               [{:tree [{:text ["def"]}
                        {:code [{:text ["2 + 2 + 2"]}]}]}]}]}]}
          (roam/parse "{abc {123}{def{2 + 2 + 2}}}"))))

 (defn syntax-nesting-test-cases [group1 group2 group3]
   (let [[open1 close1 kind1] group1
         [open2 close2 kind2] group2
         [open3 close3 kind3] group3]
     {(str open1 open2 "hello" close2 " roam world" close1)
      {kind1 [{:tree [{kind2 [{:text ["hello"]}]}
                      {:text [" roam world"]}]}]}

      (str open1 "hello " open2 "roam" close2 " world" close1)
      {kind1 [{:tree [{:text ["hello "]}
                      {kind2 [{:text ["roam"]}]}
                      {:text [" world"]}]}]}

      (str open1 "hello roam " open2 "world" close2 close1)
      {kind1 [{:tree [{:text ["hello roam "]}
                      {kind2 [{:text ["world"]}]}]}]}

      (str open1 open2 open3 "three" close3 " two" close2 " one" close1)
      {kind1 [{:tree [{kind2 [{:tree [{kind3 [{:text ["three"]}]}
                                      {:text [" two"]}]}]}
                      {:text [" one"]}]}]}

      (str open1 open2 "two " open3 "three" close3 close2 " one" close1)
      {kind1 [{:tree [{kind2 [{:tree [{:text ["two "]}
                                      {kind3 [{:text ["three"]}]}]}]}
                      {:text [" one"]}]}]}

      (str open1 "one " open2 "two " open3 "three" close3 close2 close1)
      {kind1 [{:tree [{:text ["one "]}
                      {kind2 [{:tree [{:text ["two "]}
                                      {kind3 [{:text ["three"]}]}]}]}]}]}

      (str "Nesting! " open1 "one " open2 "two " open3 "three" close3 close2 close1)
      {:tree [{:text ["Nesting! "]}
              {kind1 [{:tree
                       [{:text ["one "]}
                        {kind2 [{:tree
                                 [{:text ["two "]}
                                  {kind3 [{:text ["three"]}]}]}]}]}]}]}}))

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
       (is (= tree (roam/parse text))))))

 (deftest all-together-test
   (is (= {:tree
           [{:text ["hello "]}
            {:link
             [{:tree
               [{:text ["world "]}
                {:ref
                 [{:tree
                   [{:text ["lots "]}
                    {:render
                     [{:tree
                       [{:text ["of "]}
                        {:bold [{:text ["nested"]}]}]}]}
                    {:text [" "]}
                    {:highlight
                     [{:tree [{:italic [{:text ["stuff"]}]}
                              {:text [" here"]}]}]}]}]}
                {:text [" "]}
                {:latex [{:tree [{:text ["really, "]}
                                 {:bold [{:text ["lots"]}]}]}]}]}]}
            {:text ["!"]}]} 
          (roam/parse
           "hello [[world ((lots {{of **nested**}} ^^__stuff__ here^^)) $$really, **lots**$$]]!"))))

 (deftest syntax-alias-test
   (is (= {:alias [{:text ["hello"]} {:text ["roam"]}]}
          (roam/parse "[hello](roam)")))

   (is (= {:alias [{:tree [{:text ["hello "]}
                           {:alias [{:text ["roam"]}
                                    {:text ["world"]}]}]}
                   {:text ["cheese"]}]}
          (roam/parse "[hello [roam](world)](cheese)")))

   (is (= {:alias [{:tree [{:text ["Oh, "]}
                           {:alias [{:text ["hello, "]}
                                    {:text ["world"]}]}
                           {:text [" "]}]}
                   {:text ["the-end"]}]}
          (roam/parse "[Oh, [hello, ](world) ](the-end)")))

   (is (= {:alias [{:alias [{:text ["hello"]} {:text ["there"]}]}
                   {:text ["aliases"]}]}
          (roam/parse "[[hello](there)](aliases)")))

   (is (= {:alias [{:text ["Hi"]}
                   {:alias [{:text ["its an"]}
                            {:text ["alias"]}]}]}
          (roam/parse "[Hi]([its an](alias))")))

   (is (= {:alias [{:alias [{:text ["Not a"]}
                            {:text ["link"]}]}
                   {:tree [{:text ["its "]}
                           {:alias [{:text ["an"]}
                                    {:text ["alias"]}]}]}]}
          (roam/parse "[[Not a](link)](its [an](alias))")))

   (is (= {:alias [{:alias [{:alias [{:text ["a"]}
                                     {:text ["b"]}]}
                            {:text ["c"]}]}
                   {:text ["d"]}]}
          (roam/parse "[[[a](b)](c)](d)"))))

 (deftest complex-inputs-test
   (is (= {:render
           [{:italic
             [{:link
               [{:render
                 [{:tree
                   [{:text [{:escape ["\""]}]}
                    {:alias
                     [{:code [{:text ["A"]}]}
                      {:tree
                       [{:text [{:escape ["\""]}]}
                        {:alias
                         [{:text ["B"]}
                          {:alias
                           [{:alias
                             [{:highlight
                               [{:italic
                                 [{:ref
                                   [{:latex
                                     [{:link
                                       [{:render [{:text ["C"]}]}]}]}]}]}]}
                              {:text ["D"]}]}
                            {:link
                             [{:italic
                               [{:highlight
                                 [{:latex [{:text ["E"]}]}]}]}]}]}]}
                        {:text [{:escape ["\""]}]}]}]}
                    {:text [{:escape ["\""]}]}]}]}]}]}]}
          (roam/parse
           "{{__[[{{\\\"[{A}](\\\"[B]([[^^__(($$[[{{C}}]]$$))__^^](D)]([[__^^$$E$$^^__]]))\\\")\\\"}}]]__}}"))))

 (deftest syntax-escapes-test
   (is (= {:link
           [{:tree
             [{:text ["hello "]}
              {:syntax
               [{:text ["quoted "
                        {:escape ["\""]}
                        "and "
                        {:escape ["`"]}
                        "escaped"
                        {:escape ["`"]}
                        {:escape ["\""]}
                        " world"]}]}
              {:text [" with "]}
              {:italic [{:text ["links"]}]}]}]}
          (roam/parse
           "[[hello `quoted \\\"and \\`escaped\\`\\\" world` with __links__]]")))

   (is (= {:block
           [{:tree
             [{:text ["Syntax quotes have "]}
              {:bold [{:text [{:escape ["`"]}]}]}
              {:text [" before and after them "]}
              {:bold [{:text [{:escape ["`"]}]}]}
              {:text [" like that"]}]}]}
          (roam/parse
           "```Syntax quotes have **\\`** before and after them **\\`** like that```")))

   (is (= {:link [{:text ["a "
                          {:escape ["["]}
                          " b"]}]}
          (roam/parse "[[a \\[ b]]")))

   (is (= {:link [{:text [" a "
                          {:escape ["("]}
                          " b "]}]}
          (roam/parse "[[ a \\( b ]]")))

   (is (= {:link [{:text [" a "
                          {:escape [")"]}
                          " b "]}]}
          (roam/parse "[[ a \\) b ]]"))))

 (deftest ungrammatical-tokens-test
   (is (= {:text ["(a ] b)"]}
          (roam/parse "(a ] b)")))

   (is (= {:text ["(a [ b)"]}
          (roam/parse "(a [ b)")))

   (is (= {:text ["([ b)"]}
          (roam/parse "([ b)")))

   (is (= {:text ["[[a]"]}
          (roam/parse "[[a]")))

   (is (= {:text ["[[a [ b]]"]}
          (roam/parse "[[a [ b]]")))

   (is (= {:text ["[[ a ( b ]]"]}
          (roam/parse "[[ a ( b ]]")))

   (is (= {:text ["[[ a ) b ]]"]}
          (roam/parse "[[ a ) b ]]"))))

 (deftest node->str-test
   (is (= "abc"
          (roam/tree->str {:text ["abc"]})))
   (is (= "{hello}"
          (roam/tree->str {:code [{:text ["hello"]}]})))
   (is (= "[[hello links]]"
          (roam/tree->str {:link [{:text ["hello links"]}]})))

   (is (= "[[hello \"quotes\"]]"
          (roam/tree->str {:link
                           [{:tree
                             [{:text ["hello "]}
                              {:quote [{:text ["quotes"]}]}]}]})))

   (is (= "[[hello [[nested [[links]]]]]]"
          (roam/tree->str
           {:link [{:tree
                    [{:text ["hello "]}
                     {:link [{:tree
                              [{:text ["nested "]}
                               {:link [{:text ["links"]}]}]}]}]}]})))

   (is (= "this is [[probably [[[[enough]] linking]] for]] now."
          (roam/tree->str
           {:tree [{:text ["this is "]}
                   {:link [{:tree
                            [{:text ["probably "]}
                             {:link
                              [{:tree [{:link [{:text ["enough"]}]}
                                       {:text [" linking"]}]}]}
                             {:text [" for"]}]}]}
                   {:text [" now."]}]})))

   (is (= "hello [[world ((lots {{of **nested**}} ^^__stuff__ here^^)) $$really, **lots**$$]]!"
          (roam/tree->str
           {:tree
            [{:text ["hello "]}
             {:link
              [{:tree [{:text ["world "]}
                       {:ref
                        [{:tree [{:text ["lots "]}
                                 {:render
                                  [{:tree [{:text ["of "]}
                                           {:bold [{:text ["nested"]}]}]}]}
                                 {:text [" "]}
                                 {:highlight
                                  [{:tree
                                    [{:italic [{:text ["stuff"]}]}
                                     {:text [" here"]}]}]}]}]}
                       {:text [" "]}
                       {:latex
                        [{:tree
                          [{:text ["really, "]}
                           {:bold [{:text ["lots"]}]}]}]}]}]}
             {:text ["!"]}]}))))

 (deftest invertability-test
   (doseq [[a b c] combination-test-inputs]
     (doseq [[text _tree] (syntax-nesting-test-cases a b c)]
       (is (= text (-> text roam/parse roam/tree->str))))))
