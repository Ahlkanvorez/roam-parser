(ns roam.parser-test
  (:require [cljs.test :refer-macros [deftest is]]
            [roam.parser :as parser]))

(deftest roam-examples-test
  (is (= {:tree [{:text "The parser can parse all of Roam's major syntax at least "}
                 {:link [{:text "Nested "}
                         {:link [{:text "Links"}]}]}
                 {:text " and "}
                 {:highlight [{:bold [{:text "bold highlights"}]}]}
                 {:text " and "}
                 {:syntax-quote [{:alias {:left [{:text "html roam"}]
                                          :right [{:link [{:text "Aliases"}]}]}}]}
                 {:text "all the ones we haven't done yet as well"}]}
         (parser/parse "The parser can parse all of Roam's major syntax at least [[Nested [[Links]]]] and ^^**bold highlights**^^ and `[html roam]([[Aliases]])`all the ones we haven't done yet as well")))
  (is (= {:alias {:left [{:text "!"}
                         {:alias {:left [{:text "img"}]
                                  :right [{:text "image-as-alias.com"}]}}]
                  :right [{:text "www.roamresearch.com"}]}}
         (parser/parse "[![img](image-as-alias.com)](www.roamresearch.com)")))
  (is (= {:roam-render [{:roam-render [{:text "curly braces"}]}
                        {:text " in"}
                        {:roam-render [{:text "side"}]}
                        {:text " of "}
                        {:roam-render [{:text "curly braces"}]}]}
         (parser/parse "{{{{curly braces}} in{{side}} of {{curly braces}}}}")))
  (is (= {:tree [{:text "Specifically "}
                 {:block-quote [{:text "javascript\n\nAliases inside aliases\n>   \n  "}
                                {:alias {:left [{:text "!"}
                                                {:alias {:left [{:text "img"}]
                                                         :right [{:text "image-as-alias.com"}]}}]
                                         :right [{:text "www.roamresearch.com"}]}}
                                {:text "\n\n"}
                                {:roam-render [{:roam-render [{:text "curly braces"}]}
                                               {:text " in"}
                                               {:roam-render [{:text "side"}]}
                                               {:text " of "}
                                               {:roam-render [{:text "curly braces"}]}]}
                                {:text "\n\n\n"}]}]}
         (parser/parse "Specifically ```javascript

Aliases inside aliases
>   
  [![img](image-as-alias.com)](www.roamresearch.com)

{{{{curly braces}} in{{side}} of {{curly braces}}}}


```"))))

(deftest raw-text-test
  (is (= {:text ""}
         (parser/parse "")))
  (is (= {:text " "}
         (parser/parse " ")))
  (is (= {:text "abc"}
         (parser/parse "abc")))
  (is (= {:text "123"}
         (parser/parse "123")))
  (is (= {:text "abc-123"}
         (parser/parse "abc-123")))
  (is (= {:text "ab\\\"c12\\\"3"}
         (parser/parse "ab\\\"c12\\\"3"))))

(defn double-bracket-test-cases [open close kind]
  {(str open "hello" close)
   {kind [{:text "hello"}]}

   (str "abc " open "hello" close " 123")
   {:tree [{:text "abc "}
           {kind [{:text "hello"}]}
           {:text " 123"}]}})

(defn nested-double-bracket-test-cases [open close kind]
  (merge
   (double-bracket-test-cases open close kind)
   {(str open open "hello" close " roam world" close)
    {kind [{kind [{:text "hello"}]}
           {:text " roam world"}]}

    (str open "hello " open "roam" close " world" close)
    {kind [{:text "hello "}
           {kind [{:text "roam"}]}
           {:text " world"}]}

    (str open "hello roam " open "world" close close)
    {kind [{:text "hello roam "}
           {kind [{:text "world"}]}]}

    (str open open open "three" close " two" close " one" close)
    {kind [{kind [{kind [{:text "three"}]}
                  {:text " two"}]}
           {:text " one"}]}

    (str open open "two " open "three" close close " one" close)
    {kind [{kind [{:text "two "}
                  {kind [{:text "three"}]}]}
           {:text " one"}]}

    (str open "one " open "two " open "three" close close close)
    {kind [{:text "one "}
           {kind [{:text "two "}
                  {kind [{:text "three"}]}]}]}

    (str "Nesting! " open "one " open "two " open "three" close close close)
    {:tree [{:text "Nesting! "}
            {kind [{:text "one "}
                   {kind [{:text "two "}
                          {kind [{:text "three"}]}]}]}]}}))

(deftest syntax-link-test
  (doseq [[text tree] (nested-double-bracket-test-cases "[[" "]]" :link)]
    (is (= tree (parser/parse text)))))

(deftest syntax-ref-test
  (doseq [[text tree] (nested-double-bracket-test-cases "((" "))" :ref)]
    (is (= tree (parser/parse text)))))

(deftest syntax-roam-render-test
  (doseq [[text tree] (nested-double-bracket-test-cases "{{" "}}" :roam-render)]
    (is (= tree (parser/parse text)))))

(deftest syntax-latex-test
  (doseq [[text tree] (double-bracket-test-cases "$$" "$$" :latex)]
    (is (= tree (parser/parse text)))))

(deftest syntax-highlight-test
  (doseq [[text tree] (double-bracket-test-cases "^^" "^^" :highlight)]
    (is (= tree (parser/parse text)))))

(deftest syntax-bold-test
  (doseq [[text tree] (double-bracket-test-cases "**" "**" :bold)]
    (is (= tree (parser/parse text)))))

(deftest syntax-italic-test
  (doseq [[text tree] (double-bracket-test-cases "__" "__" :italic)]
    (is (= tree (parser/parse text)))))

(defn syntax-nesting-test-cases [group1 group2 group3]
  (let [[open1 close1 kind1] group1
        [open2 close2 kind2] group2
        [open3 close3 kind3] group3]
    {(str open1 open2 "hello" close2 " roam world" close1)
     {kind1 [{kind2 [{:text "hello"}]}
             {:text " roam world"}]}

     (str open1 "hello " open2 "roam" close2 " world" close1)
     {kind1 [{:text "hello "}
             {kind2 [{:text "roam"}]}
             {:text " world"}]}

     (str open1 "hello roam " open2 "world" close2 close1)
     {kind1 [{:text "hello roam "}
             {kind2 [{:text "world"}]}]}

     (str open1 open2 open3 "three" close3 " two" close2 " one" close1)
     {kind1 [{kind2 [{kind3 [{:text "three"}]}
                     {:text " two"}]}
             {:text " one"}]}

     (str open1 open2 "two " open3 "three" close3 close2 " one" close1)
     {kind1 [{kind2 [{:text "two "}
                     {kind3 [{:text "three"}]}]}
             {:text " one"}]}

     (str open1 "one " open2 "two " open3 "three" close3 close2 close1)
     {kind1 [{:text "one "}
             {kind2 [{:text "two "}
                     {kind3 [{:text "three"}]}]}]}

     (str "Nesting! " open1 "one " open2 "two " open3 "three" close3 close2 close1)
     {:tree [{:text "Nesting! "}
             {kind1 [{:text "one "}
                     {kind2 [{:text "two "}
                             {kind3 [{:text "three"}]}]}]}]}}))

(deftest syntax-code-test
  (is (= {:code [{:text "hello"}]}
         (parser/parse "{hello}")))
  (is (= {:tree [{:text "This is text before the code "}
                 {:code [{:text "2 + 2"}]}
                 {:text " This is text after the code"}]}
         (parser/parse "This is text before the code {2 + 2} This is text after the code")))
  (is (= {:code [{:text "Does it make sense to nest code? "}
                 {:code [{:text "Not really. "}]}
                 {:code [{:text "But we can anyway "}
                         {:code [{:text "2 * 2 * 2"}]}]}]}
         (parser/parse "{Does it make sense to nest code? {Not really. }{But we can anyway {2 * 2 * 2}}}"))))

(def combination-test-inputs
  (let [latex ["$$" "$$" :latex]
        highlight ["^^" "^^" :highlight]
        bold ["**" "**" :bold]
        italic ["__" "__" :italic]
        link ["[[" "]]" :link]
        ref ["((" "))" :ref]
        roam-render ["{{" "}}" :roam-render]
        syntax-quote ["`" "`" :syntax-quote]
        block-quote ["```" "```" :block-quote]
        quote ["\"" "\"" :quote]
        groups [latex highlight bold italic link ref roam-render quote]
        ;; It doesn't make sense to nest syntax-quotes & block-quotes together,
        ;; so split test cases into two groups, one with each & not the other.
        groups-a (conj groups syntax-quote)
        groups-b (conj groups block-quote)]
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
      (is (= tree (parser/parse text))))))

(deftest all-together-test
  (is (= {:tree [{:text "hello "}
                 {:link [{:text "world "}
                         {:ref [{:text "lots "}
                                {:roam-render [{:text "of "}
                                               {:bold [{:text "nested"}]}]}
                                {:text " "}
                                {:highlight [{:italic [{:text "stuff"}]}
                                             {:text " here"}]}]}
                         {:text " "}
                         {:latex [{:text "really, "}
                                  {:bold [{:text "lots"}]}]}]}
                 {:text "!"}]}
         (parser/parse "hello [[world ((lots {{of **nested**}} ^^__stuff__ here^^)) $$really, **lots**$$]]!"))))

(deftest syntax-alias-test
  (is (= {:alias {:left [{:text "hello"}]
                  :right [{:text "roam"}]}}
         (parser/parse "[hello](roam)")))

  (is (= {:alias {:left [{:text "hello "}
                         {:alias {:left [{:text "roam"}]
                                  :right [{:text "world"}]}}]
                  :right [{:text "cheese"}]}}
         (parser/parse "[hello [roam](world)](cheese)")))

  (is (= {:alias {:left [{:text "Oh, "}
                         {:alias {:left [{:text "hello, "}]
                                  :right [{:text "world"}]}}
                         {:text " "}]
                  :right [{:text "the-end"}]}}
         (parser/parse "[Oh, [hello, ](world) ](the-end)")))

  (is (= {:alias {:left [{:alias {:left [{:text "hello"}]
                                  :right [{:text "there"}]}}]
                  :right [{:text "aliases"}]}}
         (parser/parse "[[hello](there)](aliases)")))

  (is (= {:alias {:left [{:text "Hi"}]
                  :right [{:alias {:left [{:text "its an"}]
                                   :right [{:text "alias"}]}}]}}
         (parser/parse "[Hi]([its an](alias))")))

  (is (= {:alias {:left [{:alias {:left [{:text "Not a"}]
                                  :right [{:text "link"}]}}]
                  :right [{:text "its "}
                          {:alias {:left [{:text "an"}]
                                   :right [{:text "alias"}]}}]}}
         (parser/parse "[[Not a](link)](its [an](alias))"))))

(deftest syntax-blockquote-test
  (is (= {:block-quote [{:text "abc "} {:link [{:text "123"}]} {:text " def"}]}
         (parser/parse "```abc [[123]] def```")))

  (is (= {:tree [{:text "x"}
                 {:block-quote [{:text "y"}]}
                 {:text "z"}]}
         (parser/parse "x```y```z")))

  (is (= {:tree [{:text "c d"}
                 {:block-quote [{:text "1 "}
                                {:syntax-quote [{:link [{:text "123"}]}]}
                                {:text " 3"}]}
                 {:text "d e"}]}
         (parser/parse "c d```1 `[[123]]` 3```d e"))))

(deftest syntax-escapes-test
  (is (= {:link [{:text "hello "}
                 {:quote [{:text "quoted \\\"and \\`escaped\\`\\\" world"}]}
                 {:text " with links"}]}
         (parser/parse "[[hello \"quoted \\\"and \\`escaped\\`\\\" world\" with links]]")))
  (is (= {:block-quote [{:text "Syntax quotes have "}
                        {:bold [{:text "\\`"}]}
                        {:text " before and after them "}
                        {:bold [{:text "\\`"}]}
                        {:text " like that"}]}
         (parser/parse "```Syntax quotes have **\\`** before and after them **\\`** like that```"))))

(deftest tree->str-test
  (is (= "abc"
         (parser/tree->str {:text "abc"})))
  (is (= "[hello]"
         (parser/tree->str {:text "[hello]"})))
  (is (= "[[hello links]]"
         (parser/tree->str {:link [{:text "hello links"}]})))
  (is (= "[[hello [[nested [[links]]]]]]"
         (parser/tree->str {:link [{:text "hello "}
                                 {:link [{:text "nested "}
                                         {:link [{:text "links"}]}]}]})))
  (is (= "this is [[probably [[[[enough]] linking]] for]] now."
         (parser/tree->str
          {:tree [{:text "this is "}
                  {:link [{:text "probably "}
                          {:link [{:link [{:text "enough"}]}
                                  {:text " linking"}]}
                          {:text " for"}]}
                  {:text " now."}]})))
  (is (= "hello [[world ((lots {{of **nested**}} ^^__stuff__ here^^)) $$really, **lots**$$]]!"
         (parser/tree->str
          {:tree [{:text "hello "}
                  {:link [{:text "world "}
                          {:ref [{:text "lots "}
                                 {:roam-render [{:text "of "}
                                                {:bold [{:text "nested"}]}]}
                                 {:text " "}
                                 {:highlight [{:italic [{:text "stuff"}]}
                                              {:text " here"}]}]}
                          {:text " "}
                          {:latex [{:text "really, "}
                                   {:bold [{:text "lots"}]}]}]}
                  {:text "!"}]}))))

(def idempotency-test-cases
  ["this is [[probably [[[[enough]] linking]] for]] now."
   "hello [[world ((lots {{of **nested**}} ^^__stuff__ here^^)) $$really, **lots**$$]]!"])

(deftest invertability-test
  (doseq [[a b c] combination-test-inputs]
    (doseq [[text _tree] (syntax-nesting-test-cases a b c)]
      (is (= text (-> text parser/str->tree parser/tree->str))))))
