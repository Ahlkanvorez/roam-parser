(ns roam.core-test
  (:require [clojure.test :refer :all]
            [roam.core :as core]))

(deftest raw-text
  (is (= {:text "abc"}
         (core/parse "abc")))
  (is (= {:text "123"}
         (core/parse "123")))
  (is (= {:text "abc-123"}
         (core/parse "abc-123"))))

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

(deftest syntax-link
  (doseq [[text tree] (nested-double-bracket-test-cases "[[" "]]" :link)]
    (is (= tree (core/parse text)))))

(deftest syntax-ref
  (doseq [[text tree] (nested-double-bracket-test-cases "((" "))" :ref)]
    (is (= tree (core/parse text)))))

(deftest syntax-roam-render
  (doseq [[text tree] (nested-double-bracket-test-cases "{{" "}}" :roam-render)]
    (is (= tree (core/parse text)))))

(deftest syntax-latex
  (doseq [[text tree] (double-bracket-test-cases "$$" "$$" :latex)]
    (is (= tree (core/parse text)))))

(deftest syntax-highlight
  (doseq [[text tree] (double-bracket-test-cases "^^" "^^" :highlight)]
    (is (= tree (core/parse text)))))

(deftest syntax-bold
  (doseq [[text tree] (double-bracket-test-cases "**" "**" :bold)]
    (is (= tree (core/parse text)))))

(deftest syntax-italic
  (doseq [[text tree] (double-bracket-test-cases "__" "__" :italic)]
    (is (= tree (core/parse text)))))

(deftest syntax-alias
  (is (= :todo :todo)))

(deftest tree->str
  (is (= "abc"
         (core/tree->str {:text "abc"})))
  (is (= "[hello]"
         (core/tree->str {:text "[hello]"})))
  (is (= "[[hello links]]"
         (core/tree->str {:link [{:text "hello links"}]})))
  (is (= "[[hello [[nested [[links]]]]]]"
         (core/tree->str {:link [{:text "hello "}
                                 {:link [{:text "nested "}
                                         {:link [{:text "links"}]}]}]})))
  (is (= "this is [[probably [[[[enough]] linking]] for]] now."
         (core/tree->str
          {:tree [{:text "this is "}
                  {:link [{:text "probably "}
                          {:link [{:link [{:text "enough"}]}
                                  {:text " linking"}]}
                          {:text " for"}]}
                  {:text " now."}]}))))
