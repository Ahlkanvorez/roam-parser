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

(deftest syntax-link
  (is (= {:link [{:text "hello"}]}
         (core/parse "[[hello]]")))
  (is (= {:tree [{:text "abc "}
                 {:link [{:text "hello"}]}
                 {:text " 123"}]}
         (core/parse "abc [[hello]] 123"))))

(deftest syntax-link-recursive
  (is (= {:link [{:text "hello roam "}
                 {:link [{:text "world"}]}]}
         (core/parse "[[hello roam [[world]]]]")))
  (is (= {:link [{:text "hello "}
                 {:link [{:text "roam"}]}
                 {:text " world"}]}
         (core/parse "[[hello [[roam]] world]]")))
  (is (= {:link [{:link [{:text "hello"}]}
                 {:text " roam world"}]}
         (core/parse "[[[[hello]] roam world]]")))
  (is (= {:link [{:link [{:link [{:text "three"}]}
                         {:text " two"}]}
                 {:text " one"}]}
         (core/parse "[[[[[[three]] two]] one]]")))
  (is (= {:link [{:link [{:text "two "}
                         {:link [{:text "three"}]}]}
                 {:text " one"}]}
         (core/parse "[[[[two [[three]]]] one]]")))
  (is (= {:link [{:text "one "}
                 {:link [{:text "two "}
                         {:link [{:text "three"}]}]}]}
         (core/parse "[[one [[two [[three]]]]]]")))
  (is (= {:tree [{:text "Nesting! "}
                 {:link [{:text "one "}
                         {:link [{:text "two "}
                                 {:link [{:text "three"}]}]}]}]}
         (core/parse "Nesting! [[one [[two [[three]]]]]]"))))

(deftest syntax-ref
  (is (= :todo :todo)))

(deftest syntax-roam-render
  (is (= :todo :todo)))

(deftest syntax-latex
  (is (= :todo :todo)))

(deftest syntax-alias
  (is (= :todo :todo)))

(deftest syntax-highlight
  (is (= :todo :todo)))

(deftest syntax-bold
  (is (= :todo :todo)))

(deftest syntax-italic
  (is (= :todo :todo)))

(deftest syntax-recursive
  (is (= :todo :todo)))

(deftest tree-abstraction
  (is (= :todo :todo)))

(deftest parse-speed
  ;; Must parse 1000 strings in under 500ms
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
