(ns roam.parser.tree-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as string]
            [roam.parser.tree :as tree]
            [roam.parser.lexical :as lexical]
            [roam.parser.syntax :as syntax]))

(deftest map-leaves-test
  (is (= (tree/Node.
          :tree
          [(tree/Node.
            :bracket
            [(tree/Node. :bracket
                            [(tree/Node. :text ["A"])])
             (tree/Node. :paren
                            [(tree/Node. :text ["B"])])])
           (tree/Node.
            :paren
            [(tree/Node. :paren
                            [(tree/Node. :text ["C"])])])])
         (tree/map-leaves
          (fn [path s] (string/upper-case s))
          (lexical/parse "[[a](b)]((c))"))))

  (is (= (tree/Node.
          :tree
          [(tree/Node.
            :bracket
            [(tree/Node. :bracket
                            [(tree/Node. :text ["a"])])
             (tree/Node. :paren
                            [(tree/Node. :text ["B"])])])
           (tree/Node.
            :paren
            [(tree/Node. :paren
                            [(tree/Node. :text ["c"])])])])
         (tree/map-leaves
          (fn [path s]
            (if (= s "b")
              (clojure.string/upper-case s)
              s))
          (lexical/parse "[[a](b)]((c))"))))

  (is (= (tree/Node.
          :tree
          [(tree/Node.
            :bracket
            [(tree/Node. :bracket
                            [(tree/Node. :text ["A"])])
             (tree/Node. :paren
                            [(tree/Node. :text ["b"])])])
           (tree/Node.
            :paren
            [(tree/Node. :paren
                            [(tree/Node. :text ["c"])])])])
         (tree/map-leaves
          (fn [path s]
            (if (= path [:tree 0 :bracket 0 :bracket 0 :text 0])
              (clojure.string/upper-case s)
              s))
          (lexical/parse "[[a](b)]((c))")))))

(deftest map-tree-test
  (is (= (tree/Node. :tree [])
         (tree/map-tree (fn [_p n] n) (tree/Node. :tree []))))

  (is (= (tree/Node.
          :tree
          [(tree/Node.
            :paren
            [(tree/Node. :paren
                            [(tree/Node. :text ["a"])])
             (tree/Node. :bracket
                            [(tree/Node. :text ["b"])])])
           (tree/Node.
            :bracket
            [(tree/Node. :bracket
                            [(tree/Node. :text ["c"])])])])
         (tree/map-tree
          (fn [path node]
            (if (instance? tree/Node node)
              (let [original-kind (.-kind node)
                    new-kind (case original-kind
                               :bracket :paren
                               :paren :bracket
                               original-kind)]
                (if (not= new-kind original-kind)
                  (tree/Node. new-kind (.-children node))
                  node))
              node))
          (lexical/parse "[[a](b)]((c))"))))

  (is
   (= [:div
       [:div.link
        [:div.ref "a"]
        "b"]
       [:a {:href "d"} "c"]
       [:div.render "e"]]
      (->> "[[((a))b]][c](d){{e}}" lexical/parse syntax/analyze
           (tree/map-tree
            (fn [_path node]
              (cond (string? node) node
                    (vector? node) node
                    :default
                    (case (.-kind node)
                      :text (nth node 0)
                      :alias [:a {:href (get-in node [1 0 0])}
                              (get-in node [0 0])]
                      (apply vector (case (.-kind node)
                                      :tree :div
                                      :link :div.link
                                      :ref :div.ref
                                      :render :div.render)
                             (rest node))))))))))

(deftest update-test
  (is (= (tree/Node.
          :tree
          [(tree/Node. :text ["abc"])
           (tree/Node. :link [(tree/Node. :text ["Cheddar"])])
           (tree/Node. :ref [(tree/Node. :text ["dogs"])])])
         (letfn [(left [n] (first (first (rest n))))
                 (right [n] (first (second (rest n))))]
           (update
            (update
             (update
              (syntax/analyze
               (lexical/parse
                "[abc](123)[[[Cheddar]]](((Cheese)))[{{hot}}](((dogs)))"))
              0 left)
             1 left)
            2 right))))

  (is
   (= [:div
       [:h1 "a"]
       [:p "b"]
       [:a {:href "d"} "c"]
       [:pre "e"]]
      (-> "((a))b[c](d){{e}}" lexical/parse syntax/analyze
          (update :ref (fn [link] [:h1 (get-in link [:text 0])]))
          (update :text (fn [txt] [:p (nth txt 0)]))
          (update :alias (fn [alias]
                           [:a {:href (get-in alias [1 0 0])}
                            (get-in alias [0 0 0])]))
          (update :render (fn [render] [:pre (get-in render [:text 0])]))
          rest
          (as-> div (apply vector :div div))))))

(deftest update-in-test
  (is (= (tree/Node.
          :tree
          [(tree/Node.
            :alias
            [[(tree/Node. :text ["abc"])]
             [(tree/Node. :text ["123"])]])
           (tree/Node.
            :alias
            [[(tree/Node. :link [(tree/Node. :text ["Cheddar"])])]
             [(tree/Node. :ref [(tree/Node. :text ["CHEESE"])])]])
           (tree/Node.
            :alias
            [[(tree/Node. :render [(tree/Node. :text ["hot"])])]
             [(tree/Node. :ref [(tree/Node. :text ["dogs"])])]])])
         (update-in
          (syntax/analyze
           (lexical/parse
            "[abc](123)[[[Cheddar]]](((Cheese)))[{{hot}}](((dogs)))"))
          [1 1 0 0 0]
          string/upper-case)))

  (is (= (tree/Node.
          :tree
          [(tree/Node.
            :alias
            [[(tree/Node. :text ["abc"])]
             [(tree/Node. :text ["123"])]])
           (tree/Node.
            :alias
            [[(tree/Node. :link [(tree/Node. :text ["Havarti"])])]
             [(tree/Node. :ref [(tree/Node. :text ["Cheese"])])]])
           (tree/Node.
            :alias
            [[(tree/Node. :render [(tree/Node. :text ["hot"])])]
             [(tree/Node. :ref [(tree/Node. :text ["dogs"])])]])])
         (update-in
          (syntax/analyze
           (lexical/parse
            "[abc](123)[[[Cheddar]]](((Cheese)))[{{hot}}](((dogs)))"))
          [1 0 0 0 0]
          (fn [s] (if (= s "Cheddar") "Havarti" s)))))

  (is (= (tree/Node.
          :tree
          [(tree/Node. :ref [(tree/Node. :text ["a"])])
           (tree/Node. :link [(tree/Node. :text ["B"])])
           (tree/Node. :render [(tree/Node. :text ["c"])])])
         (update-in
          (tree/Node.
           :tree
           [(tree/Node. :ref [(tree/Node. :text ["a"])])
            (tree/Node. :link [(tree/Node. :text ["b"])])
            (tree/Node. :render [(tree/Node. :text ["c"])])])
          [:link :text 0]
          string/upper-case))))
