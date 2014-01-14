(ns interpreter.core)

(declare evaluate-with-binding)

(def primitive-functions
  {:plus +
   :times *
   :divide /
   :equals =})

(defn- zip [a b]
  (map vector a b))

(defn evaluate-if-seq [bindings tree functions]
  (if (seq? tree)
    (evaluate-with-binding bindings tree functions)
    (get bindings tree tree)))

(defn evaluate-with-binding [bindings tree functions]
  (let [arguments (rest tree)
        function-name (first tree)
        function-to-use (get
                          (merge primitive-functions functions)
                          function-name)]
    (cond
      (= :if function-name)
      (let [[condition true-form false-form] arguments]
          (if (evaluate-if-seq bindings condition functions)
            (evaluate-if-seq bindings true-form functions)
            (evaluate-if-seq bindings false-form functions)))
      (seq? function-to-use)
      (let [body (last function-to-use)
            interpreted-arguments (map #(if (seq? %)
                                          (evaluate-with-binding bindings % functions)
                                          (get bindings % %))
                                       arguments)
            argument-names (take-while (complement seq?) function-to-use)
            argument-map (into {} (zip argument-names interpreted-arguments))
            new-bindings (merge bindings argument-map)]
        (evaluate-with-binding new-bindings body functions))
      :else
      (let [interpreted-arguments (map #(if (seq? %)
                                          (evaluate-with-binding bindings % functions)
                                          (get bindings % %))
                                       arguments)]
        (apply function-to-use interpreted-arguments)))))

(defn interpret-main [functions]
  (evaluate-with-binding
    {}
    (:main functions)
    functions))
