(ns clips.like
(:use rete.core))

(def CLI-RETRACT {})

(def CLI-MODIFY {})

(defn fill-default [slots]
  (cond
    (empty? slots) nil
    (nil? (next slots)) (list (first slots) nil)
    (= (second slots) :default) (concat [(first slots) (first (nnext slots))]
                                        (fill-default (next (nnext slots)) ))
    true (concat [(first slots) nil]
                 (fill-default (next slots)) )))

(defn prefix-keys [prefix map]
  (letfn [(pfx [px key]
               (symbol (str px "-" (name key))))]
    (reduce-kv #(assoc %1 (pfx prefix %2) %3) {} map)))

(defn newvar []
  (symbol (str "?" (name (gensym "v")))))

(defn only-not-nil [mp]
  (reduce #(if (nil? (get mp %2)) (dissoc %1 %2) %1) mp (keys mp)))

(defn merge-defaults [typ sm tm]
  (merge (only-not-nil (tm typ)) sm))

(defn merge-newvars [sm slots]
  (reduce #(assoc %1 %2 (or (sm %2) (newvar))) {} slots))

(defn cli-lhs [rule]
  (take-while #(not (= % '=>)) (nnext rule)))

(defn cli-rhs [rule]
  (rest (drop-while #(not (= % '=>)) (nnext rule))))

(defn find-not-vals [pm]
  (letfn [(newval-test [v]
                       (if (and (seq? v) (= (first v) 'not))
                         (let [newv (newvar)]
                           [newv (list newv '!= (second v))])
                         [v]))]
    (let [vtm (reduce-kv #(assoc %1 %2 (newval-test %3)) {} pm)
          tl (mapcat rest (vals vtm))
          nvm (reduce-kv #(assoc %1 %2 (first %3)) {} vtm)]
      [nvm tl])))

(defn add-to [one two]
  (if (empty? one)
    two
    (recur (rest one)
           (if (some #{(first one)} two)
             two
             (cons (first one) two)) ) ))

(defn patt-will-modify [typ ovar sm vstm]
  (let [mslots (vstm ovar)
        [nvm tsts] (find-not-vals sm)
        sm2 (merge-newvars nvm (add-to mslots (keys nvm)))
        smm (select-keys sm2 mslots)
        pfx (name typ)
        sls (flatten (seq (prefix-keys pfx sm2)))
        slm (flatten (seq (prefix-keys pfx smm)))]
    (def CLI-MODIFY (assoc CLI-MODIFY ovar slm))
    (cons (cons ovar sls) tsts)))

(defn patt-will-retract [typ ovar sm tm]
  (let [pm (prefix-keys (name typ) sm)
        [nvm tsts] (find-not-vals pm)
        sls (flatten (seq nvm))]
    (def CLI-RETRACT (assoc CLI-RETRACT ovar sls))
    (cons (cons ovar sls) tsts)))

(defn patt-will-rest [typ ovar sm]
  (let [pm (prefix-keys (name typ) sm)
        [nvm tsts] (find-not-vals pm)
        sls (flatten (seq nvm))]
    (cons (cons ovar sls) tsts)))

(defn cli-trans-patt [typ tail vtr vstm tm]
  (let [[ovar & svals] tail
        sm (apply hash-map svals)]
    (cond
      (some #{ovar} vtr) (patt-will-retract typ ovar sm tm)
      (some #{ovar} (keys vstm)) (patt-will-modify typ ovar sm vstm)
      true (patt-will-rest typ ovar sm)) ))

(defn cli-trans-not [head tail]
  (let [sm (apply hash-map (rest tail))
        pm (prefix-keys (name head) sm)
        sls (flatten (seq pm))]
    (list 'true 'not (concat ['exist (first tail)] sls)) ))

(defn cli-trans-lhs
  ([lhs vtr vstm tm]
    (mapcat #(cli-trans-lhs (first %) (second %) (rest %) % vtr vstm tm) lhs))
  ([h1 h2 tail whole vtr vstm tm]
    (cond
      (test? h2) [whole]
      (vari? h2) (cli-trans-patt h1 tail vtr vstm tm)
      (= h1 'not) [(cli-trans-not (first h2) (rest h2))])))

(defn vars-to-retract [rule]
  (let [rhs (cli-rhs rule)
        rts (filter #(= (first %) 'retract) rhs)]
    (if (seq rts)
      (mapcat rest rts)) ))

(defn vars-slots-to-modify [rule]
  (letfn [(slots [x]
                 (map first (partition 2 (rest x))))]
    (let [rhs (cli-rhs rule)
          mfs (filter #(= (first %) 'modify) rhs)]
      (if (seq mfs)
        (reduce #(assoc %1 (first %2) (slots %2))
                {}
                (map #(rest (second %)) mfs)) )) ))

(defn cli-trans-ass [ass tm]
  (let [[typ ovar & svals] ass
        sm1 (apply hash-map svals)
        sm2 (merge-defaults typ sm1 tm)
        pm (prefix-keys (name typ) sm2)
        sls (flatten (seq pm))]
    [(cons 'asser (cons (list 'gensym (name typ)) sls))]))

(defn cli-trans-retr [retr]
  (doall (map #(cons 'retract (cons % (CLI-RETRACT %))) retr)))

(defn cli-trans-modi [modi]
  (let [[typ ovar & svals] modi
        cmo (CLI-MODIFY ovar)
        sm (apply hash-map svals)
        pm (prefix-keys (name typ) sm)]
    [(cons 'retract (cons ovar cmo))
     (cons 'asser (cons ovar (flatten (seq pm)) ))]))

(defn cli-trans-rhs
  ([rhs tm]
    (mapcat #(cli-trans-rhs (first %) (rest %) % tm) rhs))
  ([h tail whole tm]
    (cond
      (= h 'assert) (cli-trans-ass (first tail) tm)
      (= h 'retract) (cli-trans-retr tail)
      (= h 'modify) (cli-trans-modi (first tail))
      true [whole])))

(defn cli-trans-rule [rule tm]
  (def CLI-RETRACT {})
  (def CLI-MODIFY {})
  (let [vtr (vars-to-retract rule)
        vstm (vars-slots-to-modify rule)
        lhs (cli-trans-lhs (cli-lhs rule) vtr vstm tm)
        rhs (cli-trans-rhs (cli-rhs rule) tm)]
    (list (first rule) (second rule) lhs '=> rhs)))

(defn display-production [prod]
  (let [[name salience lhs div rhs] prod]
    (print "(")
    (println name)
    (print "  ")
    (println salience)
    (if (= (count lhs) 1)
      (do (print "  (") (print (first lhs)) (println ")"))
      (do (print "  (") (println (first lhs))
        (doseq [e (butlast (rest lhs))]
          (print "   ") (println e))
        (print "   ") (print (last lhs)) (println ")")))
    (print "  ") (println div)
    (if (= (count rhs) 1)
      (do (print "  (") (print (first rhs)) (println ")"))
      (do (print "  (") (println (first rhs))
        (doseq [e (butlast (rest rhs))]
          (print "   ") (println e))
        (print "   ") (print (last rhs)) (print ")")))
    (println ")")))

(defn cli-trans-templs [tls]
  "Translates template descriptions into templates map"
  (reduce #(assoc %1 (first %2) (apply hash-map (fill-default (rest %2)))) {} tls))

(defn cli-trans-facts [facts tm]
  "Translates fact descriptions into list of facts adding defaults"
  (let [fl1 (map #(list (first %) (apply hash-map (nnext %))) facts)
        fl2 (map #(list (first %) (merge-defaults (first %) (second %) tm)) fl1)
        fl3 (map #(list (first %) (prefix-keys (first %) (second %))) fl2)]
    (map #(cons (gensym (name (first %))) (flatten (seq (second %)))) fl3)))

(defn cli-trans-rules [rules tm]
  "Translates rule descriptions into list of productions"
  (map #(cli-trans-rule % tm) rules))

(declare PRODUCTIONS FACTS)

(defn run-with [modes]
  (let [[m1 m2] (seq (.split modes ":"))]
    (if
      (condp = m1
        "trace" (do (trace) true)
        "run" (do (untrace) true)
        (do (println (str "Wrong mode: " m1)) nil))
      (condp = m2
        "synch" (rutime (run-synch PRODUCTIONS FACTS))
        "asynch" (rutime (run-asynch PRODUCTIONS FACTS))
        (println (str "Wrong mode: " m2))) )))

(defn cli-run-file [modes path]
  (let [trf (read-string (slurp path))]
    (if (and (= (ffirst trf) 'templates)
             (= (first (second trf)) 'rules)
             (= (first (nth trf 2)) 'facts))
      (do 
        (def TEMPLATES (cli-trans-templs (rest (first trf)) ))
        (def PRODUCTIONS (cli-trans-rules (rest (second trf)) TEMPLATES))
        (def FACTS (cli-trans-facts (rest (nth trf 2)) TEMPLATES))
        (run-with modes))
      (println (str "Wrong format file: " path)) )))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (condp = (count args)
    3 (do
        (def PRODUCTIONS (read-string (slurp (nth args 1))))
        (def FACTS (read-string (slurp (nth args 2))))
        (run-with (first args)))
    2 (cli-run-file (first args) (second args))
    (println "Number of arguments: 2 or 3, see documentation!")))
