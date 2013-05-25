(ns rete.core
  (:import java.util.HashMap)
  (:gen-class))

(defn throw-mess [mess]
  "Throw message"
  (Throwable. (str mess)))

(defn obj [trip]
  "Get object of triple"
  (nth trip 2))

(defn pred [trip]
  "Get predicate of triple"
  (second trip))

(defn subj [trip]
  "Get subject of triple"
  (first trip))

(defn vari? [x]
  (and (symbol? x) (.startsWith (name x) "?")))

(defn func? [sym]
  (if (symbol? sym)
    (if-let [res (ns-resolve 'rete.core sym)] res)))

(defn prod-name [prod]
  "Get name of production"
  (first prod))

(defn salience [prod]
  "Get salience of production"
  (second prod))

(defn lhs [prod]
  "Get left hand side of production"
  (nth prod 2))

(defn rhs [prod]
  "Get right hand side of production"
  (nth prod 4))

(defn acnt 
  "Alpha memory count calculated during creation of the alpha net and used for creation of the alpha memory"
  []
  (def =ACNT= (inc =ACNT=))
  (dec =ACNT=))

(defn fcnt []
  "Fact memory count"
  (def =FCNT= (inc =FCNT=))
  (dec =FCNT=))

(defn mk-object-hm [obj hm fn]
  "Make object HashMap"
  (let [key (if (vari? obj) '? obj)]
    (if (nil? hm)                                        
      (let [hm (HashMap.)]
         (.put hm key (fn))
         hm)
      (if (.get hm key)
           hm
           (do
              (.put hm key (fn))
              hm)) )))

(defn mk-predicate-hm [pred-obj hm fn]
  "Make predicate HashMap"
  (let [[pred obj] pred-obj
        key (if (vari? pred) '? pred)]
    (if (nil? hm)
      (let [hm (HashMap.)]
        (.put hm key (mk-object-hm obj nil fn))
        hm)
      (do
        (.put hm key (mk-object-hm obj (.get hm key) fn))
        hm))))

(defn mk-subject-hm [trip hm fn]
  "Make subject HashMap"
  (let [[subj & pred-obj] trip
        key (if (vari? subj) '? subj)]
    (if (nil? hm)
      (let [hm (HashMap.)]
        (.put hm key (mk-predicate-hm pred-obj nil fn))
        hm)
      (do
        (.put hm key (mk-predicate-hm pred-obj (.get hm key) fn))
        hm))))

(defn add-anet-entry
  "If <trip> in a left hand side of a rule is a pattern (not test with a function on the predicate place)
   adds a new entry to the hash map <hm> representing the beta net"
  [trip hm]
  ;;(println [:ADD-ANET-ENTRY :TRIP trip :HM hm])
  (if (func? (pred trip))
    hm
    (mk-subject-hm trip hm acnt)))

(defn trans-lhs
  "Translate left hand side"
  ([lhs]
    (mapcat #(let [[ob & rs] %] (trans-lhs % ob rs)) lhs))
  ([trip obj props]
    ;;(println [:TRANS-LHS trip obj props])
    (if (> (count props) 2)
      (map #(cons obj %) (partition 2 props))
      (list trip))))

(defn anet-for-pset
  "Build the alpha net for the given production set (rule set) <pset> as a hash map"
  [pset]
  (loop [pp pset anet nil]
    (if (empty? pp)
      anet
      (recur (rest pp)
             (loop [trips (trans-lhs (lhs (first pp))) ant anet]
               (if (empty? trips)
                 ant
                 (recur (rest trips) (add-anet-entry (first trips) ant)) ))) )))


(defn mk-fact-obj-hm [obj hm fn]
  "Make fact object HashMap"
  (if (nil? hm)
    (let [hm (HashMap.)]
      (.put hm obj (fn))
      hm)
    (if (.get hm obj)
      hm
      (do
        (.put hm obj (fn))
        hm))))

(defn mk-fact-subj-hm [subj obj hm fn]
  "Make fact subject HashMap"
  (if (nil? hm)
    (let [hm (HashMap.)]
      (.put hm subj (mk-fact-obj-hm obj nil fn))
      hm)
    (do
      (.put hm subj (mk-fact-obj-hm obj (.get hm subj) fn))
      hm)))

(defn mk-fact-pred-hm [trip hm fn]
  "Make fact predicate HashMap"
  (let [[subj pred obj] trip]
    (if (nil? hm)
      (let [hm (HashMap.)]
        (.put hm pred (mk-fact-subj-hm subj obj nil fn))
        hm)
      (do
        (.put hm pred (mk-fact-subj-hm subj obj (.get hm pred) fn))
        hm))))

(defn mk-fact [trip]
  "Make fact"
  (let [f-cnt =FCNT=]
    (def =FMEM= (mk-fact-pred-hm trip =FMEM= fcnt))
    (if (> =FCNT= f-cnt)
      (concat trip (list (dec =FCNT=)) ) )))

(defn a-indexof-pattern 
  "Find an alpha memory cell index for a triplet pattern from a left hand side of some rule"
  [trip anet]
  ;;(println [:A-INDEXOF-PATTERN :TRIP trip :ANET anet])
  (let [[subj pred obj] (map #(if (vari? %) '? %) trip)]
    (if-let [shm  (.get anet subj)]
      (if-let [phm  (.get shm pred)]
        (.get phm obj)) )))

(defn test? [pf]
  "Is atom test function?"
  (condp = pf
    '= true
    '!= true
    '> true
    '< true
    '>= true
    '<= true
    'and true
    'or true
    'not true
    false))

(defn mk-pattern-or-test [trip]
  "Make pattern or test"
  (letfn [(reso [x]
                (if (seq? x)
                  (map reso x)
                  (or (func? x) x)))]
    (let [[s p o] trip]
      (if (test? p)
        (cons 'f (list (reso s)
                       p
                       (reso o)))
        (cons 'a trip)) )))

(defn trans-expr [ex]
  "Translate expression"
  ;;(println [:TRANS-EXPR ex ])
  (letfn [(tr-list [car cdr]
                   (cons (or (func? car) car)
                         (map trans-expr cdr)))]
    (cond
      (number? ex) ex
      (symbol? ex) ex
      (string? ex) ex
      (keyword? ex) ex
      (list? ex) (if (list? (first ex))
                   (map trans-expr ex)
                   (tr-list (first ex) (rest ex)))
      (vector? ex) (apply vector (map trans-expr ex)))))

(defn enl [lst]
  (map cons (range (count lst)) lst))

(defn beta-net-plan
  "Create a plan of the beta net that will be used to its building.
   The plan describes the beta net as a list, mean while the real beta net is an array.
   The plan is the list of lists each of which represents one cell of the beta memory.
   First element of each list is an index of the beta memory, rest of each list is a content of the corresponding cell"
  ([pset anet]
    (enl (mapcat 
           #(beta-net-plan 
              (prod-name %)
              (salience %)
              (trans-lhs (lhs %))
              (rhs %) anet) pset)))
  ([pname sal lhs rhs anet]
    (let [aips (map #(a-indexof-pattern % anet) lhs)
          pots (map mk-pattern-or-test lhs)
          all (map #(list %1 %2) aips pots)
          fir (concat (first all) [pname])
          mid (butlast (rest all))
          las (concat (last all) (list pname sal (trans-expr rhs)))]
      (if (= (count lhs) 1)
        (list (cons 'ex las))
        (concat (list (cons 'e fir))
                (map #(cons 'i %) mid)
                (list (cons 'x las)) )) )))

(defn fill-ablink
  "Fill alpha-beta links table from beta net plan"
  ([ablink bplan]
    (dotimes [i (count ablink)]
      (fill-ablink ablink bplan i)))
  ([ablink bplan i]
    (let [flt (filter #(= (nth % 2) i) bplan)]
      (aset ablink i (map first flt)) )))

(defn fill-bnet [bnet bplan]
  "Fill beta net from beta net plan"
  (doseq [[i & v] bplan]
    (aset bnet i v)))

(def reset nil)

(defn create-rete [pset]
  "Create RETE from a production set and reset"
  (try
    (def =ACNT= 0)
    (def =ANET= (anet-for-pset pset))
    (def =BPLAN= (beta-net-plan pset =ANET=))
    (def =ABLINK= (object-array =ACNT=))
    (def =BCNT= (count =BPLAN=))
    (def =BNET= (object-array =BCNT=))
    (fill-bnet =BNET= =BPLAN=)
    (fill-ablink =ABLINK= =BPLAN=)
    (reset)
    [=ACNT= =BCNT=]
    (catch Throwable twe
      (println twe)
      nil)))

(defn reset []
  "Reset: clear and initialize all memories"
  (def =AMEM= (object-array =ACNT=))
  (def =BMEM= (object-array =BCNT=))
  (def =FMEM= (HashMap.))
  (def =FCNT= 0)
  (def =CFSET= nil)
  (def =FIDS= (HashMap.)))

(defn log-lst [tit x]
  "Log list"
  (let [log (with-out-str (doall (map println x)))]
    log))

(defn log-hm [tit x]
  "Log HashMap"
  (let [log (with-out-str
              (doseq [[k v] (into {} x)]
                (println (str k ":"))
                (doseq [[k2 v2] v]
                  (println (str " " k2 ": " v2)) ) ))]
    log))

(defn log-array [tit a]
  "Log array"
  (let [log (with-out-str
              (dotimes [i (count a)]
                (println (str i " " (seq (aget a i)) )) ))]
    log))

(defn log-rete []
  "Log RETE"
  (str
    (log-hm "Alpha Net" =ANET=)
    (log-lst "Beta Net Plan" =BPLAN=)
    (log-array "Apha-Beta Links" =ABLINK=)))

(defn amem
  "Return an alpha memory cell for a given index <i>"
  [i]
  (aget =AMEM= i))

(defn set-amem [i v]
  "Set an alpha memory cell with a value <v> for a given index <i>"
  (aset =AMEM= i v))

(defn a-indices [fact]
  "For a fact find all matching alpha memory cells"
  (let [[s p o & r] fact
        anet =ANET=]
    (filter number?  [(a-indexof-pattern ['? '? '?] anet)
                      (a-indexof-pattern ['? '?  o] anet)
                      (a-indexof-pattern ['?  p '?] anet)
                      (a-indexof-pattern ['?  p  o] anet)
                      (a-indexof-pattern [ s '? '?] anet)
                      (a-indexof-pattern [ s '?  o] anet)
                      (a-indexof-pattern [ s  p '?] anet)
                      (a-indexof-pattern [ s  p  o] anet)])))

(defn bnet
  "Returns a beta net cell (beta node) for a given index <i>"
  [i]
  (aget =BNET= i))

(defn bmem
  "Returns a beta memory cell for a given index <i>"
  [i]
  (aget =BMEM= i))

(defn set-bmem [i v]
  "Set a beta memory cell with a value <v> for a given index <i>"
  (aset =BMEM= i v))

(defn vam []
  "Display alpha memory"
  (let [am =AMEM=]
    (dotimes [i (count am)]
      (println (str i " " (seq (aget am i)) )) )))

(defn eval-mp [mp expr]
  "Evaluate expression with respect to variable-value map"
  ;;(println [:EVAL-MP mp expr])
  (if (seq? expr)
    (apply (first expr) (map #(eval-mp mp %) (rest expr)))
    (or (mp expr) expr)))

(defn match-ctx [fact pattern ctx bi]
  "Match fact with pattern with respect to context"
  ;;(println [:MATCH-CTX :FACT fact :PATTERN pattern :CTX ctx :BI bi])
  (letfn [(uni [p f ctx]
               (if (vari? p)
                 (if-let [v (ctx p)]
                   (if (= f v)
                     ctx)
                   (assoc ctx p f))
                 (if (= p f)
                   ctx)))]
    (let [[sf pf of fid] fact
          [sp pp op] pattern
          ctx3 (if-let [ctx1 (uni pp pf ctx)]
                 (if-let [ctx2 (uni sp sf ctx1)]
                   (uni op of ctx2)))]
      (if ctx3
        (let [ctx4 (assoc ctx3 :FIDS (cons fid (:FIDS ctx3)))
              fids (.get =FIDS= fid)]
          (if (not (some #{bi} fids))
            (.put =FIDS= fid (cons bi fids)))
          ctx4)) )))

(defn match-ctx-list [facts pattern ctx bi]
  "Match list of facts with pattern with respect to context"
  (map #(match-ctx % pattern ctx bi) facts))

(defn vbm []
  "Display beta memory"
  (let [am =BMEM=]
    (dotimes [i (count am)]
      (println (str i " " (seq (aget am i)) )) )))

(defn fact-id [fact]
  "Get id of fact"
  ;;(println [:FID fact])
  (nth fact 3))

(defn add-to-confset
  "Make from an activated production (rule) <aprod> and a list of contexts,
   that have activated this production <match-list>,
   a list of activations and concatenate them to the conflict set"
  [aprod match-list]
  ;;(println [:ADD-TO-CONFSET :APROD aprod :MATCH-LIST match-list])
  (let [alist (map #(list aprod %) match-list)]
    (def =CFSET= (concat alist =CFSET=))))

(defn eval-test [pf sv ov]
  "Evaluate test function <predicate-function> on <subject-value> and <object-value>"
  ;;(println [:TEST pf sv ov])
  (condp = pf
    '= (= sv ov)
    '!= (not (= sv ov))
    '> (> sv ov)
    '< (< sv ov)
    '>= (>= sv ov)
    '<= (<= sv ov)
    'and (and sv ov)
    'or (or sv ov)
    'not (not ov)
    (throw-mess [:UNDEFINED-TEST pf])))

(defn apply-test 
  "Apply <test> to context <ctx> that is calculate arguments of the test
   with respect to variable values in the context and apply a function
   on predicate place of the test to these arguments"
  [test ctx]
  (try
    (let [[ss ff oo] test
          sv (eval-mp ctx ss)
          ov (eval-mp ctx oo)]
      (if (eval-test ff sv ov)
        ctx))
    (catch Exception ex
      (println [:EXCEPTION-EVAL :TEST test :ON ctx])
      (println ex)
      nil)))

(defn mk-match-list [af pattern bal ctx-list new-fid bi]
  "Make match-list"
  (let [ml (if (= af 'f)
             (map #(apply-test pattern %) ctx-list)
             (let [old-facts (amem bal)
                   only-ofs (cond
                              (nil? new-fid) old-facts
                              (= new-fid (fact-id (first old-facts))) (rest old-facts)
                              true old-facts)]
               ;;(println [:NEW-FID new-fid :BAL bal :OLD old-facts :ONLY only-ofs])
               (mapcat #(match-ctx-list only-ofs pattern % bi) ctx-list)))]
    (filter seq ml)))

(defn activate-b
  "Activate beta net cell of index <bi> with respect to a list of contexts
   already activated by a new fact with an index <new-fid>"
  [bi ctx-list new-fid]
  ;;(println [:ACTIVATE-B :BI bi :CTX-LIST ctx-list :NEW-FID new-fid])
  (let [bnode (bnet bi)
        [eix bal afpat & tail] bnode
        [af & pattern] afpat
        match-list (mk-match-list af pattern bal ctx-list new-fid bi)]
    (if (seq match-list)
      (condp = eix
        'x (add-to-confset tail match-list)
        'i (do
             (set-bmem bi (concat match-list (bmem bi)))
             (activate-b (inc bi) match-list nil)) ) )))

(defn entry-a-action [bi afpat b-mem a-mem]
  "Entry alpha activation"
  ;;(println [:ENTRY-A-ACTION :BI bi :AFPAT afpat :BMEM b-mem :AMEM a-mem])
  (let [[af & pattern] afpat]
    (if (= af 'a)
      (let [new-fact (first a-mem)
            ctx (match-ctx new-fact pattern {} bi)]
        (set-bmem bi (cons ctx b-mem))
        (activate-b (inc bi) (list ctx) (fact-id new-fact)))
      (throw-mess [:EXCEPTION-ENTRY-TEST afpat]))))

(defn inter-a-action [bi afpat b-mem a-mem]
  "Intermediate alpha activation"
  ;;(println [:INTER-A-ACTION :BI bi :AFPAT afpat :BMEM b-mem :AMEM a-mem])
  (let [[af & pattern] afpat
        ctx-list (bmem (dec bi))
        new-fact (first a-mem)
        match-list (filter seq
                           (if (= af 'f)
                             (map #(apply-test pattern %) ctx-list)
                             (map #(match-ctx new-fact pattern % bi) ctx-list)))]
    (when (seq match-list)
      (if (= af 'a)
        (set-bmem bi (concat match-list b-mem)))
      (activate-b (inc bi) match-list (fact-id new-fact)))))

(defn exit-a-action [bi afpat tail a-mem]
  "Exit alpha activation"
  ;;(println [:EXIT-A-ACTION :BI bi :AFPAT :TAIL tail :AFPAT afpat :AMEM a-mem])
  (let [[af & pattern] afpat
        ctx-list (bmem (dec bi))
        match-list (filter seq
                           (if (= af 'f)
                             (map #(apply-test pattern %) ctx-list)
                             (map #(match-ctx (first a-mem) pattern % bi) ctx-list)))]
    ;;(println [:MATCH-LIST match-list])
    (if (seq match-list)
      (add-to-confset tail match-list))))

(defn enex-a-action [bi afpat tail a-mem]
  "Entry and exit alpha activation (for LHS with 1 pattern)"
  ;;(println [:ENEX-A-ACTION :AFPAT :TAIL tail afpat :AMEM a-mem])
  (let [[af & pattern] afpat]
    (if (= af 'a)
      (if-let [ctx (match-ctx (first a-mem) pattern {} bi)]
        (add-to-confset tail (list ctx)))
      (throw-mess [:EXCEPTION-ENTRY-TEST afpat]))))

(defn activate-a
  "Activate alpha net cells for index list <ais>"
  [ais]
  ;;(println [:ACTIVATE-A :AIS ais])
  (doseq [ai ais]
    (let [ablinks (aget =ABLINK= ai)
          bnms (map #(list % (bnet %) (bmem %)) ablinks)
          a-mem (amem ai)]
      (doseq [[bi [eix bal afpat & tail] b-mem] bnms]
        (condp = eix
          'e (entry-a-action bi afpat b-mem a-mem)
          'i (inter-a-action bi afpat b-mem a-mem)
          'x (exit-a-action bi afpat tail a-mem)
          'ex (enex-a-action bi afpat tail a-mem)) ) )))

(defn eval-then-mp [mp expr]
  "Evaluation for right hand side"
  ;;(println [:EVAL-THEN-MP mp expr])
  (cond
    (vector? expr)(vec (map #(eval-then-mp mp %) expr))
    (seq? expr) (apply (first expr) (map #(eval-then-mp mp %) (rest expr)))
    true (or (mp expr) expr)))

(defn fire-resolved [reso]
  "Fire resolved production"
  ;;(println [:FIRE-RESOLVED reso])
  (let [[[pn sal rhs] ctx] reso]
    (doseq [exp rhs]
      (eval-then-mp ctx exp))))

(defn a-indices2
  "For an asserted triple find all suitable alpha memory cells (more effective than a-indices)"
  [fact]
  (def =i= nil)
  (let [[s p o & r] fact
        ss (.get =ANET= s)
        s? (.get =ANET= '?)]
    (if ss (let	[ssp (.get ss p)
                 ss? (.get ss '?)]
             (when ssp
               (def =i= (cons (.get ssp o) =i=))
               (def =i= (cons (.get ssp '?) =i=)))
             (when ss?
               (def =i= (cons (.get ss? o) =i=))
               (def =i= (cons (.get ss? '?) =i=))) ))
    (if s? (let	[s?p (.get s? p)
                 s?? (.get s? '?)]
             (when s?p
               (def =i= (cons (.get s?p o) =i=))
               (def =i= (cons (.get s?p '?) =i=)))
             (when s??
               (def =i= (cons (.get s?? o) =i=))
               (def =i= (cons (.get s?? '?) =i=))) ))
    (filter number? =i=)))

(defn find-fact-id [trip]
  "Find existing fact id for arbitrary triplet"
  (let [[s p o] trip]
    (if-let [shm (.get =FMEM= p)]
      (if-let [phm (.get shm s)]
        (.get phm o)) )))

(defn remove-ctx-with [fid ctxlist]
  "Remove context for given fact id"
  ;;(println [:REMOVE-CTX-WITH :FID fid :CTXLIST ctxlist])
  (filter #(not (some #{fid} (:FIDS %))) ctxlist))

(defn retract-b [fid bis]
  "Retract fact id from the beta memory"
  ;;(println [:RETRACT-B :FID fid :BIS bis])
  (doseq [bi bis]
    (loop [i bi]
      (set-bmem i (remove-ctx-with fid (bmem i)))
      (let [ni (inc i)]
        (if (< ni =BCNT=)
          (let [eix (first (bnet ni))]
            (if (or (= eix 'i) (= eix 'x))
              (recur ni))) ) ))))

(defn remove-fact [trip fid fmem]
  "Remove existing fact from the fact memory"
  ;;(println [:REMOVE-FACT :TRIP trip :FID fid :FMEM fmem])
  (let [[s p o] trip
        nfm
        (if-let [pm (.get fmem p)]
          (if-let [sm (.get pm s)]
            (if-let [fi (.get sm o)]
              (if (= fid fi)
                (let [xsm (do (.remove sm o) sm)
                      xpm (do (.remove pm s) pm)
                      xfm (do (.remove fmem p) fmem)
                      npm (if (.isEmpty xsm)
                            xpm
                            (do (.put xpm s xsm) xpm))]
                  (if (.isEmpty npm)
                    xfm
                    (do (.put xfm p npm) xfm)) ))) ))]
    (if (nil? nfm) fmem nfm)))

(defn remove-list [lst from]
  (reduce #(remove #{%2} %1) from lst))

(defn retract-trip [trip]
  "Retract triplet"
  ;;(println [:RETRACT-TRIP trip])
  (if-let [fid (find-fact-id trip)]
    (let [ais (a-indices2 trip)]
      ;; retract from alpha nodes
      (doseq [ai ais]
        (set-amem ai (remove #(= (fact-id %) fid) (amem ai)) ))
      ;; retract from beta nodes
      (retract-b fid (.get  =FIDS= fid))
      ;; remove from conflict set
      (def =CFSET=
        (filter #(not (some #{fid} (:FIDS (second %)))) =CFSET=))
      (.remove  =FIDS= fid)
      ;; remove from fact memory
      (def =FMEM= (remove-fact trip fid =FMEM=)) )))

(defn assert-trip
  "Function for assertion of one triple <trip> outside of the right hand side of rules"
  [trip]
  ;;(println [:ASSERT-TRIP trip])
  (if-let [fact (mk-fact trip)]
    (when-let [ais (a-indices2 fact)]
      ;; fill alpha nodes
      (doseq [ai ais]
        (set-amem ai (cons fact (amem ai)) ))
      ;; activate alpha nodes
      (activate-a ais))))

(defn assert-list
  "Function for assertion a list of triples or object descriptions (see comments on the function 'asser'). 
   For the use outside of the right hand side of rules"
  [lst]
  (doall (map assert-trip (trans-lhs lst))))

(defn assert-fact [fact]
  "Function for assertion of one fact: triple or object description"
  (assert-list [fact]))

(defn retract-list [lst]
  "Retract list of facts"
  (doseq [trip (trans-lhs lst)]
    (retract-trip trip)))

(defn fire
  "Fire!"
  ([]
    (while (not (empty? =CFSET=))
      (fire 1)))
  ([n]
    (dotimes [i n]
      (if (not (empty? =CFSET=))
        (let [[reso & remain] (sort-by #(- (salience (first %))) =CFSET=)]
          (def =CFSET= remain)
          ;;(println [:RESO reso])
          (fire-resolved reso)) ) )))

(defn asser
  "Function for the facts assertion that can be used in the right hand side of the rule.
   It has unfixed number of arguments that as a whole represent an 'object description'
   that is a list the form:
   (<object identifier> <property1> <value1> <property2> <value2> ... ).
   This description will be translated before assertion into the list of triples of the form:
   ((<object identifier> <property1> <value1>) (<object identifier> <property2> <value2>) ... )
   and all triples will be asserted into the alpha memory"
  [& args]
  (let [trips (trans-lhs (list args))]
    (doseq [trip trips]
      (assert-trip trip))))

(defn retract [& args]
  "Retract list of facts (maybe in 'object' form)"
  ;;(println [:RETRACT lst])
  (let [trips (trans-lhs (list args))]
    (doseq [trip trips]
      (retract-trip trip))))

(defn hm-to-list [hm]
  "HashMap to list"
  (letfn [(cons-kv [kv]
                   (let [[k v] kv
                         mtl (hm-to-list v)]
                     (map #(cons k %) mtl)))
          (to-seq [hm]
                  (loop [keys (.keySet hm) res nil]
                    (if (seq keys)
                      (recur (rest keys) (cons [(first keys) (.get hm (first keys))] res))
                      res)))]
    (if (= (type hm) java.util.HashMap)
      (mapcat cons-kv (to-seq hm))
      (list (list hm)))))

(defn facts []
  "List of facts"
  (sort-by #(nth % 3)
           (map #(cons (second %) (cons (first %) (rest (rest %)) ))
                (hm-to-list =FMEM=))))

(defn div
  "Function representing division"
  [x y]
  (/ x y))

(defn exist [& args]
  "Existential function used in left hand side"
  ;;(println [:EXIST args :TRANS-ARGS (trans-lhs [args])])
  (let [trips (trans-lhs [args])
        fids (map find-fact-id trips)
        nums (filter number? fids)]
    (== (count nums) (count trips))))

(defn run-synch [pset fset]
  "Create RETE for pset and assert and fire facts from fset synchronously"
  (create-rete pset)
  (doseq [f (trans-lhs fset)]
    (assert-trip f)
    (fire)))

(defn run-asynch [pset fset]
  "Create RETE for pset and assert facts from fset.
   After that issue 'Fire!'"
  (create-rete pset)
  (assert-list fset)
  (fire))
  
(defmacro rutime [expr]
  "Calculate and print time of calculation expression.
   Returns result of calculation"
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (println (str "Elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     ret#))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if (= (count args) 3)
    (let [mode (first args)
          pset (read-string (slurp (nth args 1)))
          fset (read-string (slurp (nth args 2)))]
      (if (= mode "synch")
        (rutime (run-synch pset fset))
        (rutime (run-asynch pset fset))))))
