((P1 0
     ((?x size small
          on ?y)
       (?y color red
           size big
           left-of ?z
           length ?k
           width ?m)
       (?m > ?k))
     =>
     ((println (str ?y " better than " ?x)) (println (+ ?k ?m))))
  
(P2 0
    ((?x length ?y
      width ?y))
    =>
    ((println (str ?x " is square or circle"))))

(P3 0
    ((?x length2 ?y)
      (?z length2 ?w)
      (?y > ?w))
    =>
    ((println (str ?x " length more than " ?z))))

(P4 0
    ((?x knows ?y)
      (?y has ?m
          has ?n
          likes swimming
          likes tennis	
          likes football
          size ?s)
      (?m != ?n))
    =>
    ((println (str ?x " likes that " ?y " has " ?m " and " ?n))))

(P5 0
    ((?p weight ?w
         height ?h)
      ((+ (/ ?w 2) 12.3) < (* 0.5 ?h)))
    =>
    ((println [:COOL! ?p ?w ?h])
      (println (str (+ (div ?w 2) 12.3) " < " (* 0.5 ?h)))))

(P6 0
    ((context state test)
      (?x status ?s1)
      (true not (exist ?x status hot valve closed)) )
    =>
    ((println [:ALL WELL ?x :STATUS ?s1]))))