# The CLIPS-like Language

The CLIPS-like language helps to run CLIPS examples in this environment and develop own applications. It lacks many of CLIPS features, to mention few - multislots, slot value types, complex logical expressions. Syntax also is simplified.
Application description on CLIPS-like language consist of three lists: templates, rules and facts:
```
((templates t1 t2 ... tn )
 (rules t1 t2 ... tn )
 (facts t1 t2 ... tn ))
```
Template
----
The template is a description of a specific group of facts. It is a list of the group name (type) and names of the slots. 
After each slot name can be put a default value of the slot with :default keyword before. Example:
```
(thing 
  name
  location
  on-top-of :default floor
  weight :default light)
```
Rule
----
The rule is a description of a transformation. It is represented by a list of the form:
```
(<name>
  <salience>
  <pattern1>
  <pattern_or_test2>
   ...
  <pattern_or_testN>
  =>
  <function_call1>
  <function_call2>
  <function_callM>)
```
The name is a symbol or a string.
The salience(priority) is an integer number, positive or negative.
Patterns and tests will be translated into a left hand side of the rule.
Function calls will be translated into a right hand side of the rule.

Pattern
----
The pattern is a description of a fact. It is represented by a list of the form:
```
(<group_name> <fact_variable> <slot1> <value1> <slot2> <value2> ... <slotN> <valueN>)
```
The fact_variable can be used in the right hand side of the rule in calls to functions "retract" and "modify".
As the values ​​of slots may be used constants and variables and also constructs of the form: (not x), where x is a variable or a constant. 
This latter is used instead of the CLIPS construct ~x. In that case x must exist somewhere before in this rule. 
During translation (not x) will be replaced by a generated variable ?vnnn and after this pattern will be inserted a test: (?vnnn != x).
There is no need to list all the slots of this type in the pattern, only those which are important for pattern matching.
Tests here are the same as in a native language with one exception: 
a test can be a call to the function "not" with a pattern as an argument. Example:
```
(not (thing ?x name ladder location ?place))
```

Function call
----
Function calls are calls to ordinary Clojure functions and also calls to functions "assert", "retract" and/or "modify".
The function "assert" has one argument. It has a form of the pattern. Its fact_variable must be different from those used earlier. 
During translation it will be replaced by a call to a function generating a new  fact's identifier (symbol).
The function "modify" also has one argument in the pattern's form. 
Its fact_variable must refer to some fact_variable of the pattern in the left hand side of the same rule.
No need that slots listed in the argument of "modify" function also be listed in the corresponding pattern of the left hand side.
The function "retract" has an arbitrary number of arguments that must be fact_variables on the left side of the same rule.
Example of rule:
```
(unlock-chest-with-key 0 
  (goal-is-to ?goal action unlock argument1 ?name)
  (chest ?chest name ?name contents ?contents unlocked-by ?key)
  (thing ?t name ?name location ?place on-top-of ?on)
  (monkey ?m location ?place on-top-of ?on holding ?key)
  =>
  (println (str "Monkey opens the " ?name " with the " ?key 
              " revealing the " ?contents "." ))
  (modify (chest ?chest contents nothing))
  (assert (thing ?t name ?contents location ?place on-top-of ?name))
  (retract ?goal))
```

Fact
----
The fact has a form of the pattern with an arbitrary fact_variable and constant slot values. No need to list slots that have default values, 
they will be added automatically during translation. Example:
```
(monkey ?m location t5-7 on-top-of green-couch holding blank)
```

See full example of CLIPS-like language file [mab_cli.clj] (https://github.com/rururu/rete/blob/master/examples/mab_cli.clj).
To run this example execute in REPL:
```
(require 'clips.like)
(in-ns 'clips.like)
(-main "run:asynch" "examples/mab_cli.clj")
```



