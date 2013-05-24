rete
====

Clojure RETE implementation

Benchmark table
----

<table><tr><td>1</td><td>2</td></tr></table>

****************************************************
*   Test     * CLIPS (msec) * rete (msec) * factor *
****************************************************
*   manners8 *         1.47 *      161    * x 110  *
****************************************************
*  manners16 *        18    *      859    *  x 48  *
****************************************************
*  manners32 *       270    *    11100    *  x 41  *
****************************************************
*  manners64 *      8686    *   166253    *  x 19  *
****************************************************
* manners128 *    317384    *  4223090    *  x 13  *
****************************************************

Test results obtained on the same hardware.
As can be seen from the results, rete initially far behind and then starts to catch up CLIPS.

To get the results execute in REPL the command:

```
(-main "asynch" "your_path_to_manners_pset.clj_file" "your_path_to_manners_fsetN.clj_file")
```
Copyright and license
----

Copyright © 2013 Ruslan Sorokin.

Licensed under the EPL (see the file epl.html).
