Have [git](https://git-scm.com/) and [stack](https://docs.haskellstack.org/en/stable/) installed on your system, with the route via [ghcup](https://www.haskell.org/ghcup/) being recommended for the latter.

Then:
```
git clone https://github.com/fmidue/prolog-graph-lib.git
cd prolog-graph-lib
stack build
stack exec prolog-graph -- --help
```
Also make sure you have [Graphviz](https://graphviz.org/download/) installed on your system.
