Panopti

A work-in-progress tool to aid understanding and manipulating Haskell.

# Current problems

* It is showing all of the potential types for a given start of an AST interval.  Fixing this requires constructing multi-term type explanations.

* In the presence of a type error, it is showing these types for every holes-in-expr-tree subset which typechecks.  Fixing this is easy, and just requires a way of selecting / only calculating a particular subset.  My plan is to choose type-partitioning via a heuristic based on how many cursors are contained within subsets, and how many overall subsets there are.

* Moving the cursor around splits semantic marks - inserting a mark consists of splitting around the insertion point and rewriting the new location's chunk.  Fixing / improving Graphics.UI.Gtk.Toy.Text would help this, but its utility as a prototype of marked text has served its purpose.  Planning to move to a modified version of trifecta's rope structure (see my github), so fixing the naive implementation has little benefit.

* The cursor isn't shown sometimes, due to too draconian of mark removal while drawing the expression diagram.

* Mark typeclass currently not being used for drawing, as the current default implementation is not powerful enough.

* Some types of interpreter errors cause a full crash ???

* No safety from non-terminating expressions


Near-term features / tasks:

* Make literals (optionally) into manipulable widgets

* Try additional prettifications, particularly for math / prelude / base functions

* Start implementing the DSL for matching local marked rope conditions, paired with resulting effects.  This is what will largely drive conditional prettifications / manipulations, as well as structure editing / refactorings.

* Make the type information process work for all elements of expressions.  This shouldn't be as hard as this sounds - just extending the "whereify" function.

* Make the annotations apply to proper source-spans, rather than column-intervals.  This is wrapped up in the conversion to trifecta for the buffer data structure.


Long term features: so many! here are some thoughts

* Hare-like refactorings / type driven structural separation and recombination (structure editing)

* API charting
  * Depict inter and intra module declaration relationships.  Relationships are always when declarations could be used together or in similar situations, as determined by the types. 

* Collaboration UI:
  * When in collaboration mode, a cursor reflects a user lock.  Cursor visually changes when all other clients have acknowledged that lock and the edit is guaranteed.

   * Provide a view of others' uncommitted changes (without these changes actually applying) - provide cherry picking (what about version control?? get the clients to agree on a micro-commit?)

* Autocompletion based on typing context, potentially informed by other heuristics:
   * Frequently seen code structures (suggest abstraction?)

   * Best practices heuristics - can also be used for refactorings proposals and the fixpoint of autocomplete, below.

* Fixpoint of autocomplete, based on a user-selected subset of functions to try to use, and a set of input / output examples.