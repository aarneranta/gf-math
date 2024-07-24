# FortTheL grammar and extensions

Based on documentation in based on http://nevidal.org/download/forthel.pdf

We have
- tried to simplify the abstract syntax by using GF's facilities
- built an RGL functor and instantiated it for a few languages
- not tried to eliminate ambiguities that a reader unfamiliar with ForTheL might naturally see


Hence, as out goal, a category is defined by its syntax and
semantics: for each tuple
```
  (semantic_type, linearization_type, combination_possibilities)
```
there is just one category.

