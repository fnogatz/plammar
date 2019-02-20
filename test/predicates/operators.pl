:- use_module(library(plammar/operators)).

'Merge of disjoint lists' :-
  Old = [op(400,fx,a), op(400,fx,b)],
  New = [op(600,fx,c), op(600,fx,d)],
  merge_operators(Old, New, Merged0),
  append(Old, New, Merged1),
  permutation(Merged0, Merged1), !.

'Merge of lists with shared operators' :-
  Old = [op(400,fx,a), op(400,fx,b)],
  New = [op(400,fx,a), op(600,fx,d)],
  merge_operators(Old, New, Merged0),
  Merged1 = [op(400,fx,a), op(400,fx,b), op(600,fx,d)],
  permutation(Merged0, Merged1), !.

'Duplicates in new list are removed' :-
  Old = [],
  New = [op(400,fx,a), op(400,fx,a)],
  merge_operators(Old, New, Merged0),
  Merged1 = [op(400,fx,a)],
  Merged1 = Merged0, !.

'Can not merge-in a list with self-conflicting operators' :-
  Old = [],
  New = [op(400,fx,a), op(600,fx,a)],
  \+ merge_operators(Old, New, Merged0), !.
