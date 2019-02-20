:- module(plammar_util, [
    warning/1,
    warning/2,
    list_open/2,
    list_close/1
  ]).

warning(Format, Arguments) :-
  print_message(warning, format(Format, Arguments)).
warning(Msg) :-
  warning(Msg, []).

list_open(List, Open_List) :-
  append(List, _, Open_List).

list_close([]).
list_close([_|Xs]) :-
  ( var(Xs) -> Xs = []
  ; list_close(Xs) ).
