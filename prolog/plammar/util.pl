:- module(plammar_util, [
    warning/1,
    warning/2,
    list_open/2
  ]).

warning(Format, Arguments) :-
  print_message(warning, format(Format, Arguments)).
warning(Msg) :-
  warning(Msg, []).

list_open(List, Open_List) :-
  append(List, _, Open_List).
