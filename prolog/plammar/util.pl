:- module(plammar_util, [
    warning/1,
    warning/2
  ]).

warning(Format, Arguments) :-
  print_message(warning, format(Format, Arguments)).
warning(Msg) :-
  warning(Msg, []).
