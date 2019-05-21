:- module('plammar/state', [
    initial_state/2,
    state_space/3,
    set_context/3,
    set_context/4,
    get_context/2,
    get_context/3,
    get_context/4,
    del_context/3,
    del_context/5,
    inc_context/4,
    dec_context/4,
    state_warn/3
  ]).

:- use_module(library(plammar/util)).

initial_state(Options, SN) :-
  option(initial_state(Initial_State), Options, []),
  S0 = [
    pos(1:0),
    warnings([])
  ],
  merge_options(S0, Initial_State, SN).

set_context(S0, SN, Value) :-
  option(context(Context0), S0, []),
  set_option(value(Value), Context0, Context1),
  set_option(context(Context1), S0, SN).

set_context(S0, SN, Namespace, Value) :-
  \+ is_list(Value),
  option(context(Context0), S0, []),
  What0 =.. [Namespace, NS0],
  option(What0, Context0, []),
  set_option(Value, NS0, NS1),
  What1 =.. [Namespace, NS1],
  set_option(What1, Context0, Context1),
  set_option(context(Context1), S0, SN).

set_context(S0, S0, _Namespace, []).
set_context(S0, SN, Namespace, [V|Vs]) :-
  set_context(S0, S1, Namespace, V),
  set_context(S1, SN, Namespace, Vs).

get_context(S0, Value) :-
  option(context(Context0), S0, []),
  option(value(Value), Context0, no_context).

get_context(S0, Namespace, Value) :-
  get_context(S0, Namespace, Value, no_context).

get_context(S0, Namespace, Value, Default) :-
  option(context(Context0), S0, []),
  What0 =.. [Namespace, NS0],
  option(What0, Context0, []),
  option(Value, NS0, Default).

del_context(S0, SN, Value) :-
  option(context(Context0), S0, []),
  select_option(value(Value), Context0, Context1, no_context),
  set_option(context(Context1), S0, SN).

del_context(S0, SN, Namespace, Value, Default) :-
  option(context(Context0), S0, []),
  What0 =.. [Namespace, NS0],
  option(What0, Context0, []),
  select_option(Value, NS0, NS1, Default),
  What1 =.. [Namespace, NS1],
  set_option(What1, Context0, Context1),
  set_option(context(Context1), S0, SN).
/*
del_context(S0, SN, Namespace) :-
  option(context(Context0), S0, []),
  What =.. [Namespace, _],
  select_option(What, Context0, Context1, _),
  set_option(context(Context1), S0, SN).
*/
inc_context(S0, SN, Namespace, Prop) :-
  What0 =.. [Prop, Value0],
  del_context(S0, S1, Namespace, What0, 0),
  Value1 is Value0 + 1,
  What1 =.. [Prop, Value1],
  set_context(S1, SN, Namespace, What1).

dec_context(S0, SN, Namespace, Prop) :-
  What0 =.. [Prop, Value0],
  del_context(S0, S1, Namespace, What0, 1),
  Value1 is Value0 - 1,
  What1 =.. [Prop, Value1],
  set_context(S1, SN, Namespace, What1).

state_space(S0, SN, cols(N)) :-
  option(pos(L0:C0), S0),
  L1 = L0,
  C1 is C0 + N,
  set_option(pos(L1:C1), S0, SN).

state_space(S0, SN, rows(N)) :-
  option(pos(L0:_C0), S0),
  L1 is L0 + N,
  C1 = 0,
  set_option(pos(L1:C1), S0, SN).

state_warn(S0, SN, List) :-
  option(warnings(Warnings0), S0, []),
  option(pos(L0:C0), S0),
  Warning = warn([pos(L0:C0)|List]),
  append(Warnings0, [Warning], Warnings1),
  merge_options([warnings(Warnings1)], S0, SN).
