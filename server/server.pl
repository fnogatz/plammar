:- use_module(library(plammar)).

:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).

:- use_module(library(http/http_cors)).
:- set_setting(http:cors, [*]).

:- set_prolog_flag(report_error,true).

:- http_handler(/, handle, [spawn(for_fresh_variables)]).

server(Port) :-
   http_server(http_dispatch,[port(Port)]),
   format(user_output,"Started server on port ~p.~n",[Port]).

handle(Request) :-
   cors_enable,
   member(method(Method), Request),
   handle(Method, Request).

handle(post, Request) :-
   !,
   http_read_data(Request, Data, []),
   % format(user_output, "Data is: ~p~n", [Data]),
   atom_chars(Data, Chars),
   Options = [
      targets([swi]),
      infer_operators(no)
   ],
   (
      % format(user_output, "Data is: ~p~n", [string_tree(Data,'Tree')]),
      % with_output_to(string(S), string_tree(Data, Tree)),
      % format(user_output, "Output is: ~p~n", [S])
      catch(
         call_with_time_limit(1.0, (
            prolog_parsetree(chars(Chars), AST, Options)
         )),
         _Catcher,
         reply_error
      ),
      reply_by(Request, AST)
   ;
      reply_error
   ).

reply_error(Msg) :-
   \+var(Msg),
   format(user_output, 'There has been an error: ~w~n', [Msg]),
   reply_json_dict(_{
      type: error,
      message: 'Unexpected Error',
      error: Msg
   }).

reply_error :-
   reply_json_dict(_{
      message: 'Program could not be parsed.',
      type: error
   }).

reply_by(Request, Tree) :-
   member(accept(Accept), Request),
   member(media(application/json,_,_,_), Accept),
   !,
   reply_tree(json, Tree).
reply_by(Request, Tree) :-
   member(accept(Accept), Request),
   member(media(text/'x-prolog',_,_,_), Accept),
   !,
   reply_tree(prolog, Tree).

reply_tree(json, Tree) :-
   tree_to_dict(Tree, Dict),
   with_output_to(string(S),
      print_term(Tree, [indent_arguments(2),tab_width(0),output(current_output)])),
   reply_json_dict(Dict.put('__source', S)). 
reply_tree(prolog, Tree) :-
   format('Content-Type: text/x-prolog~n~n', []),
   print_term(Tree, [indent_arguments(2),tab_width(0),output(current_output)]).

tree_to_dict(Tree, Dict) :-
   is_list(Tree),
   !,
   maplist(tree_to_dict, Tree, Dict).
tree_to_dict(Tree, Dict) :-
   Tree =.. [Name, Inner],
   is_list(Inner),
   !,
   maplist(tree_to_dict, Inner, Inner_Dicts),
   Dict = _{
      type: Name,
      elements: Inner_Dicts
   }.
tree_to_dict(Tree, Dict) :-
   Tree =.. [Name, Inner],
   compound(Inner),
   !,
   tree_to_dict(Inner, InnerDict),
   InnerName = InnerDict.type,
   Dict = _{
      type: Name
   }.put(InnerName, InnerDict).
tree_to_dict(Tree, Dict) :-
   Tree =.. [Name, Token, Inner],
   compound(Inner),
   !,
   tree_to_dict(Inner, InnerDict),
   InnerName = InnerDict.type,
   Dict = _{
      type: Name,
      token: Token
   }.put(InnerName, InnerDict).
tree_to_dict(Tree, Dict) :-
   Tree =.. [Name, Inner],
   atomic(Inner),
   !,
   Dict = _{
      type: Name,
      value: Inner
   }.
tree_to_dict(term(Op,List), Dict) :-
   !,
   tree_to_dict(List, ListDict),
   Dict = _{
      type: term,
      op: Op,
      elements: ListDict
   }.

add_types_to_tree(List, Typed) :-
   is_list(List),
   !,
   maplist(add_types_to_tree, List, Typed).
add_types_to_tree(Tree, Tree) :-
   is_dict(Tree),
   dict_pairs(Tree, Tag, _),
   member(Tag, [pos, range]),
   !.
add_types_to_tree(Tree, Typed) :-
   is_dict(Tree),
   !,
   dict_pairs(Tree, Type, Pairs),
   maplist(add_types_to_tree, Pairs, Typed_Pairs),
   dict_pairs(Typed, Type, [type-Type|Typed_Pairs]).
add_types_to_tree(Key-Value, Key-Typed) :-
   !,
   add_types_to_tree(Value, Typed).
add_types_to_tree(Value, Value) :-
   atom(Value),
   !.
