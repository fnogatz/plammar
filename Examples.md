# Examples

## Command Line Interface

*Note: Backtracked solutions are separated by `-------`.*

You have to specify the flag `--ops=_` (i.e., the list of defined operators is not bound and can therefore be extended) to allow the inferrence of operators.

### Input: `a b`

```shell
> echo "a b." | swipl -g main cli.pl -- --ops=_ --pretty
|: --------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║ 0 =< P(b) =< 1201 │  yf  │  b   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  yf,
	  [ term(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    op(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  yf  │  b   ║
║ 0 =< P(a) =< 1201 │  fx  │  a   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  yf,
	  [ term(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    op(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  yf  │  b   ║
║ 0 =< P(a) =< 1201 │  fy  │  a   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  yf,
	  [ term(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    op(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  yf  │  b   ║
║ 0 =< P(a) =< 1201 │ xfx  │  a   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  yf,
	  [ term(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    op(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  yf  │  b   ║
║ 0 =< P(a) =< 1201 │ xfy  │  a   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  yf,
	  [ term(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    op(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  yf  │  b   ║
║ 0 =< P(a) =< 1201 │ yfx  │  a   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  yf,
	  [ term(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    op(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  yf  │  b   ║
║ 0 =< P(a) =< 1201 │  xf  │  a   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  yf,
	  [ term(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    op(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  yf  │  b   ║
║ 0 =< P(a) =< 1201 │  yf  │  a   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  yf,
	  [ term(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    op(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║ 1 =< P(b) =< 1201 │  xf  │  b   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  xf,
	  [ term(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    op(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║ 0 =< P(a) =< 1201 │  fy  │  a   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  fy,
	  [ op(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    term(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  fy  │  a   ║
║ 0 =< P(b) =< 1201 │  fx  │  b   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  fy,
	  [ op(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    term(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  fy  │  a   ║
║ 0 =< P(b) =< 1201 │  fy  │  b   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  fy,
	  [ op(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    term(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  fy  │  a   ║
║ 0 =< P(b) =< 1201 │ xfx  │  b   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  fy,
	  [ op(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    term(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  fy  │  a   ║
║ 0 =< P(b) =< 1201 │ xfy  │  b   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  fy,
	  [ op(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    term(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  fy  │  a   ║
║ 0 =< P(b) =< 1201 │ yfx  │  b   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  fy,
	  [ op(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    term(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  fy  │  a   ║
║ 0 =< P(b) =< 1201 │  xf  │  b   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  fy,
	  [ op(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    term(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║       1201        │  fy  │  a   ║
║ 0 =< P(b) =< 1201 │  yf  │  b   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
(none)

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  fy,
	  [ op(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    term(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║ 1 =< P(a) =< 1201 │  fx  │  a   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text(
  [ clause_term(
      [ term(
	  fx,
	  [ op(
	      atom(
		name(
		  [ name_token(
		      letter_digit_token(
			[ small_letter_char(
			    a)
			]))
		  ]))),
	    term(
	      atom(
		name(
		  [ layout_text_sequence(
		      [ layout_text(
			  layout_char(
			    space_char(
			      ' ')))
		      ]),
		    name_token(
		      letter_digit_token(
			[ small_letter_char(
			    b)
			]))
		  ])))
	  ]),
	end(
	  [ end_token(
	      end_char(
		'.'))
	  ])
      ])
  ])
```

### Input: `if a then b`

With the following constraints:
- `a` must not be an operator.
- `b` must not be an operator.

These constraints are specified in the CLI via the option `--not-ops='[op(_,_,a), op(_,_,b)|_]'`.

Our tool shows that `then` *must* be a binary operator to make this a valid Prolog term.

```shell
> echo "if a then b." | swipl -g main cli.pl -- --ops=_ --not-ops='[op(_,_,a),op(_,_,b)|_]'        
|: --------------------------------

Operators:
╔══════════════════════╤══════╤══════╗
║      Precedence      │ Type │ Name ║
╟──────────────────────┼──────┼──────╢
║ 1 =< P(then) =< 1201 │ xfx  │ then ║
║  0 =< P(if) =< 1200  │  fy  │  if  ║
╚══════════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(xfx,[term(fy,[op(atom(name([name_token(letter_digit_token([small_letter_char(i),alphanumeric_char(alpha_char(letter_char(small_letter_char(f))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(a)]))])))]),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(t),alphanumeric_char(alpha_char(letter_char(small_letter_char(h)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(e)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(n))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔══════════════════════╤══════╤══════╗
║      Precedence      │ Type │ Name ║
╟──────────────────────┼──────┼──────╢
║ 2 =< P(then) =< 1201 │ xfx  │ then ║
║  1 =< P(if) =< 1200  │  fx  │  if  ║
╚══════════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(xfx,[term(fx,[op(atom(name([name_token(letter_digit_token([small_letter_char(i),alphanumeric_char(alpha_char(letter_char(small_letter_char(f))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(a)]))])))]),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(t),alphanumeric_char(alpha_char(letter_char(small_letter_char(h)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(e)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(n))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔══════════════════════╤══════╤══════╗
║      Precedence      │ Type │ Name ║
╟──────────────────────┼──────┼──────╢
║ 1 =< P(then) =< 1201 │ yfx  │ then ║
║  0 =< P(if) =< 1201  │  fy  │  if  ║
╚══════════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(yfx,[term(fy,[op(atom(name([name_token(letter_digit_token([small_letter_char(i),alphanumeric_char(alpha_char(letter_char(small_letter_char(f))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(a)]))])))]),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(t),alphanumeric_char(alpha_char(letter_char(small_letter_char(h)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(e)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(n))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔══════════════════════╤══════╤══════╗
║      Precedence      │ Type │ Name ║
╟──────────────────────┼──────┼──────╢
║ 1 =< P(then) =< 1201 │ yfx  │ then ║
║  1 =< P(if) =< 1201  │  fx  │  if  ║
╚══════════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(yfx,[term(fx,[op(atom(name([name_token(letter_digit_token([small_letter_char(i),alphanumeric_char(alpha_char(letter_char(small_letter_char(f))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(a)]))])))]),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(t),alphanumeric_char(alpha_char(letter_char(small_letter_char(h)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(e)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(n))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔══════════════════════╤══════╤══════╗
║      Precedence      │ Type │ Name ║
╟──────────────────────┼──────┼──────╢
║ 1 =< P(then) =< 1201 │ xfy  │ then ║
║  0 =< P(if) =< 1200  │  fy  │  if  ║
╚══════════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(xfy,[term(fy,[op(atom(name([name_token(letter_digit_token([small_letter_char(i),alphanumeric_char(alpha_char(letter_char(small_letter_char(f))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(a)]))])))]),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(t),alphanumeric_char(alpha_char(letter_char(small_letter_char(h)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(e)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(n))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔══════════════════════╤══════╤══════╗
║      Precedence      │ Type │ Name ║
╟──────────────────────┼──────┼──────╢
║ 2 =< P(then) =< 1201 │ xfy  │ then ║
║  1 =< P(if) =< 1200  │  fx  │  if  ║
╚══════════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(xfy,[term(fx,[op(atom(name([name_token(letter_digit_token([small_letter_char(i),alphanumeric_char(alpha_char(letter_char(small_letter_char(f))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(a)]))])))]),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(t),alphanumeric_char(alpha_char(letter_char(small_letter_char(h)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(e)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(n))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔══════════════════════╤══════╤══════╗
║      Precedence      │ Type │ Name ║
╟──────────────────────┼──────┼──────╢
║  1 =< P(if) =< 1201  │  fy  │  if  ║
║ 1 =< P(then) =< 1201 │ xfx  │ then ║
╚══════════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(fy,[op(atom(name([name_token(letter_digit_token([small_letter_char(i),alphanumeric_char(alpha_char(letter_char(small_letter_char(f))))]))]))),term(xfx,[term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(a)]))]))),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(t),alphanumeric_char(alpha_char(letter_char(small_letter_char(h)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(e)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(n))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))])]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔══════════════════════╤══════╤══════╗
║      Precedence      │ Type │ Name ║
╟──────────────────────┼──────┼──────╢
║  1 =< P(if) =< 1201  │  fy  │  if  ║
║ 1 =< P(then) =< 1201 │ yfx  │ then ║
╚══════════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(fy,[op(atom(name([name_token(letter_digit_token([small_letter_char(i),alphanumeric_char(alpha_char(letter_char(small_letter_char(f))))]))]))),term(yfx,[term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(a)]))]))),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(t),alphanumeric_char(alpha_char(letter_char(small_letter_char(h)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(e)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(n))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))])]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔══════════════════════╤══════╤══════╗
║      Precedence      │ Type │ Name ║
╟──────────────────────┼──────┼──────╢
║  1 =< P(if) =< 1201  │  fy  │  if  ║
║ 1 =< P(then) =< 1201 │ xfy  │ then ║
╚══════════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(fy,[op(atom(name([name_token(letter_digit_token([small_letter_char(i),alphanumeric_char(alpha_char(letter_char(small_letter_char(f))))]))]))),term(xfy,[term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(a)]))]))),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(t),alphanumeric_char(alpha_char(letter_char(small_letter_char(h)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(e)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(n))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))])]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔══════════════════════╤══════╤══════╗
║      Precedence      │ Type │ Name ║
╟──────────────────────┼──────┼──────╢
║  2 =< P(if) =< 1201  │  fx  │  if  ║
║ 1 =< P(then) =< 1200 │ xfx  │ then ║
╚══════════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(fx,[op(atom(name([name_token(letter_digit_token([small_letter_char(i),alphanumeric_char(alpha_char(letter_char(small_letter_char(f))))]))]))),term(xfx,[term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(a)]))]))),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(t),alphanumeric_char(alpha_char(letter_char(small_letter_char(h)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(e)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(n))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))])]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔══════════════════════╤══════╤══════╗
║      Precedence      │ Type │ Name ║
╟──────────────────────┼──────┼──────╢
║  2 =< P(if) =< 1201  │  fx  │  if  ║
║ 1 =< P(then) =< 1200 │ yfx  │ then ║
╚══════════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(fx,[op(atom(name([name_token(letter_digit_token([small_letter_char(i),alphanumeric_char(alpha_char(letter_char(small_letter_char(f))))]))]))),term(yfx,[term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(a)]))]))),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(t),alphanumeric_char(alpha_char(letter_char(small_letter_char(h)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(e)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(n))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))])]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔══════════════════════╤══════╤══════╗
║      Precedence      │ Type │ Name ║
╟──────────────────────┼──────┼──────╢
║  2 =< P(if) =< 1201  │  fx  │  if  ║
║ 1 =< P(then) =< 1200 │ xfy  │ then ║
╚══════════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  a   ║
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(fx,[op(atom(name([name_token(letter_digit_token([small_letter_char(i),alphanumeric_char(alpha_char(letter_char(small_letter_char(f))))]))]))),term(xfy,[term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(a)]))]))),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(t),alphanumeric_char(alpha_char(letter_char(small_letter_char(h)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(e)))),alphanumeric_char(alpha_char(letter_char(small_letter_char(n))))]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))])]),end([end_token(end_char(.))])])])
```

### Input: `g g 1`

Constraints are:
- `g` should be a prefix operator of any precedence with the type `fx`.

```shell
> echo "g g 1" | swipl -g main cli.pl -- --ops='[op(_,fx,g)|_]'
|: %
```

This correctly ends without any result, since there is simply no way `g g 1` is a valid Prolog term with a specification of `fx`.

### Input `a b c`, with `b` not an operator

Constraints are:
- `b` must not be an operator.

```shell
> echo "a b c." | swipl -g main cli.pl -- --ops=_ --not-ops='[op(_,_,b)|_]'
|: --------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║ 0 =< P(c) =< 1201 │  yf  │  c   ║
║ 0 =< P(a) =< 1201 │  fy  │  a   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(yf,[term(fy,[op(atom(name([name_token(letter_digit_token([small_letter_char(a)]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))]),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(c)]))])))]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║ 1 =< P(c) =< 1201 │  yf  │  c   ║
║ 1 =< P(a) =< 1201 │  fx  │  a   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(yf,[term(fx,[op(atom(name([name_token(letter_digit_token([small_letter_char(a)]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))]),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(c)]))])))]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║ 1 =< P(c) =< 1201 │  xf  │  c   ║
║ 0 =< P(a) =< 1200 │  fy  │  a   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(xf,[term(fy,[op(atom(name([name_token(letter_digit_token([small_letter_char(a)]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))]),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(c)]))])))]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║ 2 =< P(c) =< 1201 │  xf  │  c   ║
║ 1 =< P(a) =< 1200 │  fx  │  a   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(xf,[term(fx,[op(atom(name([name_token(letter_digit_token([small_letter_char(a)]))]))),term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))])))]),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(c)]))])))]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║ 0 =< P(a) =< 1201 │  fy  │  a   ║
║ 0 =< P(c) =< 1201 │  yf  │  c   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(fy,[op(atom(name([name_token(letter_digit_token([small_letter_char(a)]))]))),term(yf,[term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))]))),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(c)]))])))])]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║ 1 =< P(a) =< 1201 │  fy  │  a   ║
║ 1 =< P(c) =< 1201 │  xf  │  c   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(fy,[op(atom(name([name_token(letter_digit_token([small_letter_char(a)]))]))),term(xf,[term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))]))),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(c)]))])))])]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║ 1 =< P(a) =< 1201 │  fx  │  a   ║
║ 0 =< P(c) =< 1200 │  yf  │  c   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(fx,[op(atom(name([name_token(letter_digit_token([small_letter_char(a)]))]))),term(yf,[term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))]))),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(c)]))])))])]),end([end_token(end_char(.))])])])

--------------------------------

Operators:
╔═══════════════════╤══════╤══════╗
║    Precedence     │ Type │ Name ║
╟───────────────────┼──────┼──────╢
║ 2 =< P(a) =< 1201 │  fx  │  a   ║
║ 1 =< P(c) =< 1200 │  xf  │  c   ║
╚═══════════════════╧══════╧══════╝

Not Operators:
╔════════════╤══════╤══════╗
║ Precedence │ Type │ Name ║
╟────────────┼──────┼──────╢
║     *      │  *   │  b   ║
╚════════════╧══════╧══════╝

Syntax Tree:
p_text([clause_term([term(fx,[op(atom(name([name_token(letter_digit_token([small_letter_char(a)]))]))),term(xf,[term(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(b)]))]))),op(atom(name([layout_text_sequence([layout_text(layout_char(space_char( )))]),name_token(letter_digit_token([small_letter_char(c)]))])))])]),end([end_token(end_char(.))])])])
```
