/*
  PT = p_text([
    clause_term([
      term(atom(name([
        name_token(a, letter_digit_token([small_letter_char(a)]))
      ]))),
      end([end_token(end_char(.))])
    ]),
    clause_term([
      term(atom(name([
        name_token(a, letter_digit_token([small_letter_char(a)]))
      ]))),
      end([end_token(end_char(.))])
    ])
  ]),
  AST = p_text([
    clause_term([
      term(atom(name(a)))
    ]),
    clause_term([
      term(atom(name([
        name_token(a, letter_digit_token([small_letter_char(a)]))
      ])))
    ])
  ]),
*/