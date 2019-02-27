named_variable:
  named_variable(
    [ capital_letter_char('V'),
      alphanumeric_char(
        alpha_char(
          letter_char(
            small_letter_char(a)))),
      alphanumeric_char(
        alpha_char(
          letter_char(
            small_letter_char(r))))
    ])
  <=> "Var".

named_variable! "smallletter".

named_variable: "Var_with_underscores".

named_variable: "Var_with_numbers_1".

anonymous_variable:
  anonymous_variable(
      variable_indicator_char(
        underscore_char('_')))
  <=> "_".

named_variable:
  named_variable(
    [ variable_indicator_char(
        underscore_char('_')),
      alphanumeric_char(
        alpha_char(
          letter_char(
            small_letter_char(v)))),
      alphanumeric_char(
        alpha_char(
          letter_char(
            small_letter_char(a)))),
      alphanumeric_char(
        alpha_char(
          letter_char(
            small_letter_char(r))))
    ])
  <=> "_var".

variable_token: "_var".

variable_token: "V".

variable_token: "Var".
