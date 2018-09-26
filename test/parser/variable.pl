named_variable('Var'):
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
    ]).

named_variable('smallletter'):
  fail.

named_variable('Var_with_underscores'):
  true.

named_variable('Var_with_numbers_1'):
  true.

anonymous_variable('_'):
  anonymous_variable(
      variable_indicator_char(
        underscore_char('_'))).

named_variable('_var'):
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
    ]).

variable('_var'):
  true.

variable('V'):
  true.

variable('Var'):
  true.
