integer_token('123'):
  integer_token(
    integer_constant(
      [ decimal_digit_char(
    '1'),
        decimal_digit_char(
    '2'),
        decimal_digit_char(
    '3')
  ])).

% integers are allowed to start with zeroes
integer_token('00123'):
  true.

% no space in between
integer_token('12 34'):
  fail.

% no newline in between
integer_token('12\n34'):
  fail.

integer_token('a'):
  fail.

integer_token('1.2'):
  fail.

float_number_token('1.2'):
  float_number_token(
    [ integer_constant(
        [ decimal_digit_char('1') ]),
      fraction(
        [ decimal_point_char('.'),
          decimal_digit_char('2') ]),
      exponent([])
    ]).

% no space in between
float_number_token('1. 2'):
  fail.

% no multiple fraction signs
float_number_token('1.2.3'):
  fail.

float_number_token('1.2e3'):
  float_number_token(
    [ integer_constant(
        [ decimal_digit_char('1') ]
      ),
      fraction(
        [ decimal_point_char('.'),
          decimal_digit_char('2')
        ]),
        exponent(
          [ exponent_char(e),
            sign(positive_sign_char([])),
            integer_constant(
              [ decimal_digit_char('3') ])
          ]) ]).

float_number_token('1.2E3'):
  true.

float_number_token('1.2e+3'):
  true.

float_number_token('1.2e-3'):
  true.
