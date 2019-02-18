integer_token:
  integer_token(
    integer_constant(
      [ decimal_digit_char('1'),
        decimal_digit_char('2'),
        decimal_digit_char('3')
  ])) <=> "123".

% integers are allowed to start with zeroes
integer_token: "00123".

% no space in between
integer_token! "12 34".


% no newline in between
integer_token! "12\n34".

integer_token! "a".

integer_token! "1.2".

integer: "123".

float_number_token:
  float_number_token(
    [ integer_constant(
        [ decimal_digit_char('1') ]),
      fraction(
        [ decimal_point_char('.'),
          decimal_digit_char('2') ])
    ]) <=> "1.2".

% no space in between
float_number_token! "1. 2".

% no multiple fraction signs
float_number_token! "1.2.3".

float_number_token:
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
            sign([]),
            integer_constant(
              [ decimal_digit_char('3') ])
          ]) ])
  <=> "1.2e3".

float_number_token: "1.2E3".

float_number_token: "1.2e+3".

float_number_token: "1.2e-3".
