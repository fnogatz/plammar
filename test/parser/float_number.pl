float_number_token:
  float_number_token('1.2',
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
  float_number_token('1.2e3',
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

float_number: "1.23".
