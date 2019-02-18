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
