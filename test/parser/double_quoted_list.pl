double_quoted_list_token:
  double_quoted_list_token('"a"', [
    double_quote_char('"'),
    double_quoted_item(
      double_quoted_character(
        non_quote_char(
          alphanumeric_char(
            alpha_char(
              letter_char(
                small_letter_char(a))))))),
    double_quote_char('"') ])
  <=> "\"a\"".

double_quoted_list_token: "\"\\\"\"".
