%% based on letter_digit_token

name:
  name([
    name_token('a',
      letter_digit_token([
        small_letter_char(a) ]))])
  <=> "a".

name:
  name([
    name_token('ab',
      letter_digit_token([
        small_letter_char(a),
        alphanumeric_char(
          alpha_char(
            letter_char(
              small_letter_char(b) )))]))])
  <=> "ab".

name: "abc".
name: "a_".
name: "a0".

name! "Abc". % beginning with capital letter
name! "0ab". % beginning with number

%% based on graphic_token

name:
  name([
    name_token('<=>',
      graphic_token([
        graphic_token_char(graphic_char('<')),
        graphic_token_char(graphic_char('=')),
        graphic_token_char(graphic_char('>')) ]))])
  <=> "<=>".

%% based on quoted_token

name:
  name([
    name_token('\'a\'',
      quoted_token([
        single_quote_char('\''),
        single_quoted_item(
          single_quoted_character(
            non_quote_char(
              alphanumeric_char(
                alpha_char(
                  letter_char(small_letter_char(a))))))),
        single_quote_char('\'') ]))])
  <=> "'a'".

%% based on semicolon_token

name:
  name([
    name_token(';',
      semicolon_token(
        semicolon_char(';') ))])
  <=> ";".

%% based on cut_token

name:
  name([
    name_token('!', 
      cut_token(cut_char(!)))])
  <=> "!".
