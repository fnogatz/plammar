:- op(800, xfx, <=>).

string("a/*") <=>
  [
    name([name_token('a', letter_digit_token([small_letter_char(a)]))]),
    name([name_token('/*', graphic_token([graphic_token_char(graphic_char('/')),graphic_token_char(graphic_char('*'))]))])
  ].

string("a b c") <=>
  [
    name([name_token('a', letter_digit_token([small_letter_char(a)]))]),
    name([
      layout_text_sequence([layout_text(layout_char(space_char(' ')))]),
      name_token('b', letter_digit_token([small_letter_char(b)]))]),
    name([
      layout_text_sequence([layout_text(layout_char(space_char(' ')))]),
      name_token('c', letter_digit_token([small_letter_char(c)]))])
  ].
