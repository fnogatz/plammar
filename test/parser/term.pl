term:
  term([
    name([
      name_token(
        letter_digit_token([
          small_letter_char(a)]))]),
    name([
      name_token(
        graphic_token([
          graphic_token_char(graphic_char('.'))]))]),
    name([
      layout_text_sequence([
        layout_text(
          layout_char(
            space_char(' ')))]),
      name_token(
        letter_digit_token([
          small_letter_char(b)]))]),
    name([
      name_token(
        graphic_token([
          graphic_token_char(
            graphic_char('.'))]))])])
  <=> "a. b.".

term:
  term([
    name([
      name_token(
        graphic_token([
          graphic_token_char(
            graphic_char('.')),
          graphic_token_char(
            graphic_char('.'))]))])])
  <=> "..".

term: "a(X).".
term: "member(X,[X|_]).".
term: "member(X,[_|Xs]) :- member(X,Xs).".
term: "a b.".

% There is no logic in the tokenizing, so all of
%   them work, although not valid Prolog programs:
term: "a(1). b.".
term: "a(1). b./*test*/".
term: "a(1). b/*test*/.".
term: "a(1). /*test*/b.".
term: "a(1)./*test*/ b.".
term: "a(1)/*test*/. b.".
term: "a(1/*test*/). b.".
term: "a(/*test*/1). b.".
term: "a/*test*/(1). b.".
term: "/*test*/a(1). b.".
