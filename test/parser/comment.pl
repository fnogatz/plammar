single_line_comment('% c\n'):
  true.

% missing ending newline
single_line_comment('% c'):
  false.

single_line_comment('% a % b\n'):
  true.

bracketed_comment('/* cs */'):
  bracketed_comment(
    [ comment_open(
        [ comment_1_char(/),
          comment_2_char(*)
        ]),
      comment_text(
        [ char(
            layout_char(
              space_char(' '))),
          char(
            alphanumeric_char(
              alpha_char(
                letter_char(
                  small_letter_char(c))))),
          char(
            alphanumeric_char(
              alpha_char(
                letter_char(
                  small_letter_char(s))))),
          char(
            layout_char(
              space_char(' ')))
        ]),
      comment_close(
        [ comment_2_char(*),
          comment_1_char(/)
        ]) ]).

% multiline comment
bracketed_comment('/* \n ...newlines... \n */'):
  true.
