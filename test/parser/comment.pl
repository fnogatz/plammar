single_line_comment: "% c\n".

% missing ending newline
single_line_comment! "% c".

single_line_comment: "% a % b\n".

bracketed_comment:
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
        ]) ])
  <=> "/* cs */".

% multiline comment
bracketed_comment: "/* \n ...newlines... \n */".
