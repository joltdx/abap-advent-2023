CLASS zcl_advent_2023_03 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_safe_input
      IMPORTING
        input         TYPE zif_advent_2023=>ty_input_table
      RETURNING
        VALUE(result) TYPE zif_advent_2023=>ty_input_table.

ENDCLASS.



CLASS zcl_advent_2023_03 IMPLEMENTATION.

  METHOD part_1.

    DATA part_number TYPE i.
    DATA part_number_sum TYPE int8.
    DATA next_line_number TYPE i.
    FIELD-SYMBOLS <last_line> LIKE LINE OF input.
    FIELD-SYMBOLS <this_line> LIKE LINE OF input.
    FIELD-SYMBOLS <next_line> LIKE LINE OF input.

    DATA(safe_input) = get_safe_input( input ).

    DATA(schematic_height) = lines( safe_input ) - 2.

    ASSIGN safe_input[ 1 ] TO <last_line>.
    ASSIGN safe_input[ 2 ] TO <this_line>.
    ASSIGN safe_input[ 3 ] TO <next_line>.
    next_line_number = 4.

    DO schematic_height TIMES.
      FIND ALL OCCURRENCES OF REGEX '(\d+)' IN <this_line> MATCH COUNT DATA(count) RESULTS DATA(numbers_found).
      LOOP AT numbers_found ASSIGNING FIELD-SYMBOL(<number_found>).
        DATA(offset) = <number_found>-offset - 1.
        DATA(length) = <number_found>-length + 2.

        IF substring( val = <last_line> off = offset len = length ) CN '.0123456789'
        OR substring( val = <this_line> off = offset len = length ) CN '.0123456789'
        OR substring( val = <next_line> off = offset len = length ) CN '.0123456789'.

          part_number = substring( val = <this_line> off = <number_found>-offset len = <number_found>-length ).

          part_number_sum += part_number.

        ENDIF.

      ENDLOOP.

      <last_line> = <this_line>.
      <this_line> = <next_line>.
      ASSIGN safe_input[ next_line_number ] TO <next_line>.
      next_line_number += 1.

    ENDDO.

    result = part_number_sum.

  ENDMETHOD.

  METHOD part_2.

    DATA gear_ratio TYPE i.
    DATA gear_ratio_sum TYPE int8.

    DATA gear_adjacent_numbers TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    DATA(safe_input) = get_safe_input( input ).

    FIND ALL OCCURRENCES OF REGEX'(\d+)' IN TABLE safe_input RESULTS DATA(numbers_found).
    FIND ALL OCCURRENCES OF '*' IN TABLE safe_input RESULTS DATA(stars_found).

    LOOP AT stars_found ASSIGNING FIELD-SYMBOL(<star>).
      CLEAR gear_adjacent_numbers.

      LOOP AT numbers_found ASSIGNING FIELD-SYMBOL(<number_found>) WHERE line BETWEEN <star>-line - 1 AND <star>-line + 1.
        IF <star>-offset BETWEEN <number_found>-offset - 1 AND <number_found>-offset + <number_found>-length.
          APPEND substring( val = safe_input[ <number_found>-line ] off = <number_found>-offset len = <number_found>-length ) TO gear_adjacent_numbers.
        ENDIF.
      ENDLOOP.

      IF lines( gear_adjacent_numbers ) = 2.
        gear_ratio = gear_adjacent_numbers[ 1 ] * gear_adjacent_numbers[ 2 ].
        gear_ratio_sum += gear_ratio.
      ENDIF.

    ENDLOOP.

    result = gear_ratio_sum.

    " Stars found
    " Row  LINE  OFFSET  LENGTH
    " =========================
    " 1    3     4       1
    " 2    6     4       1
    " 3    10    6       1

    " Numbers found
    " Row  LINE  OFFSET  LENGTH
    " =========================
    " 1    2     1       3
    " 2    2     6       3
    " 3    4     3       2
    " 4    4     7       3
    " 5    6     1       3
    " 6    7     8       2
    " 7    8     3       3
    " 8    9     7       3
    " 9    11    2       3
    " 10   11    6       3

  ENDMETHOD.

  METHOD get_safe_input.

    " Add a "safe" border for confident and consistent use of string offsets and lengths :)
    " ............
    " .467..114...
    " ....*.......
    " ...35..633..
    " .......#....
    " .617*.......
    " ......+.58..
    " ...592......
    " .......755..
    " ....$.*.....
    " ..664.598...
    " ............

    DATA(schematic_width) = strlen( input[ 1 ] ) + 2.
    APPEND |{ '' WIDTH = schematic_width PAD = '.' }| TO result.
    LOOP AT input ASSIGNING FIELD-SYMBOL(<input>).
      APPEND |.{ <input> }.| TO result.
    ENDLOOP.
    APPEND |{ '' WIDTH = schematic_width PAD = '.' }| TO result.

  ENDMETHOD.

ENDCLASS.
