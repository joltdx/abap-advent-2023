CLASS zcl_advent_2023_01 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS
      text_to_digit
        IMPORTING
          text          TYPE string
        RETURNING
          VALUE(result) TYPE i.

ENDCLASS.



CLASS zcl_advent_2023_01 IMPLEMENTATION.

  METHOD part_1.

    DATA calibration_value TYPE i.
    DATA offset_first TYPE i.
    DATA offset_last TYPE i.
    DATA sum_of_values TYPE int8.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      FIND ALL OCCURRENCES OF REGEX '\d' IN <line> MATCH COUNT DATA(count) RESULTS DATA(digits_found).
      offset_first = digits_found[ 1 ]-offset.
      offset_last = digits_found[ count ]-offset.
      calibration_value = |{ <line>+offset_first(1) }{ <line>+offset_last(1) }|.
      sum_of_values = sum_of_values + calibration_value.
    ENDLOOP.

    result = sum_of_values.

  ENDMETHOD.

  METHOD part_2.

    " Oh, eighthree is supposed to be 83, so we cant just replace text to digits going left to right   >_<
    " I'll find first and last text digit and act appropriately

    DATA(new_input) = input.
    DATA(numbers_regex) = 'one|two|three|four|five|six|seven|eight|nine'.
    DATA(reverse_regex) = reverse( numbers_regex ).

    LOOP AT new_input ASSIGNING FIELD-SYMBOL(<line>).
      FIND FIRST OCCURRENCE OF REGEX numbers_regex IN <line> MATCH OFFSET DATA(first_offset)
                                                             MATCH LENGTH DATA(first_length).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      DATA(first_text) = <line>+first_offset(first_length).

      DATA(reverse_line) = reverse( <line> ).
      FIND FIRST OCCURRENCE OF REGEX reverse_regex IN reverse_line MATCH OFFSET DATA(last_offset)
                                                                   MATCH LENGTH DATA(last_length).
      last_offset = strlen( <line> ) - last_offset - last_length.

      DATA(last_text) = <line>+last_offset(last_length).


      IF first_offset + first_length - 1 = last_offset.
        " Overlapping matches...
        <line> = replace( val = <line>
                          off = last_offset + 1
                          len = last_length - 1
                          with = text_to_digit( last_text ) ).
        <line> = replace( val = <line>
                          off = first_offset
                          len = first_length
                          with = text_to_digit( first_text ) ).

      ELSEIF first_offset = last_offset.
        " Same match - replace only once
        <line> = replace( val = <line>
                          off = first_offset
                          len = last_length
                          with = text_to_digit( first_text ) ).

      ELSE.
        " Two different matches
        <line> = replace( val = <line>
                          off = last_offset
                          len = last_length
                          with = text_to_digit( last_text ) ).
        <line> = replace( val = <line>
                          off = first_offset
                          len = first_length
                          with = text_to_digit( first_text ) ).
      ENDIF.

    ENDLOOP.

    result = part_1( new_input ).

  ENDMETHOD.

  METHOD text_to_digit.

    CASE text.
      WHEN 'one'.
        result = 1.
      WHEN 'two'.
        result = 2.
      WHEN 'three'.
        result = 3.
      WHEN 'four'.
        result = 4.
      WHEN 'five'.
        result = 5.
      WHEN 'six'.
        result = 6.
      WHEN 'seven'.
        result = 7.
      WHEN 'eight'.
        result = 8.
      WHEN 'nine'.
        result = 9.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
