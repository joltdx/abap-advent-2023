CLASS zcl_advent_2023_13 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_c_20 TYPE c LENGTH 20.
    TYPES ty_c_20_tt TYPE STANDARD TABLE OF ty_c_20 WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_pattern,
        horizontal TYPE ty_c_20_tt,
        vertical   TYPE ty_c_20_tt,
      END OF ty_pattern.
    TYPES ty_patterns_tt TYPE STANDARD TABLE OF ty_pattern WITH EMPTY KEY.

    METHODS get_patterns
      IMPORTING
        input         TYPE zif_advent_2023=>ty_input_table
      RETURNING
        VALUE(result) TYPE ty_patterns_tt.

    METHODS get_pattern_line_score
      IMPORTING
        lines         TYPE ty_c_20_tt
      RETURNING
        VALUE(result) TYPE i.

    METHODS get_pattern_line_score_part_2
      IMPORTING
        lines         TYPE ty_c_20_tt
      RETURNING
        VALUE(result) TYPE i.

    METHODS lines_diff_by_one
      IMPORTING
        line_1        TYPE ty_c_20
        line_2        TYPE ty_c_20
      RETURNING
        VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS zcl_advent_2023_13 IMPLEMENTATION.

  METHOD part_1.

    DATA sum TYPE int8.

    DATA(patterns) = get_patterns( input ).

    LOOP AT patterns ASSIGNING FIELD-SYMBOL(<pattern>).

      DATA(horiz) = get_pattern_line_score( <pattern>-horizontal ).
      IF horiz > 0.
        sum += horiz * 100.
      ELSE.
        DATA(vertical) = get_pattern_line_score( <pattern>-vertical ).
        sum += vertical.
      ENDIF.

      IF horiz = 0 AND vertical = 0.
        DATA(nisse) = 1.
      ENDIF.

    ENDLOOP.

    result = sum.

  ENDMETHOD.

  METHOD part_2.

    DATA sum TYPE int8.

    DATA(patterns) = get_patterns( input ).

    LOOP AT patterns ASSIGNING FIELD-SYMBOL(<pattern>).

      DATA(horiz) = get_pattern_line_score_part_2( <pattern>-horizontal ).
      IF horiz > 0.
        sum += horiz * 100.
      ELSE.
        DATA(vertical) = get_pattern_line_score_part_2( <pattern>-vertical ).
        sum += vertical.
      ENDIF.

      IF horiz = 0 AND vertical = 0.
        DATA(nisse) = 1.
      ENDIF.

    ENDLOOP.

    result = sum.

  ENDMETHOD.

  METHOD get_patterns.

    APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<pattern>).

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      IF <line> IS INITIAL.
        APPEND INITIAL LINE TO result ASSIGNING <pattern>.
        CONTINUE.
      ENDIF.
      APPEND <line> TO <pattern>-horizontal.
    ENDLOOP.

    " Transpose
    LOOP AT result ASSIGNING <pattern>.
      DATA(width) = strlen( <pattern>-horizontal[ 1 ] ).
      DO width TIMES.
        APPEND INITIAL LINE TO <pattern>-vertical.
      ENDDO.
      DO width TIMES.
        DATA(pos) = sy-index - 1.
        ASSIGN <pattern>-vertical[ pos + 1 ] TO FIELD-SYMBOL(<vertical>).
        LOOP AT <pattern>-horizontal ASSIGNING FIELD-SYMBOL(<horizontal>).
          DATA(row) = sy-tabix - 1.
          <vertical>+row(1) = <horizontal>+pos(1).
        ENDLOOP.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_pattern_line_score.

    DATA possible_splits TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    DATA index_top TYPE i.
    DATA index_bottom TYPE i.

    FIELD-SYMBOLS <line_above> LIKE LINE OF lines.

    DATA(height) = lines( lines ).

    ASSIGN lines[ 1 ] TO <line_above>.
    LOOP AT lines FROM 2 ASSIGNING FIELD-SYMBOL(<line>).
      IF <line> = <line_above>.
        APPEND sy-tabix TO possible_splits.
      ENDIF.
      ASSIGN <line> TO <line_above>.
    ENDLOOP.

    LOOP AT possible_splits ASSIGNING FIELD-SYMBOL(<split>).
      index_top = <split> - 1.
      index_bottom = <split>.
      DATA(no_reflection) = abap_false.
      WHILE index_top > 0 AND index_bottom <= height.
        IF lines[ index_top ] <> lines[ index_bottom ].
          no_reflection = abap_true.
          EXIT.
        ENDIF.
        index_top -= 1.
        index_bottom += 1.
      ENDWHILE.
      IF no_reflection = abap_false.
        result = <split> - 1.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_pattern_line_score_part_2.

    DATA possible_splits TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    DATA index_top TYPE i.
    DATA index_bottom TYPE i.

    FIELD-SYMBOLS <line_above> LIKE LINE OF lines.

    DATA(height) = lines( lines ).

    ASSIGN lines[ 1 ] TO <line_above>.
    LOOP AT lines FROM 2 ASSIGNING FIELD-SYMBOL(<line>).
      IF <line> = <line_above>.
        APPEND sy-tabix TO possible_splits.
      ELSE.
        IF lines_diff_by_one( line_1 = <line_above>
                              line_2 = <line> ).
          APPEND sy-tabix TO possible_splits.
        ENDIF.
      ENDIF.
      ASSIGN <line> TO <line_above>.
    ENDLOOP.

    LOOP AT possible_splits ASSIGNING FIELD-SYMBOL(<split>).
      index_top = <split> - 1.
      index_bottom = <split>.
      DATA(no_reflection) = abap_false.
      DATA(diff_by_one) = abap_false.
      WHILE index_top > 0 AND index_bottom <= height.
        IF lines[ index_top ] <> lines[ index_bottom ].
          IF lines_diff_by_one( line_1 = lines[ index_top ]
                                line_2 = lines[ index_bottom ] ).
            IF diff_by_one = abap_true.
              no_reflection = abap_true.
              EXIT.
            ELSE.
              diff_by_one = abap_true.
            ENDIF.
          ELSE.
            no_reflection = abap_true.
            EXIT.
          ENDIF.
        ENDIF.
        index_top -= 1.
        index_bottom += 1.
      ENDWHILE.
      IF no_reflection = abap_false AND
         diff_by_one = abap_true.
        result = <split> - 1.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD lines_diff_by_one.

    DATA count_diffs TYPE i.

    DO strlen( line_1 ) TIMES.
      DATA(offset) = sy-index - 1.
      IF line_1+offset(1) <> line_2+offset(1).
        count_diffs += 1.
        IF count_diffs > 1.
          EXIT.
        ENDIF.
      ENDIF.
    ENDDO.

    IF count_diffs = 1.
      result = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
