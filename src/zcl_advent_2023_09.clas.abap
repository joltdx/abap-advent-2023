CLASS zcl_advent_2023_09 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_values_tt TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    METHODS predict_next_increment
      IMPORTING
        values        TYPE ty_values_tt
      RETURNING
        VALUE(result) TYPE i.

    METHODS predict_first_decrement
      IMPORTING
        values        TYPE ty_values_tt
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.



CLASS zcl_advent_2023_09 IMPLEMENTATION.

  METHOD part_1.

    DATA line_values TYPE ty_values_tt.
    DATA sum_next_values TYPE int8.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      SPLIT <line> AT ' ' INTO TABLE DATA(line_numbers).
      line_values = line_numbers.
      DATA(next_value) = line_values[ lines( line_values ) ] + predict_next_increment( line_values ).

      sum_next_values += next_value.

    ENDLOOP.

    result = sum_next_values.

  ENDMETHOD.

  METHOD part_2.

    DATA line_values TYPE ty_values_tt.
    DATA sum_historical_values TYPE int8.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      SPLIT <line> AT ' ' INTO TABLE DATA(line_numbers).
      line_values = line_numbers.
      DATA(historic_value) = line_values[ 1 ] - predict_first_decrement( line_values ).

      sum_historical_values += historic_value.

    ENDLOOP.

    result = sum_historical_values.

  ENDMETHOD.

  METHOD predict_next_increment.

    DATA diff TYPE i.
    DATA differences TYPE ty_values_tt.
    DATA non_zero TYPE abap_bool.

    ASSIGN values[ 1 ] TO FIELD-SYMBOL(<previous_value>).

    LOOP AT values FROM 2 ASSIGNING FIELD-SYMBOL(<value>).
      diff = <value> - <previous_value>.
      APPEND diff TO differences.
      ASSIGN <value> TO <previous_value>.
      IF diff <> 0.
        non_zero = abap_true.
      ENDIF.
    ENDLOOP.

    IF non_zero = abap_true.
      result = diff + predict_next_increment( differences ).
    ELSE.
      result = 0.
    ENDIF.

  ENDMETHOD.

  METHOD predict_first_decrement.

    DATA diff TYPE i.
    DATA differences TYPE ty_values_tt.
    DATA non_zero TYPE abap_bool.

    ASSIGN values[ 1 ] TO FIELD-SYMBOL(<previous_value>).

    LOOP AT values FROM 2 ASSIGNING FIELD-SYMBOL(<value>).
      diff = <value> - <previous_value>.
      APPEND diff TO differences.
      ASSIGN <value> TO <previous_value>.
      IF diff <> 0.
        non_zero = abap_true.
      ENDIF.
    ENDLOOP.

    IF non_zero = abap_true.
      result = differences[ 1 ] - predict_first_decrement( differences ).
    ELSE.
      result = 0.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
