CLASS zcl_advent_2023_16 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_contraption_string TYPE c LENGTH 12100.

    DATA contraption_width  TYPE i.
    DATA contraption_height TYPE i.
    DATA contraption        TYPE ty_contraption_string.
    DATA energized          TYPE ty_contraption_string.

    METHODS light_it
      IMPORTING
        start_x         TYPE i
        start_y         TYPE i
        start_direction TYPE c
      RETURNING
        VALUE(result)   TYPE i.

    METHODS debug_pretty_print_contraption
      IMPORTING
        contraption   TYPE string
        width         TYPE i
        height        TYPE i
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_advent_2023_16 IMPLEMENTATION.

  METHOD part_1.

    contraption_width = strlen( input[ 1 ] ).
    contraption_height = lines( input ).
    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      contraption = |{ contraption }{ <line> }|.
    ENDLOOP.

    result = light_it( start_x         = 0
                       start_y         = 0
                       start_direction = 'R' ).

  ENDMETHOD.

  METHOD part_2.

    DATA max_energized TYPE i.

    contraption_width = strlen( input[ 1 ] ).
    contraption_height = lines( input ).
    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      contraption = |{ contraption }{ <line> }|.
    ENDLOOP.

    DO contraption_width TIMES.
      DATA(x) = sy-index - 1.
      DATA(energized) = light_it( start_x = x
                                  start_y = 0
                                  start_direction = 'D' ).
      IF energized > max_energized.
        max_energized = energized.
      ENDIF.

      energized = light_it( start_x = x
                            start_y = contraption_height
                            start_direction = 'U' ).
      IF energized > max_energized.
        max_energized = energized.
      ENDIF.

    ENDDO.

    DO contraption_height TIMES.
      DATA(y) = sy-index - 1.
      energized = light_it( start_x = 0
                            start_y = y
                            start_direction = 'R' ).
      IF energized > max_energized.
        max_energized = energized.
      ENDIF.

      energized = light_it( start_x = contraption_width
                            start_y = y
                            start_direction = 'L' ).
      IF energized > max_energized.
        max_energized = energized.
      ENDIF.

    ENDDO.

    result = max_energized.

  ENDMETHOD.

  METHOD light_it.

    TYPES:
      BEGIN OF ty_beam,
        x         TYPE i,
        y         TYPE i,
        direction TYPE c LENGTH 1,
      END OF ty_beam.
    TYPES ty_beams_tt TYPE STANDARD TABLE OF ty_beam WITH EMPTY KEY.

    DATA loop_detector TYPE HASHED TABLE OF ty_beam WITH UNIQUE KEY x y direction.

    DATA(beams) = VALUE ty_beams_tt( ( x = start_x y = start_y direction = start_direction ) ).

    energized = contraption.

    LOOP AT beams ASSIGNING FIELD-SYMBOL(<beam>).
      DATA(this_beam) = <beam>.

      DO.
        IF this_beam-x < 0 OR this_beam-x >= contraption_width OR this_beam-y < 0 OR this_beam-y >= contraption_height.
          EXIT.
        ENDIF.

        IF line_exists( loop_detector[ x = this_beam-x y = this_beam-y direction = this_beam-direction ] ).
          EXIT.
        ELSE.
          INSERT this_beam INTO TABLE loop_detector.
        ENDIF.

        DATA(offset) = this_beam-y * contraption_width + this_beam-x.
        energized+offset(1) = '#'.

        CASE contraption+offset(1).
          WHEN '.'.   " Space
            CASE this_beam-direction.
              WHEN 'U'.
                this_beam-y = this_beam-y - 1.
              WHEN 'D'.
                this_beam-y = this_beam-y + 1.
              WHEN 'L'.
                this_beam-x = this_beam-x - 1.
              WHEN 'R'.
                this_beam-x = this_beam-x + 1.
            ENDCASE.

          WHEN '/'.   " Mirror
            CASE this_beam-direction.
              WHEN 'U'.
                this_beam-x = this_beam-x + 1.
                this_beam-direction = 'R'.
              WHEN 'D'.
                this_beam-x = this_beam-x - 1.
                this_beam-direction = 'L'.
              WHEN 'L'.
                this_beam-y = this_beam-y + 1.
                this_beam-direction = 'D'.
              WHEN 'R'.
                this_beam-y = this_beam-y - 1.
                this_beam-direction = 'U'.
            ENDCASE.

          WHEN '\'.   " Mirror
            CASE this_beam-direction.
              WHEN 'U'.
                this_beam-x = this_beam-x - 1.
                this_beam-direction = 'L'.
              WHEN 'D'.
                this_beam-x = this_beam-x + 1.
                this_beam-direction = 'R'.
              WHEN 'L'.
                this_beam-y = this_beam-y - 1.
                this_beam-direction = 'U'.
              WHEN 'R'.
                this_beam-y = this_beam-y + 1.
                this_beam-direction = 'D'.
            ENDCASE.

          WHEN '|'.   " Splitter
            CASE this_beam-direction.
              WHEN 'U'.
                this_beam-y = this_beam-y - 1.
              WHEN 'D'.
                this_beam-y = this_beam-y + 1.
              WHEN 'L' OR 'R'.
                APPEND VALUE ty_beam( x = this_beam-x y = this_beam-y - 1 direction = 'U' ) TO beams.
                this_beam-y = this_beam-y + 1.
                this_beam-direction = 'D'.
            ENDCASE.

          WHEN '-'.   " Splitter
            CASE this_beam-direction.
              WHEN 'U' OR 'D'.
                APPEND VALUE #( x = this_beam-x - 1 y = this_beam-y direction = 'L' ) TO beams.
                this_beam-x = this_beam-x + 1.
                this_beam-direction = 'R'.
              WHEN 'L'.
                this_beam-x = this_beam-x - 1.
              WHEN 'R'.
                this_beam-x = this_beam-x + 1.
            ENDCASE.

        ENDCASE.
      ENDDO.
    ENDLOOP.

    FIND ALL OCCURRENCES OF '#' IN energized MATCH COUNT DATA(count).

    result = count.

  ENDMETHOD.

  METHOD debug_pretty_print_contraption.

    DO height TIMES.
      DATA(line) = substring( val = contraption off = width * ( sy-index - 1 ) len = width ).
      IF result IS INITIAL.
        result = |{ line }|.
      ELSE.
        result = |{ result }\n{ line }|.
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
