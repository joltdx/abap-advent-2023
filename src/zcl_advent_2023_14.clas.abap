CLASS zcl_advent_2023_14 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_platform_string TYPE c LENGTH 10000.

    TYPES:
      BEGIN OF ty_cycle,
        platform TYPE ty_platform_string,
        cycle    TYPE i,
      END OF ty_cycle.

    TYPES ty_cycles_tt TYPE HASHED TABLE OF ty_cycle WITH UNIQUE KEY platform.

    DATA platform_width TYPE i.
    DATA platform_height TYPE i.
    DATA platform TYPE ty_platform_string.
    DATA platform_cycles TYPE ty_cycles_tt.


    METHODS tilt_north.
    METHODS tilt_west.
    METHODS tilt_south.
    METHODS tilt_east.
    METHODS cycle.
    METHODS debug_pretty_print_platform
      IMPORTING
        platform      TYPE string
        width         TYPE i
        height        TYPE i
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_advent_2023_14 IMPLEMENTATION.

  METHOD part_1.

    TYPES ty_c_tt TYPE STANDARD TABLE OF c WITH EMPTY KEY.
    TYPES ty_platform TYPE STANDARD TABLE OF ty_c_tt WITH EMPTY KEY.

    DATA platform TYPE ty_platform.

    DATA(width) = strlen( input[ 1 ] ).

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      APPEND INITIAL LINE TO platform ASSIGNING FIELD-SYMBOL(<platform>).
      DO width TIMES.
        DATA(offset) = sy-index - 1.
        APPEND <line>+offset(1) TO <platform>.
      ENDDO.
    ENDLOOP.

    DATA(height) = lines( platform ).

    DO width TIMES.
      DATA(x) = sy-index.
      DATA(y) = 2.
      WHILE y <= height.
        IF platform[ y ][ x ] = 'O'.
          IF platform[ y - 1 ][ x ] = '.'.
            platform[ y - 1 ][ x ] = 'O'.
            platform[ y ][ x ] = '.'.

            IF y > 2.
              y -= 1.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
        y += 1.
      ENDWHILE.
    ENDDO.

    DATA sum TYPE i.

    LOOP AT platform ASSIGNING <platform>.
      DATA(row_load) = height - sy-tabix + 1.
      FIND ALL OCCURRENCES OF 'O' IN TABLE <platform> MATCH COUNT DATA(count).
      sum += count * row_load.
    ENDLOOP.

    result = sum.

  ENDMETHOD.

  METHOD part_2.

    platform_width = strlen( input[ 1 ] ).
    platform_height = lines( input ).
    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      platform = |{ platform }{ <line> }|.
    ENDLOOP.

    DO 1000000000 TIMES.              " LOL
      DATA(cycle_number) = sy-index.

      cycle( ).

      READ TABLE platform_cycles WITH KEY platform = platform ASSIGNING FIELD-SYMBOL(<platform_cycle>).
      IF sy-subrc = 0.
        DATA(cycle_start) = <platform_cycle>-cycle.
        DATA(cycle_size) = cycle_number - <platform_cycle>-cycle.
        EXIT.
      ELSE.
        INSERT VALUE #( platform = platform
                        cycle    = cycle_number ) INTO TABLE platform_cycles.
      ENDIF.

    ENDDO.

    DATA(shortcut) = ( 1000000000 - cycle_start ) MOD cycle_size + cycle_start.

    READ TABLE platform_cycles WITH KEY cycle = shortcut ASSIGNING <platform_cycle>.

    platform = <platform_cycle>-platform.

    DATA sum TYPE i.

    DO platform_height TIMES.
      DATA(row_load) = platform_height - sy-index + 1.
      DATA(line) = substring( val = platform off = platform_width * ( sy-index - 1 ) len = platform_width ).
      FIND ALL OCCURRENCES OF 'O' IN line MATCH COUNT DATA(count).
      sum += count * row_load.
    ENDDO.

    result = sum.

  ENDMETHOD.


  METHOD tilt_north.

    DO platform_width TIMES.
      DATA(x) = sy-index - 1.
      DATA(y) = 1.
      WHILE y < platform_height.
        DATA(offset) = y * platform_width + x.
        IF platform+offset(1) = 'O'.
          DATA(offset_compare) = ( y - 1 ) * platform_width + x.
          IF platform+offset_compare(1) = '.'.
            platform+offset_compare(1) = 'O'.
            platform+offset(1) = '.'.
            IF y > 1.
              y -= 1.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
        y += 1.
      ENDWHILE.
    ENDDO.

  ENDMETHOD.


  METHOD tilt_west.

    DO platform_height TIMES.
      DATA(x) = 1.
      DATA(y) = sy-index - 1.
      WHILE x < platform_width.
        DATA(offset) = y * platform_width + x.
        IF platform+offset(1) = 'O'.
          DATA(offset_compare) = y * platform_width + x - 1.
          IF platform+offset_compare(1) = '.'.
            platform+offset_compare(1) = 'O'.
            platform+offset(1) = '.'.
            IF x > 1.
              x -= 1.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
        x += 1.
      ENDWHILE.
    ENDDO.

  ENDMETHOD.


  METHOD tilt_south.

    DO platform_width TIMES.
      DATA(x) = sy-index - 1.
      DATA(y) = platform_height - 2.
      WHILE y >= 0.
        DATA(offset) = y * platform_width + x.
        IF platform+offset(1) = 'O'.
          DATA(offset_compare) = ( y + 1 ) * platform_width + x.
          IF platform+offset_compare(1) = '.'.
            platform+offset_compare(1) = 'O'.
            platform+offset(1) = '.'.
            IF y < platform_height - 2.
              y += 1.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
        y -= 1.
      ENDWHILE.
    ENDDO.

  ENDMETHOD.


  METHOD tilt_east.

    DO platform_height TIMES.
      DATA(x) = platform_width - 2.
      DATA(y) = sy-index - 1.
      WHILE x >= 0.
        DATA(offset) = y * platform_width + x.
        IF platform+offset(1) = 'O'.
          DATA(offset_compare) = y * platform_width + x + 1.
          IF platform+offset_compare(1) = '.'.
            platform+offset_compare(1) = 'O'.
            platform+offset(1) = '.'.
            IF x < platform_width - 2.
              x += 1.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
        x -= 1.
      ENDWHILE.
    ENDDO.

  ENDMETHOD.


  METHOD cycle.

    tilt_north( ).
    tilt_west( ).
    tilt_south( ).
    tilt_east( ).

  ENDMETHOD.


  METHOD debug_pretty_print_platform.

    DO height TIMES.
      DATA(line) = substring( val = platform off = width * ( sy-index - 1 ) len = width ).
      IF result IS INITIAL.
        result = |{ line }|.
      ELSE.
        result = |{ result }\n{ line }|.
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
