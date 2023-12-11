CLASS zcl_advent_2023_10 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_c_1 TYPE c LENGTH 1.
    TYPES ty_c_255 TYPE c LENGTH 255.
    TYPES:
      BEGIN OF ty_guide,
        direction TYPE ty_c_1,
        pipe      TYPE ty_c_1,
        next      TYPE ty_c_1,
      END OF ty_guide.

    DATA guide TYPE HASHED TABLE OF ty_guide WITH UNIQUE KEY direction pipe.
    DATA map TYPE STANDARD TABLE OF ty_c_255 WITH EMPTY KEY. "zif_advent_2023=>ty_input_table.

    METHODS initialize_things
      IMPORTING
        input TYPE zif_advent_2023=>ty_input_table.

    METHODS set_map_with_safety_padding
      IMPORTING
        input TYPE zif_advent_2023=>ty_input_table.

    METHODS determine_start
      EXPORTING
        start_offset TYPE i
        start_line   TYPE i
        direction    TYPE ty_c_1
        start_tile   TYPE ty_c_1.

ENDCLASS.



CLASS zcl_advent_2023_10 IMPLEMENTATION.

  METHOD part_1.

    DATA steps TYPE i.
    DATA pipe  TYPE ty_c_1.
    DATA line  LIKE LINE OF map.

    initialize_things( input ).

    determine_start( IMPORTING start_offset = DATA(x)
                               start_line   = DATA(y)
                               direction    = DATA(direction) ).

    DO.
      CASE direction.
        WHEN 'V'.
          x -= 1.
        WHEN 'U'.
          y -= 1.
        WHEN 'H'.
          x += 1.
        WHEN 'N'.
          y += 1.
      ENDCASE.

      line = map[ y ].
      pipe = line+x(1).

      steps += 1.

      IF pipe = 'S'.
        EXIT.
      ENDIF.

      direction = guide[ direction = direction pipe = pipe ]-next.

    ENDDO.

    DATA(half_the_steps) = steps / 2.

    result = half_the_steps.

  ENDMETHOD.

  METHOD part_2.

    DATA steps      TYPE i.
    DATA pipe       TYPE ty_c_1.
    DATA is_inside TYPE abap_bool.
    DATA line       LIKE LINE OF map.

    initialize_things( input ).

    determine_start( IMPORTING start_offset = DATA(start_offset)
                               start_line   = DATA(start_line)
                               direction    = DATA(direction)
                               start_tile   = DATA(start_tile) ).

    DATA(x) = start_offset.
    DATA(y) = start_line.

    DO.
      line = map[ y ].
      pipe = line+x(1).

      IF pipe = 'S'.
        line+x(1) = start_tile.
        pipe = start_tile.
      ENDIF.

      IF pipe = 'L' AND direction = 'U'.
        line+x(1) = '1'.      " Upward facing
      ELSEIF pipe = 'J' AND direction = 'U'.
        line+x(1) = '2'.      " Upward facing

      ELSEIF pipe = 'F' AND direction = 'N'.
        line+x(1) = '3'.
      ELSEIF pipe = '7' AND direction = 'N'.
        line+x(1) = '4'.

      ELSEIF pipe = '7' AND direction = 'V'.
        line+x(1) = '5'.
      ELSEIF pipe = 'J' AND direction = 'V'.
        line+x(1) = '6'.      " Upward facing

      ELSEIF pipe = 'L' AND direction = 'H'.
        " line+x(1) = '7'. " NOOO! 7 is one of the original tiles. o_O Spent a lot of figuring that out >_<
        line+x(1) = '9'.      " Upward facing
      ELSEIF pipe = 'F' AND direction = 'H'.
        line+x(1) = '8'.
      ELSE.
        line+x(1) = direction.
      ENDIF.
      map[ y ] = line.

      CASE direction.
        WHEN 'V'.
          x -= 1.
        WHEN 'U'.
          y -= 1.
        WHEN 'H'.
          x += 1.
        WHEN 'N'.
          y += 1.
      ENDCASE.

      line = map[ y ].
      pipe = line+x(1).

      IF x = start_offset AND y = start_line.
        EXIT.
      ENDIF.

      direction = guide[ direction = direction pipe = pipe ]-next.

    ENDDO.

    DATA(map_width) = strlen( map[ 1 ] ).
    CLEAR direction.

    " Flip is_inside at each crossing of a tile with an upwards facing contact point...

    LOOP AT map FROM 2 ASSIGNING FIELD-SYMBOL(<line>).
      DO map_width TIMES.
        x = sy-index - 1.
        DATA(here) = <line>+x(1).

        CASE here.
          WHEN 'U' OR
               'N' OR
               '1' OR
               '2' OR
               '6' OR
               '9'.
            is_inside = xsdbool( is_inside = abap_false ).

          WHEN 'V' OR
               'H' OR
               '3' OR
               '4' OR
               '5' OR
               '8'.
            " nothing

          WHEN OTHERS.
            IF is_inside = abap_true.
              <line>+x(1) = 'I'.
            ENDIF.
        ENDCASE.

      ENDDO.

    ENDLOOP.

    FIND ALL OCCURRENCES OF 'I' IN TABLE map MATCH COUNT DATA(inner_tiles).

    result = inner_tiles.

  ENDMETHOD.

  METHOD set_map_with_safety_padding.

    IF lines( map ) > 0.
      RETURN.
    ENDIF.

    DATA(map_width) = strlen( input[ 1 ] ) + 2.
    APPEND |{ '' WIDTH = map_width PAD = '.' }| TO map.
    LOOP AT input ASSIGNING FIELD-SYMBOL(<input>).
      APPEND |.{ <input> }.| TO map.
    ENDLOOP.
    APPEND |{ '' WIDTH = map_width PAD = '.' }| TO map.

  ENDMETHOD.

  METHOD initialize_things.

    set_map_with_safety_padding( input ).

    guide = VALUE #( direction = 'V' ( pipe = 'L' next = 'U' )
                                     ( pipe = '-' next = 'V' )
                                     ( pipe = 'F' next = 'N' )
                     direction = 'U' ( pipe = '7' next = 'V' )
                                     ( pipe = '|' next = 'U' )
                                     ( pipe = 'F' next = 'H' )
                     direction = 'H' ( pipe = 'J' next = 'U' )
                                     ( pipe = '-' next = 'H' )
                                     ( pipe = '7' next = 'N' )
                     direction = 'N' ( pipe = 'J' next = 'V' )
                                     ( pipe = '|' next = 'N' )
                                     ( pipe = 'L' next = 'H' ) ).

  ENDMETHOD.

  METHOD determine_start.

    FIND FIRST OCCURRENCE OF 'S' IN TABLE map
      MATCH LINE start_line
      MATCH OFFSET start_offset.

    DATA(line) = map[ start_line ].

    DATA(offset) = start_offset - 1.
    DATA(left) = line+offset(1).

    offset += 2.
    DATA(right) = line+offset(1).

    line = map[ start_line - 1 ].
    DATA(up) = line+start_offset(1).

    line = map[ start_line + 1 ].
    DATA(down) = line+start_offset(1).

    DATA(link_left) = xsdbool( matches( val = left regex = 'L|-|F' ) ).

    DATA(link_right) = xsdbool( matches( val = right regex = 'J|-|7' ) ).

    DATA(link_up) = xsdbool( matches( val = up regex = '7|\||F' ) ).

    DATA(link_down) = xsdbool( matches( val = down regex = 'J|\||L' ) ).

    IF link_left = abap_true AND link_up = abap_true.
      direction = 'U'.
      start_tile = 'J'.
      RETURN.
    ENDIF.

    IF link_left = abap_true AND link_right = abap_true.
      direction = 'H'.
      start_tile = '-'.
      RETURN.
    ENDIF.

    IF link_left = abap_true AND link_down = abap_true.
      direction = 'N'.
      start_tile = '7'.
      RETURN.
    ENDIF.

    IF link_up = abap_true AND link_right = abap_true.
      direction = 'H'.
      start_tile = 'L'.
      RETURN.
    ENDIF.

    IF link_up = abap_true AND link_down = abap_true.
      direction = 'U'.
      start_tile = '|'.
      RETURN.
    ENDIF.

    IF link_right = abap_true AND link_down = abap_true.
      direction = 'H'.
      start_tile = 'F'.
      RETURN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
