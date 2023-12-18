CLASS zcl_advent_2023_18 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_coordinate,
        x TYPE i,
        y TYPE i,
      END OF ty_coordinate.
    TYPES ty_coordinates_tt TYPE STANDARD TABLE OF ty_coordinate WITH EMPTY KEY.

ENDCLASS.



CLASS zcl_advent_2023_18 IMPLEMENTATION.

  METHOD part_1.

    DATA x TYPE i.
    DATA y TYPE i.
    DATA add_length_outline TYPE i.
    DATA coordinates TYPE ty_coordinates_tt.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      SPLIT <line> AT space INTO DATA(line_direction) DATA(line_length) DATA(line_rgb).

      CASE line_direction.
        WHEN 'U'.
          y = y - line_length.
        WHEN 'D'.
          y = y + line_length.
        WHEN 'L'.
          x = x - line_length.
        WHEN 'R'.
          x = x + line_length.
      ENDCASE.
      add_length_outline += line_length.
      APPEND VALUE #( x = x y = y ) TO coordinates.
    ENDLOOP.

    DATA area TYPE i.
    DATA sum_area TYPE i.
    FIELD-SYMBOLS <coords> LIKE LINE OF coordinates.
    FIELD-SYMBOLS <coords_plus_1> LIKE LINE OF coordinates.

    " Shoelace it
    ASSIGN coordinates[ 1 ] TO <coords>.
    LOOP AT coordinates FROM 2 ASSIGNING <coords_plus_1>.
      area = ( <coords>-x * <coords_plus_1>-y ) - ( <coords_plus_1>-x * <coords>-y ).
      sum_area += area.
      ASSIGN <coords_plus_1> TO <coords>.
    ENDLOOP.

    sum_area += add_length_outline.

    result = sum_area / 2 + 1.

  ENDMETHOD.

  METHOD part_2.

    DATA x TYPE i.
    DATA y TYPE i.
    DATA add_length_outline TYPE i.
    DATA coordinates TYPE ty_coordinates_tt.

    DATA rgb_length TYPE string.
    DATA hex_length TYPE xstring.
    DATA length TYPE int8.
    DATA rgb_direction TYPE c.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      SPLIT <line> AT space INTO DATA(line_direction) DATA(line_length) DATA(line_rgb).

      rgb_length = |0{ substring( val = line_rgb off = 2 len = 5 ) CASE = UPPER }|.
      hex_length = rgb_length.
      length = hex_length.

      rgb_direction = substring( val = line_rgb off = 7 len = 1 ).

      CASE rgb_direction.
        WHEN '3'. " 'U'
          y = y - length.
        WHEN '1'. " 'D'
          y = y + length.
        WHEN '2'. " 'L'
          x = x - length.
        WHEN '0'. " 'R'
          x = x + length.
      ENDCASE.
      add_length_outline += length.
      APPEND VALUE #( x = x y = y ) TO coordinates.
    ENDLOOP.

    DATA area TYPE int8.
    DATA sum_area TYPE int8.
    FIELD-SYMBOLS <coords> LIKE LINE OF coordinates.
    FIELD-SYMBOLS <coords_plus_1> LIKE LINE OF coordinates.

    " Shoelace it!
    ASSIGN coordinates[ 1 ] TO <coords>.
    LOOP AT coordinates FROM 2 ASSIGNING <coords_plus_1>.
      area = ( <coords>-x * <coords_plus_1>-y ) - ( <coords_plus_1>-x * <coords>-y ).
      sum_area += area.
      ASSIGN <coords_plus_1> TO <coords>.
    ENDLOOP.

    sum_area += add_length_outline.

    result = sum_area / 2 + 1.

  ENDMETHOD.

ENDCLASS.
