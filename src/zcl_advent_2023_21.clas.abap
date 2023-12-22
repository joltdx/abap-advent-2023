CLASS zcl_advent_2023_21 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_x_18 TYPE x LENGTH 18.
    TYPES ty_map_tt TYPE STANDARD TABLE OF xstring WITH EMPTY KEY.

    METHODS count_bits_set
      IMPORTING
        map           TYPE ty_map_tt
        width         TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS debug_pretty_print
      IMPORTING
        map           TYPE ty_map_tt
      RETURNING
        VALUE(result) TYPE string.

    METHODS debug_map_mapped_on_input
      IMPORTING
        input         TYPE zif_advent_2023=>ty_input_table
        map           TYPE ty_map_tt
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_advent_2023_21 IMPLEMENTATION.

  METHOD part_1.

    " ...........
    " .....###.#.
    " .###.##..#.
    " ..#.#...#..    11010111011
    " ....#.#....    11110101111
    " .##..S####.    10011100001    00000100000
    " .##..#...#.    10011011101
    " .......##..    11111110011
    " .##.#.####.
    " .##..##.##.
    " ...........


    DATA plots TYPE ty_map_tt.
    DATA reached TYPE ty_map_tt.
    DATA empty_line TYPE xstring.
    DATA shifted_left TYPE xstring.
    DATA shifted_right TYPE xstring.

    DATA(width) = strlen( input[ 1 ] ).
    DATA(height) = lines( input ).

    " Prepare maps from input
    DATA(hexwidth) =  ( width / 8 + 1 ) * 2.
    DATA(xbitwidth) = hexwidth * 4.
    empty_line = |{ '0' WIDTH = hexwidth PAD = '0' }|.
    shifted_left = empty_line.
    shifted_right = empty_line.


    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      APPEND empty_line TO reached.
      APPEND empty_line TO plots ASSIGNING FIELD-SYMBOL(<tiles>).

      DO width TIMES.
        DATA(offset) = sy-index - 1.
        DATA(char) = <line>+offset(1).
        IF char =  '.' OR char = 'S'.
          SET BIT offset + 1 OF <tiles> TO 1.
        ELSE.
          SET BIT offset + 1 OF <tiles> TO 0.
        ENDIF.
      ENDDO.

    ENDLOOP.


    " Set staring position
    FIND FIRST OCCURRENCE OF 'S' IN TABLE input MATCH LINE DATA(start_line) MATCH OFFSET DATA(start_position).
    ASSIGN reached[ start_line ] TO <tiles>.
    SET BIT start_position + 1 OF <tiles> TO 1.

    DATA(number_of_steps) = 6. " 6 for test data, 64 for real puzzle

    DO number_of_steps TIMES.

      DATA(stepping) = reached.

      " First, step up and down
      DO height TIMES.
        DATA(line) = sy-index.
        ASSIGN stepping[ line ] TO FIELD-SYMBOL(<this_line>).

        " Down
        IF line > 1.
          ASSIGN reached[ line - 1 ] TO FIELD-SYMBOL(<line_above>).
          <this_line> = <this_line> BIT-OR <line_above>.
        ENDIF.

        " Up
        IF line < height.
          ASSIGN reached[ line + 1 ] TO <line_above>.
          <this_line> = <this_line> BIT-OR <line_above>.
        ENDIF.

        DATA(shift_left_right) = reached[ line ].

        DO xbitwidth TIMES.
          DATA(bit) = sy-index.
          GET BIT bit OF shift_left_right INTO DATA(bit_value).
          IF bit > 1.
            SET BIT bit - 1 OF shifted_left TO bit_value.
          ENDIF.
          IF bit < xbitwidth.
            SET BIT bit + 1 OF shifted_right TO bit_value.
          ENDIF.
        ENDDO.

        " Left
        <this_line> = <this_line> BIT-OR shifted_left.

        " Right
        <this_line> = <this_line> BIT-OR shifted_right.
      ENDDO.

      " Remove rock positions
      LOOP AT stepping ASSIGNING <this_line>.
        line = sy-tabix.
        ASSIGN plots[ line ] TO <tiles>.
        <this_line> = <this_line> BIT-AND <tiles>.
      ENDLOOP.

      " Step...
      LOOP AT reached ASSIGNING FIELD-SYMBOL(<reached_line>).
        line = sy-tabix.
        ASSIGN stepping[ line ] TO <this_line>.
        <reached_line> = <reached_line> BIT-XOR <this_line>.
      ENDLOOP.


    ENDDO.

    DATA(map_width) = xstrlen( reached[ 1 ] ) * 8.
    DATA set_count TYPE i.
    LOOP AT reached ASSIGNING <reached_line>.
      DO width TIMES.
        bit = sy-index.
        GET BIT bit OF <reached_line> INTO bit_value.
        set_count += bit_value.
      ENDDO.
    ENDLOOP.

    result = set_count.

  ENDMETHOD.

  METHOD part_2.

    " The input has no rocks on vertical or horizontal lines of start position.
    " And clear edges.
    " They will repeat fine and make a great diamond pattern.
    " Input map is an 131 x 131 square
    " Target steps is 26501365.
    " 26501365 mod 131 is 65
    " 65 steps is from start center, to edge of map.
    " 131 additonal steps will get us to the next edge (of the infinitely expanding map)
    " 26501365 - 65, divided by 131 is 202300
    " When map is fully "walked", the number of position toggles between two states
    " So for each map (that repeats), there is an odd version and an even number with different number of positions
    " The odd and even maps alternate.
    " We need the count for odd and even.
    " We are going to walk across 202300 square maps.
    " Center will be "odd"
    " Adjacent to center will be "even"
    " Adjacent to them will be "odd" again
    " And so on.
    " We will end up with (n+1)^2 odd and n^2 even
    " We need to get rid of some odd corners at the end. n+1 of each kind, to be exact.
    " We need to add some even corners at the end. n of each kind, to be exact.
    " Let's rock
    " I did not figure out all of this by myself :D

    DATA plots TYPE ty_map_tt.
    DATA reached TYPE ty_map_tt.
    DATA empty_line TYPE xstring.
    DATA shifted_left TYPE xstring.
    DATA shifted_right TYPE xstring.

    DATA(width) = strlen( input[ 1 ] ).
    DATA(height) = lines( input ).

    " Prepare maps from input
    DATA(hexwidth) =  ( width / 8 + 1 ) * 2.
    DATA(xbitwidth) = hexwidth * 4.
    empty_line = |{ '0' WIDTH = hexwidth PAD = '0' }|.
    shifted_left = empty_line.
    shifted_right = empty_line.


    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      APPEND empty_line TO reached.
      APPEND empty_line TO plots ASSIGNING FIELD-SYMBOL(<tiles>).

      DO width TIMES.
        DATA(offset) = sy-index - 1.
        DATA(char) = <line>+offset(1).
        IF char =  '.' OR char = 'S'.
          SET BIT offset + 1 OF <tiles> TO 1.
        ELSE.
          SET BIT offset + 1 OF <tiles> TO 0.
        ENDIF.
      ENDDO.

    ENDLOOP.


    " Set staring position
    FIND FIRST OCCURRENCE OF 'S' IN TABLE input MATCH LINE DATA(start_line) MATCH OFFSET DATA(start_position).
    ASSIGN reached[ start_line ] TO <tiles>.
    SET BIT start_position + 1 OF <tiles> TO 1.

    DATA(number_of_steps) = 26501365.
    DATA(number_of_squares) = number_of_steps / width.
    DATA(steps_to_odd) = width.
    DATA(steps_to_even) = width - 1.

    DO width * 2 TIMES.
      DATA(iteration) = sy-index.

      DATA(stepping) = reached.

      " First, step up and down
      DO height TIMES.
        DATA(line) = sy-index.
        ASSIGN stepping[ line ] TO FIELD-SYMBOL(<this_line>).

        " Down
        IF line > 1.
          ASSIGN reached[ line - 1 ] TO FIELD-SYMBOL(<line_above>).
          <this_line> = <this_line> BIT-OR <line_above>.
        ENDIF.

        " Up
        IF line < height.
          ASSIGN reached[ line + 1 ] TO <line_above>.
          <this_line> = <this_line> BIT-OR <line_above>.
        ENDIF.

        DATA(shift_left_right) = reached[ line ].

        DO xbitwidth TIMES.
          DATA(bit) = sy-index.
          GET BIT bit OF shift_left_right INTO DATA(bit_value).
          IF bit > 1.
            SET BIT bit - 1 OF shifted_left TO bit_value.
          ENDIF.
          IF bit < xbitwidth.
            SET BIT bit + 1 OF shifted_right TO bit_value.
          ENDIF.
        ENDDO.

        " Left
        <this_line> = <this_line> BIT-OR shifted_left.

        " Right
        <this_line> = <this_line> BIT-OR shifted_right.
      ENDDO.

      " Remove rock positions
      LOOP AT stepping ASSIGNING <this_line>.
        line = sy-tabix.
        ASSIGN plots[ line ] TO <tiles>.
        <this_line> = <this_line> BIT-AND <tiles>.
      ENDLOOP.

      " Step...
      LOOP AT reached ASSIGNING FIELD-SYMBOL(<reached_line>).
        line = sy-tabix.
        ASSIGN stepping[ line ] TO <this_line>.
        <reached_line> = <reached_line> BIT-XOR <this_line>.
      ENDLOOP.

      " Count at last odd and even...
      IF iteration = steps_to_odd.
        DATA(count_at_odd) = count_bits_set( map   = reached
                                             width = width ).
        DATA(reached_at_odd) = reached.
      ENDIF.
      IF iteration = steps_to_even.
        DATA(count_at_even) = count_bits_set( map  = reached
                                              width = width ).
        DATA(reached_at_even) = reached.
      ENDIF.

    ENDDO.

    " Remove center to count corners of both odd and even...
    DATA remove_center TYPE ty_map_tt.
    DATA(half_width) = width / 2.
    DATA(half_height) = height / 2.

    DATA(start_0) = half_width - 1.
    DATA(stop_0) = half_width - 1.

    DO height TIMES.                                                 " This is what's generated:
      line = sy-index.                                               " 11111011111
      APPEND empty_line TO remove_center ASSIGNING <tiles>.          " 11110001111
      DO width TIMES.                                                " 11100000111
        offset = sy-index - 1.                                       " 11000000011
        IF offset < start_0 OR offset > stop_0.                      " 10000000001
          SET BIT offset + 1 OF <tiles> TO 1.                        " 00000000000
        ELSE.                                                        " 10000000001
          SET BIT offset + 1 OF <tiles> TO 0.                        " 11000000011
        ENDIF.                                                       " 11100000111
      ENDDO.                                                         " 11110001111
      IF line < half_height.                                         " 11111011111
        start_0 -= 1.
        stop_0 += 1.
      ELSE.
        start_0 += 1.
        stop_0 -= 1.
      ENDIF.
    ENDDO.

    DATA(corners_odd) = reached_at_odd.
    LOOP AT corners_odd ASSIGNING <this_line>.
      line = sy-tabix.
      ASSIGN remove_center[ line ] TO <tiles>.
      <this_line> = <this_line> BIT-AND <tiles>.
    ENDLOOP.
    DATA(count_corners_odd) = count_bits_set( map   = corners_odd
                                              width = width ).

    DATA(corners_even) = reached_at_even.
    LOOP AT corners_even ASSIGNING <this_line>.
      line = sy-tabix.
      ASSIGN remove_center[ line ] TO <tiles>.
      <this_line> = <this_line> BIT-AND <tiles>.
    ENDLOOP.
    DATA(count_corners_even) = count_bits_set( map   = corners_even
                                               width = width ).

    DATA sum_of_it_all TYPE int8.
    DATA number_of_even         TYPE int8.
    DATA number_of_odd          TYPE int8.
    DATA number_of_even_corners TYPE int8.
    DATA number_of_odd_corners  TYPE int8.

    number_of_even         = number_of_squares * number_of_squares.
    number_of_odd          = ( number_of_squares + 1 ) * ( number_of_squares + 1 ).
    number_of_even_corners = number_of_squares.
    number_of_odd_corners  = number_of_squares + 1.

    sum_of_it_all  = number_of_odd * count_at_odd.
    sum_of_it_all += number_of_even * count_at_even.
    sum_of_it_all -= number_of_odd_corners * count_corners_odd.
    sum_of_it_all += number_of_even_corners * count_corners_even.

    result = sum_of_it_all.

  ENDMETHOD.

  METHOD debug_pretty_print.

    DATA(width) = xstrlen( map[ 1 ] ) * 8.

    LOOP AT map ASSIGNING FIELD-SYMBOL(<line>).
      IF sy-tabix > 1.
        result = |{ result }\n|.
      ENDIF.

      DO width TIMES.
        DATA(bit) = sy-index.
        GET BIT bit OF <line> INTO DATA(bit_value).
        result = |{ result }{ bit_value }|.

      ENDDO.

    ENDLOOP.

  ENDMETHOD.

  METHOD debug_map_mapped_on_input.

    DATA(width) = xstrlen( map[ 1 ] ) * 8.
    DATA(input_width) = strlen( input[ 1 ] ).

    LOOP AT map ASSIGNING FIELD-SYMBOL(<line>).
      DATA(line) = sy-tabix.
      DATA(input_line) = input[ line ].

      IF line > 1.
        result = |{ result }\n|.
      ENDIF.

      DO width TIMES.
        DATA(bit) = sy-index.
        GET BIT bit OF <line> INTO DATA(bit_value).
        IF bit_value = 1.
          result = |{ result }O|.
        ELSE.
          IF bit <= input_width.
            DATA(offset) = bit - 1.
            result = |{ result }{ input_line+offset(1) }|.
          ENDIF.
        ENDIF.

      ENDDO.

    ENDLOOP.

  ENDMETHOD.

  METHOD count_bits_set.

    DATA(map_width) = xstrlen( map[ 1 ] ) * 8.
    LOOP AT map ASSIGNING FIELD-SYMBOL(<map_line>).
      DO width TIMES.
        DATA(bit) = sy-index.
        GET BIT bit OF <map_line> INTO DATA(bit_value).
        result += bit_value.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
