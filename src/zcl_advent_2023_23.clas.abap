CLASS zcl_advent_2023_23 DEFINITION
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
      BEGIN OF ty_vertex,
        x0   TYPE i,
        y0   TYPE i,
        x1   TYPE i,
        y1   TYPE i,
        cost TYPE i,
      END OF ty_vertex.
    TYPES ty_vertices_tt TYPE HASHED TABLE OF ty_vertex WITH UNIQUE KEY x0 y0 x1 y1
                                                        WITH NON-UNIQUE SORTED KEY sorted_key COMPONENTS x0 y0.

    TYPES:
      BEGIN OF ty_node,
        x TYPE i,
        y TYPE i,
      END OF ty_node.
    TYPES ty_nodes_tt TYPE HASHED TABLE OF ty_node WITH UNIQUE KEY x y.

    TYPES:
      BEGIN OF ty_neighbor,
        x    TYPE i,
        y    TYPE i,
        cost TYPE i,
      END OF ty_neighbor.
    TYPES ty_neighbors_tt TYPE STANDARD TABLE OF ty_neighbor WITH EMPTY KEY.

    DATA vertices TYPE ty_vertices_tt.
    DATA visited  TYPE ty_nodes_tt.
    DATA max_cost_at_goal TYPE i.

    METHODS get_vertices_from_input
      IMPORTING
        input               TYPE zif_advent_2023=>ty_input_table
        slopes_go_both_ways TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)       TYPE ty_vertices_tt.

    METHODS transpose
      IMPORTING
        input         TYPE zif_advent_2023=>ty_input_table
      RETURNING
        VALUE(result) TYPE zif_advent_2023=>ty_input_table.

    METHODS dfs
      IMPORTING
        start         TYPE ty_node
        goal          TYPE ty_node
        cost          TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS get_neighbors
      IMPORTING
        node          TYPE ty_node
        previous_node TYPE ty_node
      RETURNING
        VALUE(result) TYPE ty_neighbors_tt.

    METHODS condense_vertices
      IMPORTING
        start         TYPE ty_node
      RETURNING
        VALUE(result) TYPE ty_vertices_tt.

ENDCLASS.



CLASS zcl_advent_2023_23 IMPLEMENTATION.

  METHOD part_1.

    vertices = get_vertices_from_input( input ).

    READ TABLE vertices WITH KEY y0 = 0 ASSIGNING FIELD-SYMBOL(<vertex>).
    DATA(start_x) = <vertex>-x0.
    DATA(start_y) = <vertex>-y0.
    READ TABLE vertices WITH KEY y1 = lines( input ) - 1 ASSIGNING <vertex>.
    DATA(goal_x) = <vertex>-x1.
    DATA(goal_y) = <vertex>-y1.

    dfs( start = VALUE #( x = start_x
                          y = start_y )
         goal  = VALUE #( x = goal_x
                          y = goal_y )
         cost  = 0 ).

    result = max_cost_at_goal.

  ENDMETHOD.

  METHOD part_2.

    vertices = get_vertices_from_input( input               = input
                                        slopes_go_both_ways = abap_true ).

    READ TABLE vertices WITH KEY y0 = 0 ASSIGNING FIELD-SYMBOL(<vertex>).
    DATA(start_x) = <vertex>-x0.
    DATA(start_y) = <vertex>-y0.
    READ TABLE vertices WITH KEY y1 = lines( input ) - 1 ASSIGNING <vertex>.
    DATA(goal_x) = <vertex>-x1.
    DATA(goal_y) = <vertex>-y1.

    CLEAR max_cost_at_goal.
    CLEAR visited.

    " Condensing is very unoptimized, but it works :)
    " I think it goes the same paths too many times, or something
    " Might get back to it later...

    vertices = condense_vertices( start = VALUE #( x = start_x
                                                   y = start_y ) ).

    dfs( start = VALUE #( x = start_x
                          y = start_y )
         goal  = VALUE #( x = goal_x
                          y = goal_y )
         cost  = 0 ).

    result = max_cost_at_goal.

    " 6422 in 320 seconds

  ENDMETHOD.

  METHOD get_vertices_from_input.

    " . path
    " # forest
    " > steep slope
    " v steep slope

    CLEAR vertices.

    FIND ALL OCCURRENCES OF REGEX '([\.v>]{2,})' IN TABLE input RESULTS DATA(horizontal).

    LOOP AT horizontal ASSIGNING FIELD-SYMBOL(<horiz>).
      DATA(line) = <horiz>-line.
      DATA(off) = <horiz>-offset.
      DATA(len) = <horiz>-length.

      DATA(horiz_line) = input[ line ].
      DATA(path) = horiz_line+off(len).

      FIND ALL OCCURRENCES OF '>' IN path RESULTS DATA(split_path).
      IF sy-subrc = 0.
        " Slopey path... Split and only one direction

        DATA(split_x0) = off.
        LOOP AT split_path ASSIGNING FIELD-SYMBOL(<split_path>).
          IF sy-tabix = lines( split_path ).
            DATA(split_x1) = off + len - 1.
          ELSE.
            split_x1 = off + <split_path>-offset + 1.
          ENDIF.
          INSERT VALUE #( x0   = split_x0
                          y0   = line - 1
                          x1   = split_x1
                          y1   = line - 1
                          cost = split_x1 - split_x0 ) INTO TABLE result ASSIGNING FIELD-SYMBOL(<split>).
          IF slopes_go_both_ways = abap_true.
            INSERT VALUE #( x0   = split_x1
                            y0   = line - 1
                            x1   = split_x0
                            y1   = line - 1
                            cost = split_x1 - split_x0 ) INTO TABLE result.
          ENDIF.
          split_x0 = <split>-x1.
        ENDLOOP.
      ELSE.
        " Straight path, both directions valid

        INSERT VALUE #( x0   = off
                        y0   = line - 1
                        x1   = off + len - 1
                        y1   = line - 1
                        cost = len - 1 ) INTO TABLE result.

        INSERT VALUE #( x0   = off + len - 1
                        y0   = line - 1
                        x1   = off
                        y1   = line - 1
                        cost = len - 1 ) INTO TABLE result.
      ENDIF.
    ENDLOOP.

    DATA(transposed) = transpose( input ).

    FIND ALL OCCURRENCES OF REGEX '([\.v>]{2,})' IN TABLE transposed RESULTS DATA(vertical).

    LOOP AT vertical ASSIGNING FIELD-SYMBOL(<vertical>).

      line = <vertical>-line.
      off = <vertical>-offset.
      len = <vertical>-length.

      DATA(vertical_line) = transposed[ line ].
      path = vertical_line+off(len).

      FIND ALL OCCURRENCES OF '>' IN path RESULTS split_path.
      IF sy-subrc = 0.
        " Slopey path... Split and only one direction

        DATA(split_y0) = off.
        LOOP AT split_path ASSIGNING <split_path>.
          IF sy-tabix = lines( split_path ).
            DATA(split_y1) = off + len - 1.
          ELSE.
            split_y1 = off + <split_path>-offset + 1.
          ENDIF.
          INSERT VALUE #( x0   = line - 1
                          y0   = split_y0
                          x1   = line - 1
                          y1   = split_y1
                          cost = split_y1 - split_y0 ) INTO TABLE result ASSIGNING <split>.
          IF slopes_go_both_ways = abap_true.
            INSERT VALUE #( x0   = line - 1
                            y0   = split_y1
                            x1   = line - 1
                            y1   = split_y0
                            cost = split_y1 - split_y0 ) INTO TABLE result.
          ENDIF.
          split_y0 = <split>-y1.
        ENDLOOP.
      ELSE.
        " Straight path, both directions valid

        INSERT VALUE #( x0   = line - 1
                        y0   = off
                        x1   = line - 1
                        y1   = off + len - 1
                        cost = len - 1 ) INTO TABLE result.

        INSERT VALUE #( x0   = line - 1
                        y0   = off + len - 1
                        x1   = line - 1
                        y1   = off
                        cost = len - 1 ) INTO TABLE result.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD transpose.

    DATA(width) = strlen( input[ 1 ] ).
    DATA(height) = lines( input ).

    DO width TIMES.
      DATA(offset) = sy-index - 1.
      APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<transposed>).
      LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
        <transposed> = |{ <transposed> }{ <line>+offset(1) }|.
      ENDLOOP.
    ENDDO.

    REPLACE ALL OCCURRENCES OF 'v' IN TABLE result WITH '>'.

  ENDMETHOD.

  METHOD dfs.

    IF start-x = goal-x AND
       start-y = goal-y.
      IF cost > max_cost_at_goal.
        max_cost_at_goal = cost.
      ENDIF.
      result = cost.
      RETURN.
    ENDIF.

    IF line_exists( visited[ x = start-x y = start-y ] ).
      RETURN.
    ELSE.
      INSERT VALUE #( x = start-x y = start-y ) INTO TABLE visited.
    ENDIF.

    DATA(neighbors) = get_neighbors( node = start
                                     previous_node = VALUE #( ) ).

    LOOP AT neighbors ASSIGNING FIELD-SYMBOL(<neighbor>).
      result = dfs( start = VALUE #( x = <neighbor>-x
                                     y = <neighbor>-y )
                    goal  = goal
                    cost = cost + <neighbor>-cost ).

    ENDLOOP.

    DELETE visited WHERE x = start-x AND y = start-y.

  ENDMETHOD.

  METHOD get_neighbors.

    LOOP AT vertices USING KEY sorted_key ASSIGNING FIELD-SYMBOL(<vertex>) WHERE x0 = node-x AND
                                                                                 y0 = node-y.
      IF <vertex>-x1 = previous_node-x AND <vertex>-y1 = previous_node-y.
        CONTINUE.
      ENDIF.

      INSERT VALUE #( x    = <vertex>-x1
                      y    = <vertex>-y1
                      cost = <vertex>-cost ) INTO TABLE result.

    ENDLOOP.

  ENDMETHOD.

  METHOD condense_vertices.

    TYPES:
      BEGIN OF ty_queue,
        node          TYPE ty_node,
        previous_node TYPE ty_node,
      END OF ty_queue.

    DATA queue TYPE STANDARD TABLE OF ty_queue WITH EMPTY KEY.
    DATA visited TYPE HASHED TABLE OF ty_queue WITH UNIQUE KEY node.
    DATA neighbors TYPE ty_neighbors_tt.
    DATA path TYPE ty_neighbors_tt.


    APPEND VALUE #( node          = start
                    previous_node = start ) TO queue.

    WHILE lines( queue ) > 0.
      DATA(current) = queue[ 1 ]-node.
      DATA(previous_node) = queue[ 1 ]-previous_node.
      DELETE queue INDEX 1.

      IF line_exists( visited[ node          = current
                               previous_node = previous_node ] ).
        CONTINUE.
      ELSE.
        INSERT VALUE #( node          = current
                        previous_node = previous_node ) INTO TABLE visited.
      ENDIF.

      DATA(current_cost) = 0.

      DATA(intersection) = get_neighbors( node          = VALUE #( x = current-x y = current-y )
                                          previous_node = previous_node ).
      IF lines( intersection ) = 0.
        CONTINUE.
      ENDIF.

      LOOP AT intersection ASSIGNING FIELD-SYMBOL(<intersection>).
        neighbors = VALUE #( ( <intersection> ) ).
        previous_node = current.
        current_cost = 0.
        DO.
          APPEND <intersection> TO path.
          DATA(this_neighbor) = neighbors[ 1 ].

          current_cost += this_neighbor-cost.

          neighbors = get_neighbors( node          = VALUE #( x = this_neighbor-x y = this_neighbor-y )
                                     previous_node = previous_node ).
          IF lines( neighbors ) = 0.
            EXIT.
          ENDIF.
          IF lines( neighbors ) > 1.
            EXIT.
          ENDIF.
          previous_node = CORRESPONDING #( this_neighbor ).

        ENDDO.

        IF line_exists( result[ x0 = current-x
                        y0   = current-y
                        x1   = this_neighbor-x
                        y1   = this_neighbor-y ] ).
          CONTINUE.
        ELSE.

          INSERT VALUE #( x0   = current-x
                          y0   = current-y
                          x1   = this_neighbor-x
                          y1   = this_neighbor-y
                          cost = current_cost ) INTO TABLE result.
          INSERT VALUE #( x0   = this_neighbor-x
                          y0   = this_neighbor-y
                          x1   = current-x
                          y1   = current-y
                          cost = current_cost ) INTO TABLE result.

          IF lines( neighbors ) > 0.
            APPEND VALUE #( node          = this_neighbor
                            previous_node = previous_node ) TO queue.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDWHILE.


  ENDMETHOD.

ENDCLASS.
