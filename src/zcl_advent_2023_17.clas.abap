CLASS zcl_advent_2023_17 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_last_three TYPE c LENGTH 3.

    TYPES:
      BEGIN OF ty_node,
        x    TYPE i,
        y    TYPE i,
        loss TYPE i,
      END OF ty_node.
    TYPES ty_nodes_tt TYPE HASHED TABLE OF ty_node WITH UNIQUE KEY x y.

    TYPES:
      BEGIN OF ty_dist_node,
        x          TYPE i,
        y          TYPE i,
        last_three TYPE ty_last_three,
        loss       TYPE i,
      END OF ty_dist_node.
    TYPES ty_dist_nodes_tt TYPE HASHED TABLE OF ty_dist_node WITH UNIQUE KEY x y last_three.

    TYPES:
      BEGIN OF ty_vertex,
        from_x TYPE i,
        from_y TYPE i,
        to_x   TYPE i,
        to_y   TYPE i,
        loss   TYPE i,
      END OF ty_vertex.
*    TYPES ty_vertexes_tt TYPE sorted TABLE OF ty_vertex WITH non-UNIQUE KEY from_x from_y. " to_x to_y.
    TYPES ty_vertexes_tt TYPE hashed TABLE OF ty_vertex WITH UNIQUE KEY from_x from_y to_x to_y.

    DATA map_width TYPE i.
    DATA map_height TYPE i.

    DATA nodes      TYPE ty_nodes_tt.
    DATA vertexes   TYPE ty_vertexes_tt.
    DATA dists      TYPE ty_dist_nodes_tt.
    DATA path_nodes TYPE ty_nodes_tt.
    DATA least_loss TYPE i.

    METHODS parse_input_into_nodes
      IMPORTING
        input TYPE zif_advent_2023=>ty_input_table.

    METHODS figure_out_node_vertexes.

    METHODS visit_neighbors
      IMPORTING
        loss            TYPE i
        x               TYPE i
        y               TYPE i
        direction       TYPE c
        path            TYPE string
      RETURNING
        VALUE(carry_on) TYPE abap_bool.

ENDCLASS.



CLASS zcl_advent_2023_17 IMPLEMENTATION.

  METHOD part_1.

    map_width = strlen( input[ 1 ] ).
    map_height = lines( input ).

    parse_input_into_nodes( input ).

    figure_out_node_vertexes( ).

    least_loss = cl_abap_math=>max_int4.

    " Zero loss at initial node
    nodes[ x = 0 y = 0 ]-loss = 0.

    visit_neighbors( loss = 0
                     x    = 0
                     y    = 0
                     direction = ''
                     path = '' ).

    result = least_loss.

  ENDMETHOD.

  METHOD part_2.

    result = |todo|.

  ENDMETHOD.

  METHOD parse_input_into_nodes.

    DATA x TYPE i.
    DATA y TYPE i.
    DATA loss TYPE i.

    IF lines( nodes ) > 0.
      RETURN.
    ENDIF.

    DATA(width) = strlen( input[ 1 ] ).

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      y = sy-tabix - 1.
      DO width TIMES.
        x = sy-index - 1.
        loss = <line>+x(1).
        INSERT VALUE #( x = x y = y loss = loss ) INTO TABLE nodes.
      ENDDO.

    ENDLOOP.

  ENDMETHOD.

  METHOD figure_out_node_vertexes.

    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<node>).
      DATA(this_loss) = <node>-loss.

      DATA(to_x) = <node>-x + 1.
      IF to_x < map_width.
        DATA(that_loss) = nodes[ x = to_x y = <node>-y ]-loss.

        INSERT VALUE #( from_x = <node>-x
                        from_y = <node>-y
                        to_x   = to_x
                        to_y   = <node>-y
                        loss   = that_loss ) INTO TABLE vertexes.

        INSERT VALUE #( from_x = to_x
                        from_y = <node>-y
                        to_x   = <node>-x
                        to_y   = <node>-y
                        loss   = this_loss ) INTO TABLE vertexes.
      ENDIF.

      DATA(to_y) = <node>-y + 1.
      IF to_y < map_height.
        that_loss = nodes[ x = <node>-x y = to_y ]-loss.

        INSERT VALUE #( from_x = <node>-x
                        from_y = <node>-y
                        to_x   = <node>-x
                        to_y   = to_y
                        loss   = that_loss ) INTO TABLE vertexes.

        INSERT VALUE #( from_x = <node>-x
                        from_y = to_y
                        to_x   = <node>-x
                        to_y   = <node>-y
                        loss   = this_loss ) INTO TABLE vertexes.
      ENDIF.


    ENDLOOP.
  ENDMETHOD.

  METHOD visit_neighbors.

    DATA some_loss TYPE i.
    DATA last_three TYPE ty_last_three.
    DATA last_like TYPE ty_last_three.

    IF x = 12 AND y = 7.
      DATA(pelle) = 1.
    ENDIF.

    IF path = 'RRDRRRURRRDDRR'."DDRDDDR'."DDDLDDR'.
      DATA(olle) = 8.
    ENDIF.

    IF x = map_width - 1 AND y = map_height - 1.
      DATA(kalle) = 3.
    ENDIF.

    IF loss > least_loss.
      RETURN.
    ENDIF.

    DATA(path_len) = strlen( path ).
    IF path_len > 2.
      DATA(offset) = path_len - 3.
      last_three = path+offset(3).
*      IF last_three+1(1) = last_three+2(1).
*        IF last_three+0(1) = last_three+2(1).
*          last_like = last_three.
*        ELSE.
*          last_like = last_three+1(2).
*        ENDIF.
*      ELSE.
*        last_like = last_three+2(1).
*      ENDIF.
    ENDIF.

    READ TABLE dists WITH KEY x          = x
                              y          = y
                              last_three = last_three ASSIGNING FIELD-SYMBOL(<dist>).
    IF sy-subrc = 0.
      IF loss <= <dist>-loss.
        <dist>-loss = loss.
        IF x = map_width - 1 AND y = map_height - 1.
          least_loss = loss.
        ENDIF.
      ELSE.
        RETURN.
      ENDIF.
    ELSE.
      INSERT VALUE #( x = x
                      y = y
                      last_three = last_three
                      loss = loss ) INTO TABLE dists.
    ENDIF.

    IF line_exists( path_nodes[ x = x y = y ] ).
      RETURN.
    ELSE.
      INSERT VALUE #( x = x y = y loss = loss ) INTO TABLE path_nodes.
    ENDIF.

    IF x = map_width - 1 AND y = map_height - 1.
      RETURN.
    ENDIF.

*
*    SELECT * FROM @vertexes AS vertexes
*      WHERE from_x = @x AND from_y = @y
*      INTO TABLE @DATA(neighbors).
*    SORT neighbors BY loss.

    " RRDRRR URRRDD RRDDRDDD RDDDLDDR

*    LOOP AT neighbors ASSIGNING FIELD-SYMBOL(<neighbor>).
*loop at vertexes assigning field-symbol(<neighbor>) where from_x = x and from_y = y .

*      IF <neighbor>-to_x = x + 1.

    " Right
    IF x < map_width - 1 AND
       direction <> 'L' AND
       last_three <> 'RRR'.

      read table vertexes assigning field-symbol(<neighbor>) with key from_x = x
                                                                      from_y = y
                                                                      to_x   = x + 1
                                                                      to_y   = y    .

*      READ TABLE neighbors WITH KEY to_x = x + 1
*                                    to_y = y ASSIGNING FIELD-SYMBOL(<neighbor>).
      some_loss = loss + <neighbor>-loss.
      visit_neighbors( loss = some_loss
                       x    = x + 1
                       y    = y
                       direction = 'R'
                       path = |{ path }R| ).

    ENDIF.

*      ELSEIF <neighbor>-to_y = y + 1.

    " Down
    IF y < map_height - 1 AND
       direction <> 'U' AND
       last_three <> 'DDD'.

      read table vertexes assigning <neighbor> with key from_x = x
                                                        from_y = y
                                                        to_x   = x
                                                        to_y   = y + 1.
*      READ TABLE neighbors WITH KEY to_x = x
*                                    to_y = y + 1 ASSIGNING <neighbor>.
      some_loss = loss + <neighbor>-loss.
      visit_neighbors( loss = some_loss
                       x    = x
                       y    = y + 1
                       direction = 'D'
                       path = |{ path }D| ).

    ENDIF.

*      ELSEIF <neighbor>-to_x = x - 1.

    " Left
    IF x > 0 AND
       direction <> 'R' AND
       last_three <> 'LLL'.
      read table vertexes assigning <neighbor> with key from_x = x
                                                        from_y = y
                                                        to_x   = x - 1
                                                        to_y   = y.
*      READ TABLE neighbors WITH KEY to_x = x - 1
*                                    to_y = y ASSIGNING <neighbor>.
      some_loss = loss + <neighbor>-loss.
      visit_neighbors( loss = some_loss
                       x    = x - 1
                       y    = y
                       direction = 'L'
                       path = |{ path }L| ).

    ENDIF.

*      ELSEIF <neighbor>-to_y = y - 1.

    " Up
    IF y > 0 AND
       direction <> 'D' AND
       last_three <> 'UUU'.
      read table vertexes assigning <neighbor> with key from_x = x
                                                        from_y = y
                                                        to_x   = x
                                                        to_y   = y - 1.
*      READ TABLE neighbors WITH KEY to_x = x
*                                    to_y = y - 1 ASSIGNING <neighbor>.
      some_loss = loss + <neighbor>-loss.
      visit_neighbors( loss = some_loss
                       x    = x
                       y    = y - 1
                       direction = 'U'
                       path = |{ path }U| ).

    ENDIF.

*      ENDIF.
*
*    ENDLOOP.

    DELETE path_nodes WHERE x = x AND y = y.




  ENDMETHOD.

ENDCLASS.
