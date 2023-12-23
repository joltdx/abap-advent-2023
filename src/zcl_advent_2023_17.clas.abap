CLASS zcl_advent_2023_17 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

    METHODS part_1_slow IMPORTING
                          input
                            TYPE zif_advent_2023=>ty_input_table
                        RETURNING
                          VALUE(result) TYPE string.

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
    TYPES ty_vertices_tt TYPE HASHED TABLE OF ty_vertex WITH UNIQUE KEY from_x from_y to_x to_y.

    DATA map_width TYPE i.
    DATA map_height TYPE i.
    DATA max_x TYPE i.
    DATA max_y TYPE i.

    DATA nodes      TYPE ty_nodes_tt.
    DATA vertices   TYPE ty_vertices_tt.
    DATA dists      TYPE ty_dist_nodes_tt.
    DATA path_nodes TYPE ty_nodes_tt.
    DATA least_loss TYPE i.

    METHODS parse_input_into_nodes
      IMPORTING
        input TYPE zif_advent_2023=>ty_input_table.

    METHODS figure_out_node_vertices.

    METHODS visit_neighbors
      IMPORTING
        loss      TYPE i
        x         TYPE i
        y         TYPE i
        direction TYPE c
        path      TYPE string.

    METHODS a_star
      IMPORTING
        from_x TYPE i
        from_y TYPE i
        to_x   TYPE i
        to_y   TYPE i.

    METHODS a_star_max_three
      IMPORTING
        from_x        TYPE i
        from_y        TYPE i
        to_x          TYPE i
        to_y          TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS a_star_dynamic
      IMPORTING
        from_x        TYPE i
        from_y        TYPE i
        to_x          TYPE i
        to_y          TYPE i
        min_steps     TYPE i
        max_steps     TYPE i
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.



CLASS zcl_advent_2023_17 IMPLEMENTATION.

  METHOD part_1.

    map_width = strlen( input[ 1 ] ).
    map_height = lines( input ).
    max_x = map_width - 1.
    max_y = map_height - 1.

    parse_input_into_nodes( input ).

    result = a_star_max_three( from_x = 0
                               from_y = 0
                               to_x   = max_x
                               to_y   = max_y ).

  ENDMETHOD.

  METHOD part_1_slow.

    " THIS WORKED TOO, BUT MUCH TO SLOW...
    " Keeping it just because...

    map_width = strlen( input[ 1 ] ).
    map_height = lines( input ).
    max_x = map_width - 1.
    max_y = map_height - 1.

    parse_input_into_nodes( input ).

    figure_out_node_vertices( ).

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

    map_width = strlen( input[ 1 ] ).
    map_height = lines( input ).
    max_x = map_width - 1.
    max_y = map_height - 1.

    parse_input_into_nodes( input ).

    result = a_star_dynamic( from_x = 0
                             from_y = 0
                             to_x   = max_x
                             to_y   = max_y
                             min_steps = 4
                             max_steps = 10 ).

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

  METHOD figure_out_node_vertices.

    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<node>).
      DATA(this_loss) = <node>-loss.

      DATA(to_x) = <node>-x + 1.
      IF to_x < map_width.
        DATA(that_loss) = nodes[ x = to_x y = <node>-y ]-loss.

        INSERT VALUE #( from_x = <node>-x
                        from_y = <node>-y
                        to_x   = to_x
                        to_y   = <node>-y
                        loss   = that_loss ) INTO TABLE vertices.

        INSERT VALUE #( from_x = to_x
                        from_y = <node>-y
                        to_x   = <node>-x
                        to_y   = <node>-y
                        loss   = this_loss ) INTO TABLE vertices.
      ENDIF.

      DATA(to_y) = <node>-y + 1.
      IF to_y < map_height.
        that_loss = nodes[ x = <node>-x y = to_y ]-loss.

        INSERT VALUE #( from_x = <node>-x
                        from_y = <node>-y
                        to_x   = <node>-x
                        to_y   = to_y
                        loss   = that_loss ) INTO TABLE vertices.

        INSERT VALUE #( from_x = <node>-x
                        from_y = to_y
                        to_x   = <node>-x
                        to_y   = <node>-y
                        loss   = this_loss ) INTO TABLE vertices.
      ENDIF.


    ENDLOOP.
  ENDMETHOD.

  METHOD visit_neighbors.

    DATA some_loss TYPE i.
    DATA last_three TYPE ty_last_three.
    DATA last_like TYPE ty_last_three.

    FIELD-SYMBOLS <neighbor> LIKE LINE OF vertices.

    IF loss > least_loss.
      RETURN.
    ENDIF.

    DATA(path_len) = strlen( path ).
    IF path_len > 2.
      DATA(offset) = path_len - 3.
      last_three = path+offset(3).
    ENDIF.

    READ TABLE dists WITH KEY x          = x
                              y          = y
                              last_three = last_three ASSIGNING FIELD-SYMBOL(<dist>).
    IF sy-subrc = 0.
      IF loss < <dist>-loss.
        <dist>-loss = loss.
        IF x = max_x AND y = max_y.
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

    IF x = max_x AND y = max_y.
      RETURN.
    ENDIF.

    " Right?
    IF x < max_x AND
       direction <> 'L' AND
       last_three <> 'RRR'.

      READ TABLE vertices ASSIGNING <neighbor> WITH KEY from_x = x
                                                        from_y = y
                                                        to_x   = x + 1
                                                        to_y   = y.

      some_loss = loss + <neighbor>-loss.
      visit_neighbors( loss      = some_loss
                       x         = x + 1
                       y         = y
                       direction = 'R'
                       path      = path && 'R' ). "|{ path }R| ).

    ENDIF.

    " Down?
    IF y < max_y AND
       direction <> 'U' AND
       last_three <> 'DDD'.

      READ TABLE vertices ASSIGNING <neighbor> WITH KEY from_x = x
                                                        from_y = y
                                                        to_x   = x
                                                        to_y   = y + 1.
      some_loss = loss + <neighbor>-loss.
      visit_neighbors( loss      = some_loss
                       x         = x
                       y         = y + 1
                       direction = 'D'
                       path      = path && 'D' ). "|{ path }D| ).

    ENDIF.

    " Left?
    IF x > 0 AND
       direction <> 'R' AND
       last_three <> 'LLL'.
      READ TABLE vertices ASSIGNING <neighbor> WITH KEY from_x = x
                                                        from_y = y
                                                        to_x   = x - 1
                                                        to_y   = y.
      some_loss = loss + <neighbor>-loss.
      visit_neighbors( loss      = some_loss
                       x         = x - 1
                       y         = y
                       direction = 'L'
                       path      = path && 'L' ). "|{ path }L| ).

    ENDIF.

    " Up?
    IF y > 0 AND
       direction <> 'D' AND
       last_three <> 'UUU'.
      READ TABLE vertices ASSIGNING <neighbor> WITH KEY from_x = x
                                                        from_y = y
                                                        to_x   = x
                                                        to_y   = y - 1.
      some_loss = loss + <neighbor>-loss.
      visit_neighbors( loss      = some_loss
                       x         = x
                       y         = y - 1
                       direction = 'U'
                       path      = path && 'U' )."|{ path }U| ).

    ENDIF.


    DELETE path_nodes WHERE x = x AND y = y.

  ENDMETHOD.

  METHOD a_star.

    " https://en.wikipedia.org/wiki/A*_search_algorithm

    DATA open_set TYPE SORTED TABLE OF ty_node WITH NON-UNIQUE KEY loss.
    DATA came_from TYPE HASHED TABLE OF ty_vertex WITH UNIQUE KEY to_x to_y.
    DATA g_score TYPE HASHED TABLE OF ty_node WITH UNIQUE KEY x y.
    DATA f_score TYPE HASHED TABLE OF ty_node WITH UNIQUE KEY x y.
    DATA previous_direction TYPE i.
    DATA direction_streak TYPE i.

    INSERT nodes[ x = from_x y = from_y ] INTO TABLE open_set.

    g_score = nodes.
    MODIFY g_score FROM VALUE #( loss = cl_abap_math=>max_int4 ) TRANSPORTING loss WHERE loss IS NOT INITIAL.
    g_score[ x = from_x y = from_y ]-loss = 0.

    f_score = nodes.
    MODIFY f_score FROM VALUE #( loss = cl_abap_math=>max_int4 ) TRANSPORTING loss WHERE loss IS NOT INITIAL.
    f_score[ x = from_x y = from_y ]-loss = cl_abap_math=>max_int4.     " Not sure what to set this to???

    WHILE lines( open_set ) > 0.
      READ TABLE open_set INDEX 1 INTO DATA(current).

      IF current-x = to_x AND current-y = to_y.
        " We're done

        " Reconstruct path

        EXIT.
      ENDIF.

      DELETE open_set INDEX 1.

      " Neighbors
      DO 4 TIMES.
        DATA(direction) = sy-index.

        CASE direction.
          WHEN 1. " Up
            IF current-y > 0.
              ASSIGN nodes[ x = current-x y = current-y - 1 ] TO FIELD-SYMBOL(<neighbor>).
            ELSE.
              CONTINUE.
            ENDIF.

          WHEN 2. " Down
            IF current-y < max_y.
              ASSIGN nodes[ x = current-x y = current-y + 1 ] TO <neighbor>.
            ELSE.
              CONTINUE.
            ENDIF.

          WHEN 3. " Left
            IF current-x > 0.
              ASSIGN nodes[ x = current-x - 1 y = current-y ] TO <neighbor>.
            ELSE.
              CONTINUE.
            ENDIF.

          WHEN 4. " Right
            IF current-x < max_x.
              ASSIGN nodes[ x = current-x + 1 y = current-y ] TO <neighbor>.
            ELSE.
              CONTINUE.
            ENDIF.

        ENDCASE.

        DATA(tentative_score) = g_score[ x = current-x y = current-y ]-loss + <neighbor>-loss.

        IF tentative_score < g_score[ x = <neighbor>-x y = <neighbor>-y ]-loss.
          READ TABLE came_from WITH KEY to_x = <neighbor>-x to_y = <neighbor>-y ASSIGNING FIELD-SYMBOL(<came_from>).
          IF sy-subrc <> 0.
            INSERT VALUE #( to_x   = <neighbor>-x
                            to_y   = <neighbor>-y ) INTO TABLE came_from ASSIGNING <came_from>.
          ENDIF.
          <came_from>-from_x = current-x.
          <came_from>-from_y = current-y.

          g_score[ x = <neighbor>-x y = <neighbor>-y ]-loss = tentative_score.
          f_score[ x = <neighbor>-x y = <neighbor>-y ]-loss = tentative_score + <neighbor>-loss.   " I have no idea about this one...
          IF NOT line_exists( open_set[ x = <neighbor>-x y = <neighbor>-y ] ).
            INSERT VALUE #( x = <neighbor>-x y = <neighbor>-y loss = tentative_score ) INTO TABLE open_set.
          ENDIF.

        ENDIF.

      ENDDO.

      " For each neighbor of current...
      " ...
      " ...

    ENDWHILE.

    DATA(nisse) = 3.

  ENDMETHOD.

  METHOD a_star_max_three.

    " https://en.wikipedia.org/wiki/A*_search_algorithm

    TYPES:
      BEGIN OF ty_node_with_direction,
        x         TYPE i,
        y         TYPE i,
        direction TYPE c LENGTH 1,
        loss      TYPE i,
      END OF ty_node_with_direction.
    TYPES ty_nodes_with_direction_tt TYPE HASHED TABLE OF ty_node_with_direction WITH UNIQUE KEY x y direction.

    TYPES:
      BEGIN OF ty_came_from,
        from_x         TYPE i,
        from_y         TYPE i,
        from_direction TYPE c LENGTH 1,
        to_x           TYPE i,
        to_y           TYPE i,
        to_direction   TYPE c LENGTH 1,
      END OF ty_came_from.


    DATA open_set TYPE SORTED TABLE OF ty_node_with_direction WITH NON-UNIQUE KEY loss.
    DATA came_from TYPE HASHED TABLE OF ty_came_from WITH UNIQUE KEY to_x to_y to_direction.
    DATA g_score TYPE HASHED TABLE OF ty_node_with_direction WITH UNIQUE KEY x y direction.
    DATA neighbor_loss TYPE i.
    DATA this_direction TYPE c LENGTH 1.

    INSERT VALUE #( x = from_x y = from_y direction = 'D' loss = 0 ) INTO TABLE open_set.
    INSERT VALUE #( x = from_x y = from_y direction = 'R' loss = 0 ) INTO TABLE open_set.

    INSERT VALUE #( x = from_x y = from_y direction = 'D' loss = 0 ) INTO TABLE g_score.
    INSERT VALUE #( x = from_x y = from_y direction = 'R' loss = 0 ) INTO TABLE g_score.


    WHILE lines( open_set ) > 0.
      READ TABLE open_set INDEX 1 INTO DATA(current).

      IF current-x = to_x AND current-y = to_y.
        " We're done

        " Reconstruct path

*        EXIT.
      ENDIF.

      DELETE open_set INDEX 1.

      " Neighbors
      DO 12 TIMES.
        DATA(direction) = sy-index.


        CASE direction.
          WHEN 1. " Up 1
            IF current-direction <> 'U' AND current-direction <> 'D' AND current-y > 0.
              ASSIGN nodes[ x = current-x y = current-y - 1 ] TO FIELD-SYMBOL(<neighbor>).
              neighbor_loss = <neighbor>-loss.
              this_direction = 'U'.
            ELSE.
              CONTINUE.
            ENDIF.

          WHEN 2. " Up 2
            IF current-direction <> 'U' AND current-direction <> 'D' AND current-y > 1.
              ASSIGN nodes[ x = current-x y = current-y - 2 ] TO <neighbor>.
              neighbor_loss += <neighbor>-loss.
              this_direction = 'U'.
            ELSE.
*              previous_direction = 'U'.
              CONTINUE.
            ENDIF.

          WHEN 3. " Up 3
            IF current-direction <> 'U' AND current-direction <> 'D' AND current-y > 2.
              ASSIGN nodes[ x = current-x y = current-y - 3 ] TO <neighbor>.
              neighbor_loss += <neighbor>-loss.
              this_direction = 'U'.
            ELSE.
*              previous_direction = 'U'.
              CONTINUE.
            ENDIF.

          WHEN 4. " Down 1
            IF current-direction <> 'D' AND current-direction <> 'U' AND current-y < max_y.
              ASSIGN nodes[ x = current-x y = current-y + 1 ] TO <neighbor>.
              neighbor_loss = <neighbor>-loss.
              this_direction = 'D'.
            ELSE.
              CONTINUE.
            ENDIF.

          WHEN 5. " Down 2
            IF current-direction <> 'D' AND current-direction <> 'U' AND current-y < max_y + 1.
              ASSIGN nodes[ x = current-x y = current-y + 2 ] TO <neighbor>.
              neighbor_loss += <neighbor>-loss.
              this_direction = 'D'.
            ELSE.
*              previous_direction = 'D'.
              CONTINUE.
            ENDIF.

          WHEN 6. " Down 3
            IF current-direction <> 'D' AND current-direction <> 'U' AND current-y < max_y + 2.
              ASSIGN nodes[ x = current-x y = current-y + 3 ] TO <neighbor>.
              neighbor_loss += <neighbor>-loss.
              this_direction = 'D'.
            ELSE.
*              previous_direction = 'D'.
              CONTINUE.
            ENDIF.

          WHEN 7. " Left 1
            IF current-direction <> 'L' AND current-direction <> 'R' AND current-x > 0.
              ASSIGN nodes[ x = current-x - 1 y = current-y ] TO <neighbor>.
              neighbor_loss = <neighbor>-loss.
              this_direction = 'L'.
            ELSE.
              CONTINUE.
            ENDIF.

          WHEN 8. " Left 2
            IF current-direction <> 'L' AND current-direction <> 'R' AND current-x > 1.
              ASSIGN nodes[ x = current-x - 2 y = current-y ] TO <neighbor>.
              neighbor_loss += <neighbor>-loss.
              this_direction = 'L'.
            ELSE.
              CONTINUE.
            ENDIF.

          WHEN 9. " Left 3
            IF current-direction <> 'L' AND current-direction <> 'R' AND current-x > 2.
              ASSIGN nodes[ x = current-x - 3 y = current-y ] TO <neighbor>.
              neighbor_loss += <neighbor>-loss.
              this_direction = 'L'.
            ELSE.
              CONTINUE.
            ENDIF.

          WHEN 10. " Right 1
            IF current-direction <> 'R' AND current-direction <> 'L' AND current-x < max_x.
              ASSIGN nodes[ x = current-x + 1 y = current-y ] TO <neighbor>.
              neighbor_loss = <neighbor>-loss.
              this_direction = 'R'.
            ELSE.
              CONTINUE.
            ENDIF.

          WHEN 11. " Right 2
            IF current-direction <> 'R' AND current-direction <> 'L' AND current-x < max_x + 1.
              ASSIGN nodes[ x = current-x + 2 y = current-y ] TO <neighbor>.
              neighbor_loss += <neighbor>-loss.
              this_direction = 'R'.
            ELSE.
              CONTINUE.
            ENDIF.

          WHEN 12. " Right 3
            IF current-direction <> 'R' AND current-direction <> 'L' AND current-x < max_x + 2.
              ASSIGN nodes[ x = current-x + 3 y = current-y ] TO <neighbor>.
              neighbor_loss += <neighbor>-loss.
              this_direction = 'R'.
            ELSE.
              CONTINUE.
            ENDIF.

        ENDCASE.

        READ TABLE g_score WITH KEY x = current-x y = current-y direction = current-direction ASSIGNING FIELD-SYMBOL(<current_g_score>).
        IF sy-subrc <> 0.
          INSERT VALUE #( x = current-x y = current-y direction = current-direction loss = cl_abap_math=>max_int4 ) INTO TABLE g_score ASSIGNING <current_g_score>.
        ENDIF.

        DATA(tentative_score) = <current_g_score>-loss + neighbor_loss.

        READ TABLE g_score WITH KEY x = <neighbor>-x y = <neighbor>-y direction = this_direction ASSIGNING FIELD-SYMBOL(<neighbor_g_score>).
        IF sy-subrc <> 0.
          INSERT VALUE #( x = <neighbor>-x y = <neighbor>-y direction = this_direction loss = cl_abap_math=>max_int4 ) INTO TABLE g_score ASSIGNING <neighbor_g_score>.
        ENDIF.

        IF tentative_score < <neighbor_g_score>-loss.
          READ TABLE came_from WITH KEY to_x = <neighbor>-x to_y = <neighbor>-y to_direction = this_direction ASSIGNING FIELD-SYMBOL(<came_from>).
          IF sy-subrc <> 0.
            INSERT VALUE #( to_x   = <neighbor>-x
                            to_y   = <neighbor>-y
                            to_direction = this_direction ) INTO TABLE came_from ASSIGNING <came_from>.
          ENDIF.
          <came_from>-from_x         = current-x.
          <came_from>-from_y         = current-y.
          <came_from>-from_direction = current-direction.

          <neighbor_g_score>-loss = tentative_score.
          IF NOT line_exists( open_set[ x = <neighbor>-x y = <neighbor>-y direction = this_direction ] ).
            INSERT VALUE #( x = <neighbor>-x y = <neighbor>-y direction = this_direction loss = tentative_score ) INTO TABLE open_set.
          ENDIF.

        ENDIF.

      ENDDO.


    ENDWHILE.

    DATA(loss_r) = g_score[ x = to_x y = to_y direction = 'R' ]-loss.
    DATA(loss_d) = g_score[ x = to_x y = to_y direction = 'D' ]-loss.
    IF loss_r < loss_d.
      result = loss_r.
    ELSE.
      result = loss_d.
    ENDIF.

  ENDMETHOD.


  METHOD a_star_dynamic.

    " Could be tidied up a bit but it's basically a A* thing but we want to find longest path, so we need test all paths
    " (So it's NOT an A* then :)
    " And there's a minimum and maximum amount of steps to take...

    " https://en.wikipedia.org/wiki/A*_search_algorithm

    TYPES:
      BEGIN OF ty_node_with_direction,
        x         TYPE i,
        y         TYPE i,
        direction TYPE c LENGTH 1,
        loss      TYPE i,
      END OF ty_node_with_direction.
    TYPES ty_nodes_with_direction_tt TYPE HASHED TABLE OF ty_node_with_direction WITH UNIQUE KEY x y direction.

    TYPES:
      BEGIN OF ty_came_from,
        from_x         TYPE i,
        from_y         TYPE i,
        from_direction TYPE c LENGTH 1,
        to_x           TYPE i,
        to_y           TYPE i,
        to_direction   TYPE c LENGTH 1,
      END OF ty_came_from.


    DATA open_set TYPE SORTED TABLE OF ty_node_with_direction WITH NON-UNIQUE KEY loss.
    DATA came_from TYPE HASHED TABLE OF ty_came_from WITH UNIQUE KEY to_x to_y to_direction.
    DATA g_score TYPE HASHED TABLE OF ty_node_with_direction WITH UNIQUE KEY x y direction.
    DATA neighbor_loss TYPE i.
    DATA this_direction TYPE c LENGTH 1.

    DATA x TYPE i.
    DATA y TYPE i.

    INSERT VALUE #( x = from_x y = from_y direction = 'D' loss = 0 ) INTO TABLE open_set.
    INSERT VALUE #( x = from_x y = from_y direction = 'R' loss = 0 ) INTO TABLE open_set.

    INSERT VALUE #( x = from_x y = from_y direction = 'D' loss = 0 ) INTO TABLE g_score.
    INSERT VALUE #( x = from_x y = from_y direction = 'R' loss = 0 ) INTO TABLE g_score.


    WHILE lines( open_set ) > 0.
      READ TABLE open_set INDEX 1 INTO DATA(current).

      DELETE open_set INDEX 1.

      " Neighbors
      DATA(each_direction_count) = max_steps - min_steps + 1.
      DATA(combinations) = each_direction_count * 4.
      DO 4 TIMES.
        DATA(direction) = sy-index.
        CLEAR neighbor_loss.

        " Mandatory pre-steps
        CASE direction.
          WHEN 1. " Up
            this_direction = 'U'.
            DO min_steps - 1 TIMES.
              DATA(step) = sy-index.
              ASSIGN nodes[ x = current-x
                            y = current-y - step ] TO FIELD-SYMBOL(<neighbor>).
              IF sy-subrc = 0.
                neighbor_loss += <neighbor>-loss.
              ENDIF.
            ENDDO.

          WHEN 2. " Down
            this_direction = 'D'.
            DO min_steps - 1 TIMES.
              step = sy-index.
              ASSIGN nodes[ x = current-x
                            y = current-y + step ] TO <neighbor>.
              IF sy-subrc = 0.
                neighbor_loss += <neighbor>-loss.
              ENDIF.
            ENDDO.

          WHEN 3. " Left
            this_direction = 'L'.
            DO min_steps - 1 TIMES.
              step = sy-index.
              ASSIGN nodes[ x = current-x - step
                            y = current-y ] TO <neighbor>.
              IF sy-subrc = 0.
                neighbor_loss += <neighbor>-loss.
              ENDIF.
            ENDDO.

          WHEN 4. " Right
            this_direction = 'R'.

            DO min_steps - 1 TIMES.
              step = sy-index.
              ASSIGN nodes[ x = current-x + step
                            y = current-y ] TO <neighbor>.
              IF sy-subrc = 0.
                neighbor_loss += <neighbor>-loss.
              ENDIF.
            ENDDO.

        ENDCASE.

        " Actual steps
        DO each_direction_count TIMES.
          DATA(direction_count) = sy-index + min_steps - 1.
          DATA(keep_going) = abap_true.

          CASE direction.
            WHEN '1'. " Up
              IF current-direction <> 'U' AND
                 current-direction <> 'D' AND
                 current-y >= direction_count.

                ASSIGN nodes[ x = current-x
                              y = current-y - direction_count ] TO <neighbor>.
                neighbor_loss += <neighbor>-loss.
              ELSE.
                keep_going = abap_false.
                EXIT.
              ENDIF.

            WHEN '2'. " Down
              IF current-direction <> 'D' AND
                 current-direction <> 'U' AND
                 current-y <= max_y - direction_count.

                ASSIGN nodes[ x = current-x
                              y = current-y + direction_count ] TO <neighbor>.

                neighbor_loss += <neighbor>-loss.
              ELSE.
                keep_going = abap_false.
                EXIT.
              ENDIF.

            WHEN '3'. " Left
              IF current-direction <> 'L' AND
                 current-direction <> 'R' AND
                 current-x >= direction_count.

                ASSIGN nodes[ x = current-x - direction_count
                              y = current-y ] TO <neighbor>.

                neighbor_loss += <neighbor>-loss.
              ELSE.
                keep_going = abap_false.
                EXIT.
              ENDIF.

            WHEN '4'. " Right
              IF current-direction <> 'R' AND
                 current-direction <> 'L' AND
                 current-x <= max_x - direction_count.

                ASSIGN nodes[ x = current-x + direction_count
                              y = current-y ] TO <neighbor>.

                neighbor_loss += <neighbor>-loss.
              ELSE.
                keep_going = abap_false.
                EXIT.
              ENDIF.

          ENDCASE.

          IF keep_going = abap_false.
            CONTINUE.
          ENDIF.

          READ TABLE g_score WITH KEY x = current-x y = current-y direction = current-direction ASSIGNING FIELD-SYMBOL(<current_g_score>).
          IF sy-subrc <> 0.
            INSERT VALUE #( x = current-x y = current-y direction = current-direction loss = cl_abap_math=>max_int4 ) INTO TABLE g_score ASSIGNING <current_g_score>.
          ENDIF.

          DATA(tentative_score) = <current_g_score>-loss + neighbor_loss.

          READ TABLE g_score WITH KEY x = <neighbor>-x y = <neighbor>-y direction = this_direction ASSIGNING FIELD-SYMBOL(<neighbor_g_score>).
          IF sy-subrc <> 0.
            INSERT VALUE #( x = <neighbor>-x y = <neighbor>-y direction = this_direction loss = cl_abap_math=>max_int4 ) INTO TABLE g_score ASSIGNING <neighbor_g_score>.
          ENDIF.

          IF tentative_score < <neighbor_g_score>-loss.
            READ TABLE came_from WITH KEY to_x = <neighbor>-x to_y = <neighbor>-y to_direction = this_direction ASSIGNING FIELD-SYMBOL(<came_from>).
            IF sy-subrc <> 0.
              INSERT VALUE #( to_x   = <neighbor>-x
                              to_y   = <neighbor>-y
                              to_direction = this_direction ) INTO TABLE came_from ASSIGNING <came_from>.
            ENDIF.
            <came_from>-from_x         = current-x.
            <came_from>-from_y         = current-y.
            <came_from>-from_direction = current-direction.

            <neighbor_g_score>-loss = tentative_score.
            IF NOT line_exists( open_set[ x = <neighbor>-x y = <neighbor>-y direction = this_direction ] ).
              INSERT VALUE #( x = <neighbor>-x y = <neighbor>-y direction = this_direction loss = tentative_score ) INTO TABLE open_set.
            ENDIF.

          ENDIF.

        ENDDO.
      ENDDO.

    ENDWHILE.


    DATA(loss_r) = g_score[ x = to_x y = to_y direction = 'R' ]-loss.
    DATA(loss_d) = g_score[ x = to_x y = to_y direction = 'D' ]-loss.
    IF loss_r < loss_d.
      result = loss_r.
    ELSE.
      result = loss_d.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
