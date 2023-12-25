CLASS zcl_advent_2023_25 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_node_id TYPE c LENGTH 3.

    TYPES:
      BEGIN OF ty_wire,
        a TYPE ty_node_id,
        b TYPE ty_node_id,
      END OF ty_wire.
    TYPES ty_wires_tt TYPE SORTED TABLE OF ty_wire WITH NON-UNIQUE KEY a.

    DATA wires TYPE ty_wires_tt.
    DATA nodes TYPE STANDARD TABLE OF ty_node_id WITH EMPTY KEY.

    METHODS get_shortest_cycle_through
      IMPORTING
        node          TYPE ty_node_id
        through       TYPE ty_node_id
      RETURNING
        VALUE(result) TYPE i.

    METHODS count_components
      IMPORTING
        node          TYPE ty_node_id
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.



CLASS zcl_advent_2023_25 IMPLEMENTATION.


  METHOD part_1.

    TYPES:
      BEGIN OF ty_cycles,
        node    TYPE ty_node_id,
        through TYPE ty_node_id,
        steps   TYPE i,
      END OF ty_cycles.

    DATA back_in_more_than_three TYPE ty_wires_tt.
    DATA visited TYPE RANGE OF ty_node_id.
    DATA cycles TYPE STANDARD TABLE OF ty_cycles WITH EMPTY KEY.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<input_line>).
      SPLIT <input_line> AT ': ' INTO DATA(input_left) DATA(input_right).
      SPLIT input_right AT ' ' INTO TABLE DATA(right_nodes).
      APPEND input_left TO nodes.
      LOOP AT right_nodes ASSIGNING FIELD-SYMBOL(<right_node>).
        INSERT VALUE #( a = input_left
                        b = <right_node> ) INTO TABLE wires.
        INSERT VALUE #( a = <right_node>
                        b = input_left ) INTO TABLE wires.
        APPEND <right_node> TO nodes.
      ENDLOOP.
    ENDLOOP.

    SORT nodes BY table_line.
    DELETE ADJACENT DUPLICATES FROM nodes COMPARING ALL FIELDS.

    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<node>).
      LOOP AT wires ASSIGNING FIELD-SYMBOL(<neighbor>) WHERE a = <node>.

        IF line_exists( cycles[ node = <neighbor>-b through = <node> ] ).
          CONTINUE.
        ENDIF.

        DATA(steps) = get_shortest_cycle_through( node    = <node>
                                                  through = <neighbor>-b ).

        APPEND VALUE #( node = <node>
                        through = <neighbor>-b
                        steps = steps ) TO cycles.

      ENDLOOP.
    ENDLOOP.

    SORT cycles BY steps DESCENDING.
    LOOP AT cycles ASSIGNING FIELD-SYMBOL(<cycle>) TO 3.
      DELETE wires WHERE a = <cycle>-node AND b = <cycle>-through.
      DELETE wires WHERE a = <cycle>-through AND b = <cycle>-node.
    ENDLOOP.

    DATA(count_one) = count_components( node = <cycle>-node ).
    DATA(count_the_other) = count_components( node = <cycle>-through ).

    DATA(multiply_them_both) = count_one * count_the_other.

    result = multiply_them_both.

  ENDMETHOD.


  METHOD part_2.

    result = |there is no part 2|.

  ENDMETHOD.

  METHOD get_shortest_cycle_through.

    TYPES:
      BEGIN OF ty_path,
        node  TYPE ty_node_id,
        count TYPE i,
      END OF ty_path.

    DATA open_set TYPE SORTED TABLE OF ty_path WITH NON-UNIQUE KEY count.
    DATA g_score TYPE HASHED TABLE OF ty_path WITH UNIQUE KEY node.
    DATA visited TYPE HASHED TABLE OF ty_node_id WITH UNIQUE KEY table_line.

    g_score = nodes.
    MODIFY g_score FROM VALUE #( count = cl_abap_math=>max_int4 ) TRANSPORTING count WHERE count IS INITIAL.
    g_score[ node = through ]-count = 1.

    LOOP AT wires ASSIGNING FIELD-SYMBOL(<wire>) WHERE a = through AND b <> node.
      INSERT VALUE #( node = <wire>-b count = 1 ) INTO TABLE open_set.
      g_score[ node = <wire>-b ]-count = 2.
    ENDLOOP.


    WHILE lines( open_set ) > 0.
      READ TABLE open_set INDEX 1 INTO DATA(current).
      DELETE open_set INDEX 1.

      IF current-node = node.
        " We're done
        EXIT.
      ENDIF.

      IF line_exists( visited[ table_line = current-node ] ).
        CONTINUE.
      ELSE.
        INSERT current-node INTO TABLE visited.
      ENDIF.

      DATA(tentative_score) = g_score[ node = current-node ]-count + 1.

      LOOP AT wires ASSIGNING FIELD-SYMBOL(<neighbor>) WHERE a = current-node.
        IF tentative_score < g_score[ node = <neighbor>-b ]-count.
          g_score[ node = <neighbor>-b ]-count = tentative_score.
          IF NOT line_exists( open_set[ node = <neighbor>-b ] ).
            INSERT VALUE #( node = <neighbor>-b count = tentative_score ) INTO TABLE open_set.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDWHILE.

    result = g_score[ node = node ]-count.

  ENDMETHOD.

  METHOD count_components.

    DATA visited TYPE HASHED TABLE OF ty_node_id WITH UNIQUE KEY table_line.

    DATA open_set TYPE STANDARD TABLE OF ty_node_id WITH EMPTY KEY.

    LOOP AT wires ASSIGNING FIELD-SYMBOL(<wire>) WHERE a = node.
      INSERT <wire>-b INTO TABLE open_set.
    ENDLOOP.

    INSERT node INTO TABLE visited.

    WHILE lines( open_set ) > 0.
      DATA(current) = open_set[ 1 ].
      DELETE open_set INDEX 1.

      IF line_exists( visited[ table_line = current ] ).
        CONTINUE.
      ELSE.
        INSERT current INTO TABLE visited.
      ENDIF.

      LOOP AT wires ASSIGNING <wire> WHERE a = current.
        INSERT <wire>-b INTO TABLE open_set.
      ENDLOOP.

    ENDWHILE.

    result = lines( visited ).

  ENDMETHOD.

ENDCLASS.
