CLASS zcl_advent_2023_22 DEFINITION
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
      BEGIN OF ty_brick_coords,
        id TYPE i,
        x1 TYPE i,
        y1 TYPE i,
        z1 TYPE i,
        x2 TYPE i,
        y2 TYPE i,
        z2 TYPE i,
      END OF ty_brick_coords.
    TYPES ty_brick_coords_tt TYPE SORTED TABLE OF ty_brick_coords WITH NON-UNIQUE KEY z1.

    TYPES:
      BEGIN OF ty_brick,
        x  TYPE i,
        y  TYPE i,
        z  TYPE i,
        id TYPE i,
      END OF ty_brick.
    TYPES ty_bricks_tt TYPE HASHED TABLE OF ty_brick WITH UNIQUE KEY x y z
                                                     WITH NON-UNIQUE SORTED KEY id_key COMPONENTS id.

    TYPES ty_brick_ids_tt TYPE SORTED TABLE OF i WITH NON-UNIQUE DEFAULT KEY.

    TYPES:
      BEGIN OF ty_bricks_memo,
        id     TYPE i,
        bricks TYPE ty_brick_ids_tt,
      END OF ty_bricks_memo.

    DATA the_bricks TYPE ty_brick_coords_tt.
    DATA the_grid TYPE ty_bricks_tt.

    METHODS get_bricks_from_input
      IMPORTING
        input  TYPE zif_advent_2023=>ty_input_table
      EXPORTING
        bricks TYPE ty_brick_coords_tt
        grid   TYPE ty_bricks_tt.

    METHODS settle_bricks
      IMPORTING
        input TYPE zif_advent_2023=>ty_input_table.

    METHODS disintegrate_and_settle
      IMPORTING
        id            TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS get_bricks_below
      IMPORTING
        id            TYPE i
      RETURNING
        VALUE(result) TYPE ty_brick_ids_tt.

    METHODS get_bricks_below_local
      IMPORTING
        id            TYPE i
        grid          TYPE ty_bricks_tt
      RETURNING
        VALUE(result) TYPE ty_brick_ids_tt.

    METHODS get_bricks_above
      IMPORTING
        id            TYPE i
      RETURNING
        VALUE(result) TYPE ty_brick_ids_tt.

ENDCLASS.



CLASS zcl_advent_2023_22 IMPLEMENTATION.

  METHOD part_1.

    settle_bricks( input ).

    DATA count TYPE i.

    LOOP AT the_bricks ASSIGNING FIELD-SYMBOL(<brick>).
      DATA(bricks_above) = get_bricks_above( <brick>-id ).
      DATA(count_above) = lines( bricks_above ).
      IF count_above = 0.
        count += 1.
      ELSE.
        DATA(still_safe) = abap_true.
        LOOP AT bricks_above ASSIGNING FIELD-SYMBOL(<above>).
          DATA(count_below) = get_bricks_below( <above> ).
          IF lines( count_below ) < 2.
            still_safe = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF still_safe = abap_true.
          count += 1.
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = count.

  ENDMETHOD.

  METHOD part_2.

    settle_bricks( input ).

    DATA sum TYPE i.

    LOOP AT the_bricks ASSIGNING FIELD-SYMBOL(<brick>).
      sum += disintegrate_and_settle( <brick>-id ).
    ENDLOOP.

    result = sum.

  ENDMETHOD.

  METHOD get_bricks_from_input.

    " 1,0,1~1,2,1
    " 0,0,2~2,0,2
    " 0,2,3~2,2,3
    " 0,0,4~0,2,4
    " 2,0,5~2,2,5
    " 0,1,6~2,1,6
    " 1,1,8~1,1,9

    " Individual brick coordinates
    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      SPLIT <line> AT '~' INTO DATA(pos1) DATA(pos2).
      SPLIT pos1 AT ',' INTO DATA(xa) DATA(ya) DATA(za).
      SPLIT pos2 AT ',' INTO DATA(xb) DATA(yb) DATA(zb).
      IF xa < xb.
        DATA(x1) = xa.
        DATA(x2) = xb.
      ELSE.
        x1 = xb.
        x2 = xa.
      ENDIF.
      IF ya < yb.
        DATA(y1) = ya.
        DATA(y2) = yb.
      ELSE.
        y1 = yb.
        y2 = ya.
      ENDIF.
      IF za < zb.
        DATA(z1) = za.
        DATA(z2) = zb.
      ELSE.
        z1 = zb.
        z2 = za.
      ENDIF.

      INSERT VALUE #( x1 = x1 y1 = y1 z1 = z1 x2 = x2 y2 = y2 z2 = z2 ) INTO TABLE bricks.
    ENDLOOP.

    " Assign ID
    LOOP AT bricks ASSIGNING FIELD-SYMBOL(<brick>).
      <brick>-id = sy-tabix.
    ENDLOOP.

    " Expand bricks across their coordinates
    LOOP AT bricks ASSIGNING <brick>.
      DATA(x) = <brick>-x1.
      WHILE x <= <brick>-x2.
        INSERT VALUE #( x = x y = <brick>-y1 z = <brick>-z1 id = <brick>-id ) INTO TABLE grid.
        x += 1.
      ENDWHILE.

      DATA(y) = <brick>-y1.
      WHILE y <= <brick>-y2.
        INSERT VALUE #( x = <brick>-x1 y = y z = <brick>-z1 id = <brick>-id ) INTO TABLE grid.
        y += 1.
      ENDWHILE.

      DATA(z) = <brick>-z1.
      WHILE z <= <brick>-z2.
        INSERT VALUE #( x = <brick>-x1 y = <brick>-y1 z = z id = <brick>-id ) INTO TABLE grid.
        z += 1.
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.

  METHOD settle_bricks.

    IF the_grid IS NOT INITIAL.
      CLEAR the_bricks.
      CLEAR the_grid.
    ENDIF.

    get_bricks_from_input( EXPORTING input = input
                           IMPORTING bricks = the_bricks
                                     grid   = the_grid ).

    DATA new_positions LIKE the_grid.

    LOOP AT the_grid ASSIGNING FIELD-SYMBOL(<brick>) USING KEY id_key.
      DATA(brick_id) = <brick>-id.

      DATA(keep_looking) = abap_true.

      WHILE keep_looking = abap_true.
        DATA(bricks_below) = get_bricks_below( brick_id ).

        IF lines( bricks_below ) = 0.
          CLEAR new_positions.
          LOOP AT the_grid ASSIGNING FIELD-SYMBOL(<grid>) USING KEY id_key WHERE id = brick_id .
            INSERT VALUE #( x = <grid>-x y = <grid>-y z = <grid>-z - 1 id = <grid>-id ) INTO TABLE new_positions.
            DELETE TABLE the_grid FROM <grid>.
          ENDLOOP.
          INSERT LINES OF new_positions INTO TABLE the_grid.
        ELSE.
          keep_looking = abap_false.
        ENDIF.
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.

  METHOD disintegrate_and_settle.

    DATA new_positions LIKE the_grid.
    DATA settled_ids TYPE SORTED TABLE OF i WITH NON-UNIQUE DEFAULT KEY.

    DATA(local_grid) = the_grid.

    DELETE local_grid USING KEY id_key WHERE id = id.

    LOOP AT local_grid ASSIGNING FIELD-SYMBOL(<brick>) USING KEY id_key.
      DATA(brick_id) = <brick>-id.
      DATA(keep_looking) = abap_true.

      WHILE keep_looking = abap_true.
        DATA(bricks_below) = get_bricks_below_local( id = brick_id
                                                     grid = local_grid ).

        IF lines( bricks_below ) = 0.
          CLEAR new_positions.
          LOOP AT local_grid ASSIGNING FIELD-SYMBOL(<grid>) USING KEY id_key WHERE id = brick_id .
            INSERT VALUE #( x = <grid>-x y = <grid>-y z = <grid>-z - 1 id = <grid>-id ) INTO TABLE new_positions.
            DELETE TABLE local_grid FROM <grid>.
          ENDLOOP.
          INSERT LINES OF new_positions INTO TABLE local_grid.
          APPEND brick_id  TO settled_ids.
        ELSE.
          keep_looking = abap_false.
        ENDIF.
      ENDWHILE.

    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM settled_ids.

    result = lines( settled_ids ).

  ENDMETHOD.

  METHOD get_bricks_above.

    LOOP AT the_grid ASSIGNING FIELD-SYMBOL(<grid>) USING KEY id_key WHERE id = id.
      READ TABLE the_grid WITH KEY x = <grid>-x y = <grid>-y z = <grid>-z + 1 ASSIGNING FIELD-SYMBOL(<above>).
      IF sy-subrc = 0 AND <above>-id <> <grid>-id.
        INSERT <above>-id INTO TABLE result.
      ENDIF.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM result.

  ENDMETHOD.

  METHOD get_bricks_below.

    LOOP AT the_grid ASSIGNING FIELD-SYMBOL(<grid>) USING KEY id_key WHERE id = id.
      READ TABLE the_grid WITH KEY x = <grid>-x y = <grid>-y z = <grid>-z - 1 ASSIGNING FIELD-SYMBOL(<below>).
      IF sy-subrc = 0 AND <below>-id <> <grid>-id.
        INSERT <below>-id INTO TABLE result.
      ELSEIF <grid>-z = 1.
        INSERT 0 INTO TABLE result.
      ENDIF.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM result.

  ENDMETHOD.

  METHOD get_bricks_below_local.

    LOOP AT grid ASSIGNING FIELD-SYMBOL(<grid>) USING KEY id_key WHERE id = id.
      READ TABLE grid WITH KEY x = <grid>-x y = <grid>-y z = <grid>-z - 1 ASSIGNING FIELD-SYMBOL(<below>).
      IF sy-subrc = 0 AND <below>-id <> <grid>-id.
        INSERT <below>-id INTO TABLE result.
      ELSEIF <grid>-z = 1.
        INSERT 0 INTO TABLE result.
      ENDIF.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM result.

  ENDMETHOD.

ENDCLASS.
