CLASS zcl_advent_2023_24 DEFINITION
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
      BEGIN OF ty_point,
        x TYPE decfloat34,
        y TYPE decfloat34,
        z TYPE decfloat34,
      END OF ty_point.

    TYPES:
      BEGIN OF ty_hailstone,
        px TYPE decfloat34,
        py TYPE decfloat34,
        pz TYPE decfloat34,
        vx TYPE decfloat34,
        vy TYPE decfloat34,
        vz TYPE decfloat34,
      END OF ty_hailstone.

*    TYPES:
*      BEGIN OF ty_line,
*        x1 TYPE decfloat34,
*        y1 TYPE decfloat34,
*        z1 TYPE decfloat34,
*        x2 TYPE decfloat34,
*        y2 TYPE decfloat34,
*        z2 TYPE decfloat34,
*        vx TYPE decfloat34,
*        vy TYPE decfloat34,
*        vz TYPE decfloat34,
*      END OF ty_line.

    METHODS get_line_line_intersection
      IMPORTING
        x1            TYPE decfloat34
        y1            TYPE decfloat34
        x2            TYPE decfloat34
        y2            TYPE decfloat34
        x3            TYPE decfloat34
        y3            TYPE decfloat34
        x4            TYPE decfloat34
        y4            TYPE decfloat34
      RETURNING
        VALUE(result) TYPE ty_point.

    METHODS get_xyz_intersection
      IMPORTING
        stone1        TYPE ty_hailstone
        stone2        TYPE ty_hailstone
        rock          TYPE ty_hailstone OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_point.
ENDCLASS.



CLASS zcl_advent_2023_24 IMPLEMENTATION.

  METHOD part_1.

    TYPES:
      BEGIN OF ty_line,
        x1 TYPE decfloat34,
        y1 TYPE decfloat34,
        x2 TYPE decfloat34,
        y2 TYPE decfloat34,
        vx TYPE decfloat34,
        vy TYPE decfloat34,
      END OF ty_line.

    DATA lines TYPE STANDARD TABLE OF ty_line WITH EMPTY KEY.

    IF lines( input ) > 10.
      " *** For test data:
      DATA(min_coord) = 200000000000000.
      DATA(max_coord) = 400000000000000.
    ELSE.
      " *** For real puzzle data:
      min_coord = 7.    " 7  for test data    200000000000000 for real input
      max_coord = 27.   " 27 for test data    400000000000000 for real input
    ENDIF.


    LOOP AT input ASSIGNING FIELD-SYMBOL(<input_line>).
      DATA(input_line) = <input_line>.
      CONDENSE input_line.
      REPLACE ALL OCCURRENCES OF ',' IN input_line WITH space.
      SPLIT input_line AT space INTO DATA(px) DATA(py) DATA(pz) DATA(at) DATA(vx) DATA(vy) DATA(vz).
      APPEND VALUE #( x1 = px
                      y1 = py
                      x2 = px + vx
                      y2 = py + vy
                      vx = vx
                      vy = vy ) TO lines.
    ENDLOOP.

    " https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection

    DATA(count_intersections) = 0.

    LOOP AT lines ASSIGNING FIELD-SYMBOL(<l1>).
      DATA(x1) = <l1>-x1.
      DATA(y1) = <l1>-y1.
      DATA(x2) = <l1>-x2.
      DATA(y2) = <l1>-y2.

      DATA(line1_count) = sy-tabix.
      LOOP AT lines FROM line1_count + 1 ASSIGNING FIELD-SYMBOL(<l2>).
        DATA(x3) = <l2>-x1.
        DATA(y3) = <l2>-y1.
        DATA(x4) = <l2>-x2.
        DATA(y4) = <l2>-y2.

        DATA(denominator) = ( x1 - x2 ) * ( y3 - y4 ) - ( y1 - y2 ) * ( x3 - x4 ).
        IF denominator = 0.
          CONTINUE.
        ENDIF.

        DATA(nom_left) = x1 * y2 - y1 * x2.
        DATA(nom_right) = x3 * y4 - y3 * x4.

        " DATA(nominator_x) = ( x1 * y2 - y1 * x2 ) * ( x3 - x4 ) - ( x1 - x2 ) * ( x3 * y4 - y3 * x4 ).
        DATA(nominator_x) = nom_left * ( x3 - x4 ) - nom_right * ( x1 - x2 ).

        " DATA(nominator_y) = ( x1 * y2 - y1 * x2 ) * ( y3 - y4 ) - ( y1 - y2 ) * ( x3 * y4 - y3 * x4 ).
        DATA(nominator_y) = nom_left * ( y3 - y4 ) - nom_right * ( y1 - y2 ).

        DATA(intersect_x) = nominator_x / denominator.
        DATA(intersect_y) = nominator_y / denominator.

        IF intersect_x >= min_coord AND
           intersect_x <= max_coord AND
           intersect_y >= min_coord AND
           intersect_y <= max_coord.

          IF ( <l1>-vx > 0 AND intersect_x < x1 ) OR
             ( <l1>-vx < 0 AND intersect_x > x1 ) OR
             ( <l1>-vy > 0 AND intersect_y < y1 ) OR
             ( <l1>-vy < 0 AND intersect_y > y1 ) OR
             ( <l2>-vx > 0 AND intersect_x < x3 ) OR
             ( <l2>-vx < 0 AND intersect_x > x3 ) OR
             ( <l2>-vy > 0 AND intersect_y < y3 ) OR
             ( <l2>-vy < 0 AND intersect_y > y3 ).
            CONTINUE.
          ENDIF.
          count_intersections += 1.
        ENDIF.

      ENDLOOP.
    ENDLOOP.

    result = count_intersections.

  ENDMETHOD.

  METHOD part_2.

    DATA hailstones TYPE STANDARD TABLE OF ty_hailstone WITH EMPTY KEY.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<input_line>).
      DATA(input_line) = <input_line>.
      CONDENSE input_line.
      REPLACE ALL OCCURRENCES OF ',' IN input_line WITH space.
      SPLIT input_line AT space INTO DATA(px) DATA(py) DATA(pz) DATA(at) DATA(vx) DATA(vy) DATA(vz).
      APPEND VALUE #( px = px
                      py = py
                      pz = pz
                      vx = vx
                      vy = vy
                      vz = vz ) TO hailstones.
    ENDLOOP.

    " There are mathy solutions to this, but let's do some "clever" brute-forcing by
    " adjusting 3 of the hailstones by a rock velocity vx vy vz, to make all of them
    " intersect in x, y and z axis. Then all other hailstones will also intersect there
    " for the same adjustment (according to problem statement being true).
    " The intersection point is at the rock throwing position and then we're done!

    ASSIGN hailstones[ 1 ] TO FIELD-SYMBOL(<s1>).
    ASSIGN hailstones[ 2 ] TO FIELD-SYMBOL(<s2>).
    ASSIGN hailstones[ 3 ] TO FIELD-SYMBOL(<s3>).

    IF lines( input ) < 10.
      DATA(search_range) = 50.
    ELSE.
      search_range = 1000.
    ENDIF.
    DATA(half_of_that) = search_range / 2.

    DO search_range TIMES.
      DATA(dx) = sy-index - half_of_that.

      " We don't need to bother with some dx...
      " If x1 > x2 and v1 > v2, a dx between v1 and v2 will not work. And vice versa
      " Skip early
      IF <s1>-px > <s2>-px AND <s1>-vx > <s2>-vx AND dx > <s1>-vx AND dx < <s2>-vx
      OR <s1>-px < <s2>-px AND <s1>-vx < <s2>-vx AND dx > <s2>-vx AND dx < <s1>-vx
      OR <s1>-px > <s3>-px AND <s1>-vx > <s3>-vx AND dx > <s1>-vx AND dx < <s3>-vx
      OR <s1>-px < <s3>-px AND <s1>-vx < <s3>-vx AND dx > <s3>-vx AND dx < <s1>-vx
      OR <s2>-px > <s3>-px AND <s2>-vx > <s3>-vx AND dx > <s2>-vx AND dx < <s3>-vx
      OR <s2>-px < <s3>-px AND <s2>-vx < <s3>-vx AND dx > <s3>-vx AND dx < <s2>-vx.
        CONTINUE.
      ENDIF.

      DO search_range TIMES.
        DATA(dy) = sy-index - half_of_that.

        IF <s1>-py > <s2>-py AND <s1>-vy > <s2>-vy AND dy > <s1>-vy AND dy < <s2>-vy
          OR <s1>-py < <s2>-py AND <s1>-vy < <s2>-vy AND dy > <s2>-vy AND dy < <s1>-vy
          OR <s1>-py > <s3>-py AND <s1>-vy > <s3>-vy AND dy > <s1>-vy AND dy < <s3>-vy
          OR <s1>-py < <s3>-py AND <s1>-vy < <s3>-vy AND dy > <s3>-vy AND dy < <s1>-vy
          OR <s2>-py > <s3>-py AND <s2>-vy > <s3>-vy AND dy > <s2>-vy AND dy < <s3>-vy
          OR <s2>-py < <s3>-py AND <s2>-vy < <s3>-vy AND dy > <s3>-vy AND dy < <s2>-vy.
          CONTINUE.
        ENDIF.

        " Pre-check! If x-y intersection between 1 and 2 doesn't match, there's no need to check any of the z
        DATA(pre_check12) = get_line_line_intersection( x1 = <s1>-px                  y1 = <s1>-py
                                                        x2 = <s1>-px + <s1>-vx - dx   y2 = <s1>-py + <s1>-vy - dy
                                                        x3 = <s2>-px                  y3 = <s2>-py
                                                        x4 = <s2>-px + <s2>-vx - dx   y4 = <s2>-py + <s2>-vy - dy ).

        DATA(pre_check13) = get_line_line_intersection( x1 = <s1>-px                  y1 = <s1>-py
                                                        x2 = <s1>-px + <s1>-vx - dx   y2 = <s1>-py + <s1>-vy - dy
                                                        x3 = <s3>-px                  y3 = <s3>-py
                                                        x4 = <s3>-px + <s3>-vx - dx   y4 = <s3>-py + <s3>-vy - dy ).

        IF pre_check12 <> pre_check13.
          CONTINUE.
        ENDIF.

        DO search_range TIMES.
          DATA(dz) = sy-index - half_of_that.

          DATA(rock) = VALUE ty_hailstone( vx = dx  vy = dy  vz = dz ).

          IF <s1>-pz > <s2>-pz AND <s1>-vz > <s2>-vz AND dz > <s1>-vz AND dz < <s2>-vz
          OR <s1>-pz < <s2>-pz AND <s1>-vz < <s2>-vz AND dz > <s2>-vz AND dz < <s1>-vz
          OR <s1>-pz > <s3>-pz AND <s1>-vz > <s3>-vz AND dz > <s1>-vz AND dz < <s3>-vz
          OR <s1>-pz < <s3>-pz AND <s1>-vz < <s3>-vz AND dz > <s3>-vz AND dz < <s1>-vz
          OR <s2>-pz > <s3>-pz AND <s2>-vz > <s3>-vz AND dz > <s2>-vz AND dz < <s3>-vz
          OR <s2>-pz < <s3>-pz AND <s2>-vz < <s3>-vz AND dz > <s3>-vz AND dz < <s2>-vz.
            CONTINUE.
          ENDIF.

          DATA(intersection12) = get_xyz_intersection( stone1 = <s1>   stone2 = <s2>   rock = rock ).
          DATA(intersection13) = get_xyz_intersection( stone1 = <s1>   stone2 = <s3>   rock = rock ).
          IF intersection12 <> intersection13.
            CONTINUE.
          ENDIF.

          DATA(intersection23) = get_xyz_intersection( stone1 = <s2>   stone2 = <s3>   rock = rock ).
          IF intersection12 <> intersection23.
            CONTINUE.
          ENDIF.

          " If we're here, we have 3 hailstones intersecting at the same position, that ought to do it!
          DATA(rock_sum) = intersection12-x + intersection12-y + intersection12-z.
          result = |{ rock_sum } (at { intersection12-x }, { intersection12-y }, { intersection12-z } @ { dx }, { dy }, { dz })|.
          RETURN.

        ENDDO.

      ENDDO.

    ENDDO.

    result = |not found|.

  ENDMETHOD.



  METHOD get_line_line_intersection.

    " https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection

    DATA(denominator) = ( x1 - x2 ) * ( y3 - y4 ) - ( y1 - y2 ) * ( x3 - x4 ).
    IF denominator = 0.
      RETURN.
    ENDIF.

    DATA(nom_left) = x1 * y2 - y1 * x2.
    DATA(nom_right) = x3 * y4 - y3 * x4.

    " DATA(nominator_x) = ( x1 * y2 - y1 * x2 ) * ( x3 - x4 ) - ( x1 - x2 ) * ( x3 * y4 - y3 * x4 ).
    DATA(nominator_x) = nom_left * ( x3 - x4 ) - nom_right * ( x1 - x2 ).

    " DATA(nominator_y) = ( x1 * y2 - y1 * x2 ) * ( y3 - y4 ) - ( y1 - y2 ) * ( x3 * y4 - y3 * x4 ).
    DATA(nominator_y) = nom_left * ( y3 - y4 ) - nom_right * ( y1 - y2 ).

    result = VALUE #( x = nominator_x / denominator
                      y = nominator_y / denominator ).
  ENDMETHOD.

  METHOD get_xyz_intersection.

    ASSIGN stone1 TO FIELD-SYMBOL(<l1>).
    ASSIGN stone2 TO FIELD-SYMBOL(<l2>).
    ASSIGN rock TO FIELD-SYMBOL(<r>).

    " X Y
    DATA(intersection) = get_line_line_intersection( x1 = <l1>-px                      y1 = <l1>-py
                                                     x2 = <l1>-px + <l1>-vx - <r>-vx   y2 = <l1>-py + <l1>-vy - <r>-vy
                                                     x3 = <l2>-px                      y3 = <l2>-py
                                                     x4 = <l2>-px + <l2>-vx - <r>-vx   y4 = <l2>-py + <l2>-vy - <r>-vy ).
    IF intersection IS NOT INITIAL.
      result-x = intersection-x.
      result-y = intersection-y.
    ENDIF.

    " X Z
    intersection = get_line_line_intersection( x1 = <l1>-px                      y1 = <l1>-pz
                                               x2 = <l1>-px + <l1>-vx - <r>-vx   y2 = <l1>-pz + <l1>-vz - <r>-vz
                                               x3 = <l2>-px                      y3 = <l2>-pz
                                               x4 = <l2>-px + <l2>-vx - <r>-vx   y4 = <l2>-pz + <l2>-vz - <r>-vz ).
    IF intersection IS NOT INITIAL.
      result-x = intersection-x.
      result-z = intersection-y.
    ENDIF.

    " Y Z
    intersection = get_line_line_intersection( x1 = <l1>-py                      y1 = <l1>-pz
                                               x2 = <l1>-py + <l1>-vy - <r>-vy   y2 = <l1>-pz + <l1>-vz - <r>-vz
                                               x3 = <l2>-py                      y3 = <l2>-pz
                                               x4 = <l2>-py + <l2>-vy  - <r>-vy  y4 = <l2>-pz + <l2>-vz - <r>-vz ).
    IF intersection IS NOT INITIAL.
      result-y = intersection-x.
      result-z = intersection-y.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
