CLASS zcl_advent_2023_06 DEFINITION
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
      BEGIN OF ty_race,
        time TYPE int8,
        dist TYPE int8,
      END OF ty_race.
    TYPES ty_tt TYPE STANDARD TABLE OF ty_race WITH EMPTY KEY.

    METHODS get_num_ways_to_win
      IMPORTING
        time          TYPE int8
        dist          TYPE int8
      RETURNING
        VALUE(result) TYPE int8.

ENDCLASS.



CLASS zcl_advent_2023_06 IMPLEMENTATION.

  METHOD part_1.

    DATA multiplied TYPE int8 VALUE 1.

    "Time:      7  15   30
    "Distance:  9  40  200

    DATA(input_line) = input[ 1 ].
    CONDENSE input_line.
    SPLIT input_line AT space INTO TABLE DATA(times).

    input_line = input[ 2 ].
    CONDENSE input_line.
    SPLIT input_line AT space INTO TABLE DATA(dists).

    LOOP AT times ASSIGNING FIELD-SYMBOL(<time>) FROM 2.
      ASSIGN dists[ sy-tabix ] TO FIELD-SYMBOL(<dist>).

      DATA(num_ways_to_win) = get_num_ways_to_win( time = CONV #( <time> )
                                                   dist = CONV #( <dist> ) ).

      multiplied *= num_ways_to_win.

    ENDLOOP.

    result = multiplied.

  ENDMETHOD.

  METHOD part_2.

    DATA(input_line) = input[ 1 ].
    CONDENSE input_line NO-GAPS.
    SPLIT input_line AT ':' INTO DATA(header_time) DATA(time).

    input_line = input[ 2 ].
    CONDENSE input_line NO-GAPS.
    SPLIT input_line AT ':' INTO DATA(header_dist) DATA(dist).

    DATA(num_ways_to_win) = get_num_ways_to_win( time = CONV #( time )
                                                 dist = CONV #( dist ) ).

    result = num_ways_to_win.

  ENDMETHOD.

  METHOD get_num_ways_to_win.

    DATA too_slow TYPE i.
    DATA too_fast TYPE i.

    DO.
      too_slow += 1.
      IF too_slow * ( time - too_slow ) <= dist.
        CONTINUE.
      ENDIF.
      EXIT.
    ENDDO.

    too_fast = time.
    DO.
      too_fast -= 1.
      IF too_fast * ( time - too_fast ) <= dist.
        CONTINUE.
      ENDIF.
      EXIT.
    ENDDO.

    result = too_fast - too_slow + 1.

  ENDMETHOD.

ENDCLASS.
