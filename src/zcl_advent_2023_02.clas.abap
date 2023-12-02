CLASS zcl_advent_2023_02 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_advent_2023_02 IMPLEMENTATION.

  METHOD part_1.

    TYPES:
      BEGIN OF ty_revealed,
        game  TYPE i,
        red   TYPE i,
        green TYPE i,
        blue  TYPE i,
      END OF ty_revealed.
    TYPES ty_revealed_tt TYPE SORTED TABLE OF ty_revealed WITH NON-UNIQUE KEY game.

    DATA games TYPE ty_revealed_tt.
    DATA game_id TYPE i.

    " Input format:
    " Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    " Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue

    LOOP AT input ASSIGNING FIELD-SYMBOL(<input>).
      SPLIT <input> AT ':' INTO DATA(game) DATA(sets).
      game_id = substring( val = game off = 5 ).
      SPLIT sets AT ';' INTO TABLE DATA(sets_data).

      LOOP AT sets_data ASSIGNING FIELD-SYMBOL(<set_data>).
        INSERT VALUE #( game = game_id ) INTO TABLE games ASSIGNING FIELD-SYMBOL(<game>).

        FIND REGEX '(\d*) red' IN <set_data> SUBMATCHES DATA(count).
        IF sy-subrc = 0.
          <game>-red = count.
        ENDIF.

        FIND REGEX '(\d*) green' IN <set_data> SUBMATCHES count.
        IF sy-subrc = 0.
          <game>-green = count.
        ENDIF.

        FIND REGEX '(\d*) blue' IN <set_data> SUBMATCHES count.
        IF sy-subrc = 0.
          <game>-blue = count.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    DATA invalid_games TYPE RANGE OF i.

    SELECT DISTINCT game AS low, 'I' AS sign, 'EQ' AS option
      FROM @games AS games
      WHERE red > 12
         OR green > 13
         OR blue > 14
      INTO CORRESPONDING FIELDS OF TABLE @invalid_games.

    DELETE games WHERE game IN invalid_games.

    SELECT SUM( DISTINCT game ) FROM @games AS games INTO @DATA(sum_of_games).

    result = sum_of_games.

  ENDMETHOD.

  METHOD part_2.

    TYPES:
      BEGIN OF ty_revealed,
        game  TYPE i,
        red   TYPE i,
        green TYPE i,
        blue  TYPE i,
      END OF ty_revealed.
    TYPES ty_revealed_tt TYPE SORTED TABLE OF ty_revealed WITH NON-UNIQUE KEY game.

    DATA games TYPE ty_revealed_tt.
    DATA game_id TYPE i.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<input>).
      SPLIT <input> AT ':' INTO DATA(game) DATA(sets).
      game_id = substring( val = game off = 5 ).
      SPLIT sets AT ';' INTO TABLE DATA(sets_data).
      INSERT VALUE #( game = game_id ) INTO TABLE games ASSIGNING FIELD-SYMBOL(<game>).

      LOOP AT sets_data ASSIGNING FIELD-SYMBOL(<set_data>).
        FIND REGEX '(\d*) red' IN <set_data> SUBMATCHES DATA(count).
        IF sy-subrc = 0 AND count > <game>-red.
          <game>-red = count.
        ENDIF.

        FIND REGEX '(\d*) green' IN <set_data> SUBMATCHES count.
        IF sy-subrc = 0 AND count > <game>-green.
          <game>-green = count.
        ENDIF.

        FIND REGEX '(\d*) blue' IN <set_data> SUBMATCHES count.
        IF sy-subrc = 0 AND count > <game>-blue.
          <game>-blue = count.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    DATA power TYPE i.
    DATA power_sum TYPE int8.

    LOOP AT games ASSIGNING <game>.
      power = <game>-red * <game>-green * <game>-blue.
      power_sum = power_sum + power.
    ENDLOOP.

    result = power_sum.

  ENDMETHOD.

ENDCLASS.
