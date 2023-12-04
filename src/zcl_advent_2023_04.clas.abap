CLASS zcl_advent_2023_04 DEFINITION
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



CLASS zcl_advent_2023_04 IMPLEMENTATION.

  METHOD part_1.

    DATA match_count TYPE i.
    DATA total_score TYPE i.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      SPLIT <line>  AT ':' INTO DATA(card) DATA(numbers).
      SPLIT numbers AT '|' INTO DATA(winning) DATA(mine).
      SPLIT winning AT ' ' INTO TABLE DATA(winning_table).
      SPLIT mine    AT ' ' INTO TABLE DATA(mine_table).

      match_count = 0.

      LOOP AT winning_table ASSIGNING FIELD-SYMBOL(<winning>) WHERE table_line IS NOT INITIAL.
        IF line_exists( mine_table[ table_line = <winning> ] ).
          match_count += 1.
        ENDIF.
      ENDLOOP.

      IF match_count > 0.
        total_score += 2 ** ( match_count - 1 ).
      ENDIF.

    ENDLOOP.

    result = total_score.

  ENDMETHOD.

  METHOD part_2.

    TYPES:
      BEGIN OF ty_card,
        card      TYPE i,
        winning   TYPE RANGE OF i,
        mine      TYPE STANDARD TABLE OF i WITH EMPTY KEY,
        instances TYPE i,
      END OF ty_card.
    TYPES ty_card_tt TYPE SORTED TABLE OF ty_card WITH UNIQUE KEY card.

    DATA pile_of_cards TYPE ty_card_tt.
    DATA card_number TYPE i.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      SPLIT <line>  AT ':' INTO DATA(card) DATA(numbers).
      SPLIT numbers AT '|' INTO DATA(winning) DATA(mine).
      SPLIT winning AT ' ' INTO TABLE DATA(winning_table).
      SPLIT mine    AT ' ' INTO TABLE DATA(mine_table).
      DELETE mine_table WHERE table_line IS INITIAL.

      card_number += 1.

      INSERT VALUE #( card = card_number
                      winning   = VALUE #( FOR <winning> IN winning_table WHERE ( table_line IS NOT INITIAL )
                                           sign   = 'I'
                                           option = 'EQ'
                                           ( low = <winning> ) )
                      mine      = mine_table
                      instances = 1 ) INTO TABLE pile_of_cards.
    ENDLOOP.

    LOOP AT pile_of_cards ASSIGNING FIELD-SYMBOL(<card>).

      SELECT COUNT( * )
        FROM @<card>-mine AS mine
        WHERE table_line IN @<card>-winning
        INTO @DATA(win_count).

      IF win_count > 0.
        LOOP AT pile_of_cards FROM <card>-card + 1 TO <card>-card + win_count ASSIGNING FIELD-SYMBOL(<extra_card>).
          <extra_card>-instances += <card>-instances.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    SELECT SUM( instances ) FROM @pile_of_cards AS pile_of_cards INTO @DATA(number_of_cards).

    result = number_of_cards.

  ENDMETHOD.

ENDCLASS.
