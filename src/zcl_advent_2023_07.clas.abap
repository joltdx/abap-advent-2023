CLASS zcl_advent_2023_07 DEFINITION
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
      BEGIN OF ty_hand,
        hand   TYPE string,
        bid    TYPE i,
        type   TYPE i,
        card_1 TYPE i,
        card_2 TYPE i,
        card_3 TYPE i,
        card_4 TYPE i,
        card_5 TYPE i,
      END OF ty_hand.
    TYPES ty_hands_tt TYPE STANDARD TABLE OF ty_hand WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_card_count,
        card  TYPE i,
        count TYPE i,
      END OF ty_card_count.
    TYPES ty_card_count_tt TYPE STANDARD TABLE OF ty_card_count WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF hand_type,
        five_of_a_kind  TYPE i VALUE 1,
        four_of_a_kind  TYPE i VALUE 2,
        full_house      TYPE i VALUE 3,
        three_of_a_kind TYPE i VALUE 4,
        two_pair        TYPE i VALUE 5,
        one_pair        TYPE i VALUE 6,
        high_card       TYPE i VALUE 7,
      END OF hand_type.

    METHODS parse_hands
      IMPORTING
        input         TYPE zif_advent_2023=>ty_input_table
        j_value       TYPE i DEFAULT 11
      RETURNING
        VALUE(result) TYPE ty_hands_tt.

    METHODS get_hand_type
      IMPORTING
        card_count_input TYPE ty_card_count_tt
      RETURNING
        VALUE(result)    TYPE i.

ENDCLASS.



CLASS zcl_advent_2023_07 IMPLEMENTATION.

  METHOD part_1.

    DATA total_winnings TYPE int8.

    DATA(hands) = parse_hands( input ).

    SORT hands BY type ASCENDING card_1 DESCENDING card_2 DESCENDING card_3 DESCENDING card_4 DESCENDING card_5 DESCENDING.

    DATA(rank) = lines( hands ).
    LOOP AT hands ASSIGNING FIELD-SYMBOL(<hand>).
      total_winnings += <hand>-bid * rank.
      rank -= 1.
    ENDLOOP.

    result = total_winnings.

  ENDMETHOD.

  METHOD part_2.

    DATA total_winnings TYPE int8.

    DATA(hands) = parse_hands( input   = input
                               j_value = 1 ).

    SORT hands BY type ASCENDING card_1 DESCENDING card_2 DESCENDING card_3 DESCENDING card_4 DESCENDING card_5 DESCENDING.

    DATA(rank) = lines( hands ).
    LOOP AT hands ASSIGNING FIELD-SYMBOL(<hand>).
      total_winnings = total_winnings + ( <hand>-bid * rank ).
      rank -= 1.
    ENDLOOP.

    result = total_winnings.

  ENDMETHOD.

  METHOD parse_hands.

    TYPES:
      BEGIN OF ty_card_value,
        card  TYPE c LENGTH 1,
        value TYPE i,
      END OF ty_card_value.
    TYPES ty_card_value_tt TYPE HASHED TABLE OF ty_card_value WITH UNIQUE KEY card.

    DATA card_values TYPE ty_card_value_tt.
    DATA card_count TYPE ty_card_count_tt.

    card_values = VALUE #( ( card = 'A' value = 14 )
                           ( card = 'K' value = 13 )
                           ( card = 'Q' value = 12 )
                           ( card = 'J' value = j_value )
                           ( card = 'T' value = 10 )
                           ( card = '9' value = 9 )
                           ( card = '8' value = 8 )
                           ( card = '7' value = 7 )
                           ( card = '6' value = 6 )
                           ( card = '5' value = 5 )
                           ( card = '4' value = 4 )
                           ( card = '3' value = 3 )
                           ( card = '2' value = 2 ) ).

    LOOP AT input ASSIGNING FIELD-SYMBOL(<input>).
      APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<line>).

      SPLIT <input> AT ' ' INTO DATA(hand) DATA(bid).
      <line>-hand = hand.
      <line>-bid = bid.

      CLEAR card_count.

      <line>-card_1 = card_values[ card = hand+0(1) ]-value.
      IF line_exists( card_count[ card = <line>-card_1 ] ).
        card_count[ card = <line>-card_1 ]-count += 1.
      ELSE.
        INSERT VALUE #( card = <line>-card_1 count = 1 ) INTO TABLE card_count.
      ENDIF.

      <line>-card_2 = card_values[ card = hand+1(1) ]-value.
      IF line_exists( card_count[ card = <line>-card_2 ] ).
        card_count[ card = <line>-card_2 ]-count += 1.
      ELSE.
        INSERT VALUE #( card = <line>-card_2 count = 1 ) INTO TABLE card_count.
      ENDIF.

      <line>-card_3 = card_values[ card = hand+2(1) ]-value.
      IF line_exists( card_count[ card = <line>-card_3 ] ).
        card_count[ card = <line>-card_3 ]-count += 1.
      ELSE.
        INSERT VALUE #( card = <line>-card_3 count = 1 ) INTO TABLE card_count.
      ENDIF.

      <line>-card_4 = card_values[ card = hand+3(1) ]-value.
      IF line_exists( card_count[ card = <line>-card_4 ] ).
        card_count[ card = <line>-card_4 ]-count += 1.
      ELSE.
        INSERT VALUE #( card = <line>-card_4 count = 1 ) INTO TABLE card_count.
      ENDIF.

      <line>-card_5 = card_values[ card = hand+4(1) ]-value.
      IF line_exists( card_count[ card = <line>-card_5 ] ).
        card_count[ card = <line>-card_5 ]-count += 1.
      ELSE.
        INSERT VALUE #( card = <line>-card_5 count = 1 ) INTO TABLE card_count.
      ENDIF.

      <line>-type = get_hand_type( card_count ).

    ENDLOOP.

  ENDMETHOD.

  METHOD get_hand_type.

    DATA(card_count) = card_count_input.

    IF lines( card_count ) > 1.

      SORT card_count BY count DESCENDING.

      READ TABLE card_count WITH KEY card = 1 ASSIGNING FIELD-SYMBOL(<joker>).
      IF sy-subrc = 0.
        DATA(joker_index) = sy-tabix.
        IF joker_index = 1.
          card_count[ 2 ]-count += <joker>-count.
        ELSE.
          card_count[ 1 ]-count += <joker>-count.
        ENDIF.

        DELETE card_count INDEX joker_index.
      ENDIF.
    ENDIF.

    SORT card_count BY count DESCENDING.

    CASE lines( card_count ).
      WHEN 1.
        result = hand_type-five_of_a_kind.

      WHEN 2.
        IF card_count[ 1 ]-count = 4.
          result = hand_type-four_of_a_kind.
        ELSE.
          result = hand_type-full_house.
        ENDIF.

      WHEN 3.
        IF card_count[ 1 ]-count = 3.
          result = hand_type-three_of_a_kind.
        ELSE.
          result = hand_type-two_pair.
        ENDIF.

      WHEN 4.
        result = hand_type-one_pair.

      WHEN 5.
        result = hand_type-high_card.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
