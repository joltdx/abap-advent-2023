CLASS zcl_advent_2023_08 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_node TYPE c LENGTH 3.

    TYPES:
      BEGIN OF ty_network,
        node  TYPE ty_node,
        left  TYPE ty_node,
        right TYPE ty_node,
      END OF ty_network.
    TYPES ty_network_tt TYPE HASHED TABLE OF ty_network WITH UNIQUE KEY node.

    TYPES:
      BEGIN OF ty_prime_factors,
        factor TYPE int8,
        count  TYPE int8,
      END OF ty_prime_factors.
    TYPES ty_prime_factors_tt TYPE STANDARD TABLE OF ty_prime_factors WITH EMPTY KEY.

    DATA the_network TYPE ty_network_tt.

    METHODS get_network
      IMPORTING
        input         TYPE zif_advent_2023=>ty_input_table
      RETURNING
        VALUE(result) TYPE ty_network_tt.

    METHODS get_prime_factors
      IMPORTING
        number        TYPE int8
      RETURNING
        VALUE(result) TYPE ty_prime_factors_tt.

ENDCLASS.



CLASS zcl_advent_2023_08 IMPLEMENTATION.

  METHOD part_1.

    DATA steps TYPE int8.
    DATA instruction_offset TYPE i.

    DATA(instructions) = input[ 1 ].
    DATA(max_offset) = strlen( instructions ) - 1.

    DATA(network) = get_network( input ).

    ASSIGN network[ node = 'AAA' ] TO FIELD-SYMBOL(<current_node>).

    DO.
      IF <current_node>-node = 'ZZZ'.
        EXIT.
      ENDIF.

      DATA(direction) = instructions+instruction_offset(1).
      IF direction = 'L'.
        ASSIGN network[ node = <current_node>-left ] TO <current_node>.
      ELSE.
        ASSIGN network[ node = <current_node>-right ] TO <current_node>.
      ENDIF.

      steps += 1.
      IF instruction_offset >= max_offset.
        instruction_offset = 0.
      ELSE.
        instruction_offset += 1.
      ENDIF.
    ENDDO.

    result = steps.

  ENDMETHOD.

  METHOD part_2.

    DATA steps TYPE int8.
    DATA instruction_offset TYPE i.
    DATA current_nodes TYPE STANDARD TABLE OF ty_network WITH EMPTY KEY.
    DATA step_counts TYPE STANDARD TABLE OF int8.
    DATA prime_factors TYPE ty_prime_factors_tt.

    DATA(instructions) = input[ 1 ].
    DATA(max_offset) = strlen( instructions ) - 1.

    DATA(network) = get_network( input ).

    current_nodes = VALUE #( FOR <network> IN network WHERE ( node CP '++A' )
                             ( <network> ) ).

    LOOP AT current_nodes ASSIGNING FIELD-SYMBOL(<current_node>).

      steps = 0.
      instruction_offset = 0.

      DO.
        IF <current_node>-node+2(1) = 'Z'.
          EXIT.
        ENDIF.

        DATA(direction) = instructions+instruction_offset(1).
        IF direction = 'L'.
          <current_node> = network[ node = <current_node>-left ].
        ELSE.
          <current_node> = network[ node = <current_node>-right ].
        ENDIF.

        steps += 1.
        IF instruction_offset >= max_offset.
          instruction_offset = 0.
        ELSE.
          instruction_offset += 1.
        ENDIF.
      ENDDO.

      INSERT steps INTO TABLE step_counts.

    ENDLOOP.

    " Aaargh, brute forcing this was too brutal... Resolving to maths :D

    SORT step_counts BY table_line.
    DELETE ADJACENT DUPLICATES FROM step_counts COMPARING table_line.

    LOOP AT step_counts ASSIGNING FIELD-SYMBOL(<step_count>).
      INSERT LINES OF get_prime_factors( <step_count> ) INTO TABLE prime_factors.
    ENDLOOP.

    SORT prime_factors BY factor ASCENDING count DESCENDING.
    DELETE ADJACENT DUPLICATES FROM prime_factors COMPARING factor.

    steps = 1.

    LOOP AT prime_factors ASSIGNING FIELD-SYMBOL(<prime_factor>).
      steps *= <prime_factor>-factor ** <prime_factor>-count.
    ENDLOOP.

    result = steps.

  ENDMETHOD.

  METHOD get_network.

    IF the_network IS NOT INITIAL.
      result = the_network.
      RETURN.
    ENDIF.

    LOOP AT input FROM 3 ASSIGNING FIELD-SYMBOL(<line>).
      FIND REGEX '(...) = \((...), (...)\)' IN <line> SUBMATCHES DATA(node) DATA(left) DATA(right).
      INSERT VALUE #( node = node
                      left = left
                      right = right ) INTO TABLE the_network.
    ENDLOOP.

    result = the_network.

  ENDMETHOD.

  METHOD get_prime_factors.

    DATA factor TYPE int8 VALUE 2.
    DATA count TYPE i.

    DATA(rest) = number.

    WHILE rest > 1.
      IF rest MOD factor = 0.
        count += 1.
        rest = rest / factor.
      ELSE.
        IF count > 0.
          INSERT VALUE #( factor = factor
                          count  = count ) INTO TABLE result.
        ENDIF.
        factor += 1.
        count = 0.

      ENDIF.

    ENDWHILE.
    IF count > 0.
      INSERT VALUE #( factor = factor
                      count  = count ) INTO TABLE result.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
