CLASS zcl_advent_2023_20 DEFINITION
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
      BEGIN OF ty_module,
        id     TYPE string,
        module TYPE REF TO lif_module,
        type   TYPE string,
      END OF ty_module.
    TYPES ty_modules_tt TYPE HASHED TABLE OF ty_module WITH UNIQUE KEY id.

    TYPES:
      BEGIN OF ty_input,
        id    TYPE string,
        input TYPE string,
      END OF ty_input.
    TYPES ty_inputs_tt TYPE STANDARD TABLE OF ty_input WITH EMPTY KEY.

    METHODS
      initialize_modules
        IMPORTING
          input   TYPE zif_advent_2023=>ty_input_table
        EXPORTING
          modules TYPE ty_modules_tt
          inputs  TYPE ty_inputs_tt.

ENDCLASS.



CLASS zcl_advent_2023_20 IMPLEMENTATION.

  METHOD part_1.

    initialize_modules( EXPORTING input   = input
                        IMPORTING modules = DATA(modules) ).

    DATA queue TYPE ty_sent_pulses_tt.
    " DATA queue_history TYPE ty_sent_pulses_tt.
    DATA sent_low TYPE i.
    DATA sent_high TYPE i.

    DO 1000 TIMES.

      APPEND VALUE #( source = 'button' destination = 'broadcaster' pulse = low ) TO queue.
      " APPEND VALUE #( source = 'button' destination = 'broadcaster' pulse = low ) TO queue_history.

      WHILE queue IS NOT INITIAL.
        DATA(processing) = queue[ 1 ].
        DELETE queue INDEX 1.

        TRY.
            DATA(process_module) = modules[ id = processing-destination ]-module.

            DATA(more_pulses) = process_module->process( signal = VALUE #( source = processing-source
                                                                           pulse  = processing-pulse ) ).

            APPEND LINES OF more_pulses TO queue.
            " APPEND LINES OF more_pulses TO queue_history.

          CATCH cx_sy_itab_line_not_found.
            " Untyped module. Count pulse but do not process

        ENDTRY.

        IF processing-pulse = low.
          sent_low += 1.
        ELSE.
          sent_high += 1.
        ENDIF.

      ENDWHILE.
    ENDDO.

    DATA multiplied TYPE int8.
    multiplied = sent_low * sent_high.

    result = multiplied.

  ENDMETHOD.

  METHOD part_2.

    TYPES:
      BEGIN OF ty_iteration,
        node      TYPE string,
        iteration TYPE i,
      END OF ty_iteration.
    TYPES ty_iterations_tt TYPE STANDARD TABLE OF ty_iteration WITH EMPTY KEY.

    DATA queue TYPE ty_sent_pulses_tt.
    DATA iterations TYPE ty_iterations_tt.
    DATA interesting_nodes TYPE RANGE OF string.
    DATA we_have_them_all TYPE abap_bool.

    initialize_modules( EXPORTING input   = input
                        IMPORTING modules = DATA(modules)
                                  inputs  = DATA(inputs) ).

    " Making some assumptions based on puzzle input... :)
    " rx only has one input
    " that input has four inputs, which in turn have one input each.
    " All those are Conjuncion modules so when all their inputs are high, then send low
    " We want rx to send low
    " That means all inputs to rx need to be high at the same time
    " Which means all their inputs need to be low at the same time.
    " They are the interesting nodes.
    " Find the cycles of low for them...
    " Let's rock!

    " rx only has one input
    DATA(rx_input_module) = inputs[ id = 'rx' ]-input.

    " four nodes input to rx. When all four nodes sends high to the rx_input_module, then it sends low to rx...
    DATA(rx_input_nodes) = VALUE ty_inputs_tt( FOR <in> IN inputs WHERE ( id = rx_input_module )
                                               ( <in> ) ).
    interesting_nodes = VALUE #( FOR <interesting> IN rx_input_nodes
                                 sign   = 'I'
                                 option = 'EQ'
                                 ( low = <interesting>-input ) ).


    " find cycles for each of the interesting nodes...
    WHILE we_have_them_all = abap_false.
      DATA(iteration) = sy-index.

      APPEND VALUE #( source = 'button' destination = 'broadcaster' pulse = low ) TO queue.

      WHILE queue IS NOT INITIAL.
        DATA(processing) = queue[ 1 ].
        DELETE queue INDEX 1.

        TRY.
            DATA(process_module) = modules[ id = processing-destination ]-module.

            DATA(more_pulses) = process_module->process( signal = VALUE #( source = processing-source
                                                                           pulse  = processing-pulse ) ).

            APPEND LINES OF more_pulses TO queue.


          CATCH cx_sy_itab_line_not_found.
            " Untyped module. Count pulse but do not process

        ENDTRY.

        IF processing-pulse = low AND processing-destination IN interesting_nodes.
          IF NOT line_exists( iterations[ node = processing-destination ] ).
            APPEND VALUE #( node      = processing-destination
                            iteration = iteration ) TO iterations.
          ENDIF.
          IF lines( iterations ) = lines( interesting_nodes ).
            we_have_them_all = abap_true.
            EXIT.
          ENDIF.
        ENDIF.

      ENDWHILE.

    ENDWHILE.

    " Multiply cycles to get the one interation that in the darkness binds them
    DATA multiplied TYPE int8 VALUE 1.
    LOOP AT iterations ASSIGNING FIELD-SYMBOL(<iteration>).
      multiplied *= <iteration>-iteration.
    ENDLOOP.

    result = multiplied.

  ENDMETHOD.

  METHOD initialize_modules.

    CLEAR modules.
    CLEAR inputs.

    DATA destinations TYPE ty_destinations_tt.

    " broadcaster -> a, b, c
    " %a -> b
    " %b -> c
    " %c -> inv
    " &inv -> a

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      SPLIT <line> AT space INTO DATA(module) DATA(arrow) DATA(destination_ids).
      SPLIT destination_ids AT ', ' INTO TABLE destinations.

      IF module = 'broadcaster'.
        DATA(module_id) = module.
        INSERT VALUE #( id     = module_id
                        module = NEW lcl_broadcaster( id           = module_id
                                                      destinations = destinations )
                        type   = 'Broadcast' ) INTO TABLE modules.
      ELSE.
        DATA(module_type) = module+0(1).
        module_id = module+1.

        CASE module_type.
          WHEN '%'.    " Flip-flop
            INSERT VALUE #( id     = module_id
                            module = NEW lcl_flip_flop( id           = module_id
                                                        destinations = destinations )
                            type   = 'Flip-flop' ) INTO TABLE modules.

          WHEN '&'.    " Conjunction
            INSERT VALUE #( id     = module_id
                            module = NEW lcl_conjunction( id           = module_id
                                                          destinations = destinations )
                            type   = 'Conjunction' ) INTO TABLE modules.

        ENDCASE.

        LOOP AT destinations ASSIGNING FIELD-SYMBOL(<destination>).
          APPEND VALUE #( id = <destination> input = module_id ) TO inputs.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    " Initialize conjunction modules
    SORT inputs BY id input.
    DELETE ADJACENT DUPLICATES FROM inputs COMPARING id input.
    LOOP AT modules ASSIGNING FIELD-SYMBOL(<module>) WHERE type = 'Conjunction'.
      LOOP AT inputs ASSIGNING FIELD-SYMBOL(<input>) WHERE id = <module>-id.
        CAST lcl_conjunction( <module>-module )->initialize_input( <input>-input ).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
