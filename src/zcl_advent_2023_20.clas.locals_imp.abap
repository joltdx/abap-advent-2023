*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_broadcaster IMPLEMENTATION.

  METHOD lif_module~process.

    result = VALUE #( FOR <destination> IN destinations
                      source = id
                      pulse = low
                      ( destination = <destination> ) ).

  ENDMETHOD.

  METHOD constructor.

    me->id = id.
    me->destinations = destinations.

  ENDMETHOD.

  METHOD get_destinations.

    result = destinations.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_flip_flop IMPLEMENTATION.

  METHOD constructor.

    me->id = id.
    me->destinations = destinations.

    state = low.

  ENDMETHOD.

  METHOD lif_module~process.

    IF signal-pulse = high.
      RETURN.
    ENDIF.

    IF state = low.
      state = high.
    ELSE.
      state = low.
    ENDIF.

    result = VALUE #( FOR <destination> IN destinations
                      source = id
                      pulse  = state
                      ( destination = <destination> ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_conjunction IMPLEMENTATION.

  METHOD constructor.

    me->id = id.
    me->destinations = destinations.

  ENDMETHOD.

  METHOD initialize_input.

    INSERT VALUE #( source = id
                    pulse  = low ) INTO TABLE memory.

  ENDMETHOD.

  METHOD lif_module~process.

    READ TABLE memory WITH KEY source = signal-source ASSIGNING FIELD-SYMBOL(<memory>).
    <memory>-pulse = signal-pulse.

    IF line_exists( memory[ pulse = low ] ).
      DATA(send_signal) = high.
    ELSE.
      send_signal = low.
    ENDIF.

    result = VALUE #( FOR <destination> IN destinations
                      source = id
                      pulse  = send_signal
                      ( destination = <destination> ) ).

  ENDMETHOD.

ENDCLASS.
