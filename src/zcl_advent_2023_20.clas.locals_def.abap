*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES ty_pulse TYPE c LENGTH 1.

TYPES:
  BEGIN OF ty_sent_pulse,
    source      TYPE string,
    pulse       TYPE ty_pulse,
    destination TYPE string,
  END OF ty_sent_pulse.
TYPES ty_sent_pulses_tt TYPE STANDARD TABLE OF ty_sent_pulse WITH EMPTY KEY.
TYPES ty_received_pulses TYPE HASHED TABLE OF ty_sent_pulse WITH UNIQUE KEY source.
TYPES ty_destinations_tt TYPE STANDARD TABLE OF string WITH EMPTY KEY.

CONSTANTS high TYPE ty_pulse VALUE 'h'.
CONSTANTS low TYPE ty_pulse VALUE 'l'.

INTERFACE lif_module.

  METHODS process
    IMPORTING
      signal        TYPE ty_sent_pulse
    RETURNING
      VALUE(result) TYPE ty_sent_pulses_tt.

ENDINTERFACE.

CLASS lcl_broadcaster DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_module.

    METHODS constructor
      IMPORTING
        id           TYPE string
        destinations TYPE ty_destinations_tt.

    METHODS get_destinations
      RETURNING
        VALUE(result) TYPE ty_destinations_tt.

  PRIVATE SECTION.
    DATA id TYPE string.
    DATA destinations TYPE ty_destinations_tt.

ENDCLASS.

CLASS lcl_flip_flop DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_module.

    METHODS constructor
      IMPORTING
        id           TYPE string
        destinations TYPE ty_destinations_tt.

  PRIVATE SECTION.
    DATA id TYPE string.
    DATA destinations TYPE ty_destinations_tt.
    DATA state TYPE c.

ENDCLASS.

CLASS lcl_conjunction DEFINITION.

  PUBLIC SECTION.
    INTERFACES lif_module.

    METHODS constructor
      IMPORTING
        id           TYPE string
        destinations TYPE ty_destinations_tt.

    METHODS initialize_input
      IMPORTING
        id TYPE string.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_memory,
        source TYPE string,
        pulse  TYPE ty_pulse,
      END OF ty_memory.

    DATA id TYPE string.
    DATA destinations TYPE ty_destinations_tt.
    DATA memory TYPE HASHED TABLE OF ty_memory WITH UNIQUE KEY source.

ENDCLASS.
