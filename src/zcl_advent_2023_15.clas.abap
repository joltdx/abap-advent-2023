CLASS zcl_advent_2023_15 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS hash
      IMPORTING
        characters    TYPE string
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.



CLASS zcl_advent_2023_15 IMPLEMENTATION.

  METHOD part_1.

    DATA sum TYPE i.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      SPLIT <line> AT ',' INTO TABLE DATA(steps).
      LOOP AT steps ASSIGNING FIELD-SYMBOL(<step>).
        sum += hash( <step> ).
      ENDLOOP.
    ENDLOOP.

    result = sum.

  ENDMETHOD.

  METHOD part_2.

    TYPES:
      BEGIN OF ty_lens,
        label        TYPE string,
        focal_length TYPE i,
      END OF ty_lens.
    TYPES ty_lens_tt TYPE STANDARD TABLE OF ty_lens WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_box,
        box    TYPE i,
        lenses TYPE ty_lens_tt,
      END OF ty_box.
    TYPES ty_boxes_tt TYPE STANDARD TABLE OF ty_box WITH EMPTY KEY.

    DATA boxes TYPE ty_boxes_tt.
    DATA label TYPE string.
    DATA focal_length TYPE i.
    DATA box TYPE i.
    DATA sum TYPE i.

    DO 256 TIMES.
      APPEND VALUE #( box = sy-index - 1 ) TO boxes.
    ENDDO.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      SPLIT <line> AT ',' INTO TABLE DATA(steps).
      LOOP AT steps ASSIGNING FIELD-SYMBOL(<step>).
        IF <step> CS '-'.
          SPLIT <step> AT '-' INTO label DATA(there_is_no_more).

          box = hash( label ).

          READ TABLE boxes ASSIGNING FIELD-SYMBOL(<box>) WITH KEY box = box.
          READ TABLE <box>-lenses WITH KEY label = label TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            DELETE <box>-lenses INDEX sy-tabix.
          ENDIF.

        ELSE.
          SPLIT <step> AT '=' INTO label DATA(step_focal_length).
          focal_length = step_focal_length.

          box = hash( label ).

          READ TABLE boxes ASSIGNING <box> WITH KEY box = box.
          READ TABLE <box>-lenses WITH KEY label = label ASSIGNING FIELD-SYMBOL(<lens>).
          IF sy-subrc = 0.
            <lens>-focal_length = focal_length.
          ELSE.
            APPEND VALUE #( label = label
                            focal_length = focal_length ) TO <box>-lenses.
          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDLOOP.

    LOOP AT boxes ASSIGNING <box>.
      DATA(box_score) = <box>-box + 1.
      LOOP AT <box>-lenses ASSIGNING <lens>.
        DATA(lens_score) = sy-tabix.
        sum += box_score * lens_score * <lens>-focal_length.
      ENDLOOP.
    ENDLOOP.

    result = sum.

  ENDMETHOD.

  METHOD hash.

    DO strlen( characters ) TIMES.
      DATA(offset) = sy-index - 1.
      DATA(char) = characters+offset(1).
      DATA(ascii) = CONV i( cl_abap_conv_out_ce=>uccp( char ) ).
      result += ascii.
      result *= 17.
      result = result MOD 256.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
