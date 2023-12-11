CLASS zcl_advent_2023_10 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.
    METHODS part_2_nope
      IMPORTING
        input         TYPE zif_advent_2023=>ty_input_table
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_c_1 TYPE c LENGTH 1.
    TYPES ty_c_255 TYPE c LENGTH 255.
    TYPES:
      BEGIN OF ty_guide,
        direction TYPE ty_c_1,
        pipe      TYPE ty_c_1,
        next      TYPE ty_c_1,
      END OF ty_guide.

    DATA guide TYPE HASHED TABLE OF ty_guide WITH UNIQUE KEY direction pipe.
    DATA map TYPE STANDARD TABLE OF ty_c_255 WITH EMPTY KEY. "zif_advent_2023=>ty_input_table.

    METHODS initialize_things
      IMPORTING
        input TYPE zif_advent_2023=>ty_input_table.

    METHODS set_map_with_safety_padding
      IMPORTING
        input TYPE zif_advent_2023=>ty_input_table.

    METHODS determine_start
      EXPORTING
        start_offset TYPE i
        start_line   TYPE i
        direction    TYPE ty_c_1
        start_tile   TYPE ty_c_1.

ENDCLASS.



CLASS zcl_advent_2023_10 IMPLEMENTATION.

  METHOD part_1.

    DATA steps TYPE i.
    DATA pipe  TYPE ty_c_1.
    DATA line  LIKE LINE OF map.

    initialize_things( input ).

    determine_start( IMPORTING start_offset = DATA(x)
                               start_line   = DATA(y)
                               direction    = DATA(direction) ).

    DO.
      CASE direction.
        WHEN 'V'.
          x -= 1.
        WHEN 'U'.
          y -= 1.
        WHEN 'H'.
          x += 1.
        WHEN 'N'.
          y += 1.
      ENDCASE.

      line = map[ y ].
      pipe = line+x(1).

      steps += 1.

      IF pipe = 'S'.
        EXIT.
      ENDIF.

      direction = guide[ direction = direction pipe = pipe ]-next.

    ENDDO.

    DATA(half_the_steps) = steps / 2.

    result = half_the_steps.

  ENDMETHOD.

  METHOD part_2.

    DATA steps      TYPE i.
    DATA pipe       TYPE ty_c_1.
    DATA is_inside TYPE abap_bool.
    DATA line       LIKE LINE OF map.

    initialize_things( input ).

    determine_start( IMPORTING start_offset = DATA(start_offset)
                               start_line   = DATA(start_line)
                               direction    = DATA(direction)
                               start_tile   = DATA(start_tile) ).

    DATA(x) = start_offset.
    DATA(y) = start_line.

    DO.
      DATA(old_x) = x.
      DATA(old_y) = y.

      line = map[ old_y ].
      pipe = line+old_x(1).

      IF pipe = 'S'.
        line+old_x(1) = start_tile.
        pipe = start_tile.
      ENDIF.
      IF pipe = 'L' AND direction = 'U'.
        line+old_x(1) = '1'.
      ELSEIF pipe = 'J' AND direction = 'U'.
        line+old_x(1) = '2'.

      ELSEIF pipe = 'F' AND direction = 'N'.
        line+old_x(1) = '3'.
      ELSEIF pipe = '7' AND direction = 'N'.
        line+old_x(1) = '4'.

      ELSEIF pipe = '7' AND direction = 'V'.
        line+old_x(1) = '5'.
      ELSEIF pipe = 'J' AND direction = 'V'.
        line+old_x(1) = '6'.

      ELSEIF pipe = 'L' AND direction = 'H'.
        line+old_x(1) = '7'.
      ELSEIF pipe = 'F' AND direction = 'H'.
        line+old_x(1) = '8'.

*      IF pipe = 'L' OR pipe = 'F' OR pipe = '7' OR pipe = 'J'.
*        line+old_x(1) = to_lower( direction ).
      ELSE.
        line+old_x(1) = direction.
      ENDIF.
      map[ old_y ] = line.

      CASE direction.
        WHEN 'V'.
          x -= 1.
        WHEN 'U'.
          y -= 1.
        WHEN 'H'.
          x += 1.
        WHEN 'N'.
          y += 1.
      ENDCASE.

      line = map[ y ].
      pipe = line+x(1).

      IF x = start_offset AND y = start_line.
        EXIT.
      ENDIF.

      direction = guide[ direction = direction pipe = pipe ]-next.

**      pipe = line+x(1).
*      IF pipe = '|' AND ( direction = 'U' or direction = 'N' ).
*        IF is_inside = abap_false.
*          is_inside = abap_true.
*          line+x(1) = 'O'.
*          map[ y ] = line.
*        ELSE.
*          is_inside = abap_false.
*          line+x(1) = 'o'.
*          map[ y ] = line.
*        ENDIF.
*      ELSEIF is_inside = abap_true. " AND ( pipe = 'V' OR pipe = 'H' ). "OR pipe = 'N' ).
*        is_inside = abap_false.
*        line+x(1) = 'o'. "direction.
*        map[ y ] = line.
*      ELSE.
*        line+x(1) = 'x'.
*        map[ y ] = line.
*      ENDIF.
**      IF pipe = '|' AND direction = 'N'.
**        line+x(1) = 'o'. "direction.
**        map[ y ] = line.
**      ENDIF.
*
**      pipe = line+x(1).
**      IF ( pipe = 'V' OR pipe = 'H') AND ( direction = 'U' OR direction = 'N' ).
**        line+x(1) = 'w'. "direction.
**        map[ y ] = line.
**      ENDIF.

    ENDDO.

    DATA(map_width) = strlen( map[ 1 ] ).
    CLEAR direction.

    LOOP AT map FROM 2 ASSIGNING FIELD-SYMBOL(<line>).
      DO map_width TIMES.
        x = sy-index - 1.
        DATA(here) = <line>+x(1).

        IF is_inside = abap_false.
          IF here = 'U' OR here = '1'." OR here = '2'. "OR here = 'N' or here = '3' or here = '4'.
*            <line>+x(1) = 'O'.
            is_inside = abap_true.
          ENDIF.
        ELSE.
          IF here = 'H' OR here = 'V' OR here = '5' OR here = '6' OR here = '7' OR here = '8'.
*            <line>+x(1) = 'o'.
            is_inside = abap_false.
          ELSEIF here = 'U' OR here = 'N' or here = '2'.
            is_inside = abap_false.
          ELSEIF here = 'u' OR here = 'n' OR here = 'h' OR here = 'v' OR here = '3' OR here = '4'.
*            <line>+x(1) = 'o'.
*            is_inside = abap_false.
          ELSE.
            <line>+x(1) = 'I'.
          ENDIF.
        ENDIF.

      ENDDO.

    ENDLOOP.

    FIND ALL OCCURRENCES OF 'I' IN TABLE map MATCH COUNT DATA(inner_tiles).

    result = inner_tiles.

" Row  TABLE_LINE
" ===========================
" 1    ......................
" 2    ..3VVVV53535353V5.....
" 3    ..N8HH4UNUNUNUN82.....
" 4    ..NU.36UNUNUNUN15.....
" 5    .3615741616UN16b1V5...
" 6    .7HH2.74.a.16848H415..
" 7    .....3V6..8482N157415.
" 8    .....74.84UN15N.1574U.
" 9    ......N8272N82N84U.72.
" 10   .....361V5.NU.NUNU....
" 11   .....7HHH2.72.7272....
" 12   ......................
"
" Row  TABLE_LINE
" ===========================
" 1    ......................
" 2    ..3VVVV53535353V5.....
" 3    ..N8HH4UNUNUNUN82.....
" 4    ..NUI36UNUNUNUN15.....
" 5    .3615741616UN16b1V5...
" 6    .7HH2.74.a.16848H415..
" 7    .....3V6..8482N157415.
" 8    .....74.84UN15N.1574UI
" 9    IIIIIIN8272N82N84UI72.
" 10   .....361V5.NUINUNUIIII
" 11   IIIII7HHH2.72.7272....
" 12   ......................




    " Row  TABLE_LINE
    " ===========================
    " 1    ......................
    " 2    ..nVVVVvnvnvnvnVv.....
    " 3    ..NhHHnUNUNUNUNhu.....
    " 4    ..NU.nvUNUNUNUNuv.....
    " 5    .nvuvhnuvuvUNuvbuVv...
    " 6    .hHHu.hn.a.uvhnhHnuv..
    " 7    .....nVv..hnhuNuvhnuv.
    " 8    .....hn.hnUNuvN.uvhnU.
    " 9    ......NhuhuNhuNhnU.hu.
    " 10   .....nvuVv.NU.NUNU....
    " 11   .....hHHHu.hu.huhu....
    " 12   ......................
    "
    " Row  TABLE_LINE
    " ===========================
    " 1    ......................
    " 2    ..nVVVVvnvnvnvnVv.....
    " 3    ..NhHHnOoOoOoOohOIIIII
    " 4    IIoOIovOoOoOoOoOo.....
    " 5    .nvOohnOoOoOoOobOVo...
    " 6    .hHHOIon.a.OohnhHnOo..
    " 7    .....nVv..hnhOoOohnOo.
    " 8    .....hn.hnOoOoN.OohnOI
    " 9    IIIIIIohOoOohOohnOIoOI
    " 10   IIIIIovOVo.NOIoOoOIIII
    " 11   IIIIIoHHHOIoOIoOoOIIII
    " 12   IIIIIIIIIIIIIIIIIIIIII

    " Row  TABLE_LINE
    " ===========================
    " 1    ......................
    " 2    ..nVVVVvnvnvnvnVv.....
    " 3    ..NhHHnOoOoOoOohu.....
    " 4    ..NOIovOoOoOoOouv.....
    " 5    .nvuvhnuvuvOouvbuVv...
    " 6    .hHHu.hn.a.uvhnhHnuv..
    " 7    .....nVv..hnhuNuvhnuv.
    " 8    .....hn.hnOouvN.uvhnOI
    " 9    IIIIIIohuhuNhuNhnOIou.
    " 10   .....nvuVv.NOIoOoOIIII
    " 11   IIIIIoHHHu.hu.huhu....
    " 12   ......................



    " Row  TABLE_LINE
    " ==================
    " 1    .............
    " 2    .............
    " 3    ..hHHHHHHHn..
    " 4    ..UnVVVVVvN..
    " 5    ..UN.....UN..
    " 6    ..UN.....UN..
    " 7    ..UhHn.hHuN..
    " 8    ..U..N.U..N..
    " 9    ..uVVv.uVVv..
    " 10   .............
    " 11   .............
    "
    " Row  TABLE_LINE
    " ==================
    " 1    .............
    " 2    .............
    " 3    ..hHHHHHHHn..
    " 4    ..OoVVVVVvN..
    " 5    ..Oo.....Oo..
    " 6    ..Oo.....Oo..
    " 7    ..OhHo.hHOo..
    " 8    ..OIIo.OIIo..
    " 9    ..OVVvIuVVvII
    " 10   IIIIIIIIIIIII
    " 11   IIIIIIIIIIIII

    " Row  TABLE_LINE
    " ==================
    " 1    .............
    " 2    .............
    " 3    ..FVVVVVVVv..
    " 4    ..NhHHHHHnU..
    " 5    ..NU.....NU..
    " 6    ..NU.....NU..
    " 7    ..NuVv.nVvU..
    " 8    ..N..U.N..U..
    " 9    ..hHHu.hHHu..
    " 10   .............
    " 11   .............
    "
    "
    " Row  TABLE_LINE
    " ==================
    " 1    .............
    " 2    .............
    " 3    ..FVVVVVVVv..
    " 4    ..NhHHHHHnOII
    " 5    IIoOIIIIIoOII
    " 6    IIoOIIIIIoOII
    " 7    IIoOVvIoVvOII
    " 8    IIo..OIo..OII
    " 9    IIhHHuIhHHuII
    " 10   IIIIIIIIIIIII
    " 11   IIIIIIIIIIIII


    " Row  TABLE_LINE
    " ===========================
    " 1    ......................
    " 2    ..hHHHHnhnhnhnhHn.....
    " 3    ..UnVVvNUNUNUNUnv.....
    " 4    ..UN.huNUNUNUNUhn.....
    " 5    .huhnuvhuhuNUhubhHn...
    " 6    .uVVv.uv.a.huFvnVvhn..
    " 7    .....hHu..nvnvUhnuvhn.
    " 8    .....uv.nvNUhnU.hnuvN.
    " 9    ......UnvuvUnvUnvN.uv.
    " 10   .....huhHn.UN.UNUN....
    " 11   .....uVVVv.uv.uvuv....
    " 12   ......................

    " Row  TABLE_LINE
    " ===========================
    " 1    ......................
    " 2    ..hHHHHnhnhnhnhHn.....
    " 3    ..OoVVvNOoOoOoOov.....
    " 4    ..Oo.hOoOoOoOoOho.....
    " 5    .hOhoOvhuhuoOhuIhHo...
    " 6    .OVVvIuvIIIhuIvoVvhn..
    " 7    .....hHOIIovnvOhoOvho.
    " 8    .....OvIovNOhoOIhoOvo.
    " 9    ......OovOvUovOovN.OvI
    " 10   IIIIIhuhHo.Oo.OoOo....
    " 11   .....OVVVvIuvIuvuvIIII
    " 12   IIIIIIIIIIIIIIIIIIIIII


    " Row  TABLE_LINE
    " ==================
    " 1    .............
    " 2    .............
    " 3    ..UHHHHHHHH..
    " 4    ..UVVVVVVUN..
    " 5    ..UN.....UN..
    " 6    ..UN.....UN..
    " 7    ..UNHH.UHHN..
    " 8    ..U..N.U..N..
    " 9    ..VVVN.VVVN..
    " 10   .............
    " 11   .............

    " Row  TABLE_LINE
    " ==================
    " 1    .............
    " 2    .............
    " 3    ..UHHHHHHHN..
    " 4    ..UNVVVVVUN..
    " 5    ..UN.....UN..
    " 6    ..UN.....UN..
    " 7    ..UNHN.UHUN..
    " 8    ..U..N.U..N..
    " 9    ..UVVN.UVVN..
    " 10   .............
    " 11   .............

    " Row  TABLE_LINE
    " ===========================
    " 1    ......................
    " 2    ..NVVVVUNUNUNUNVUIIIII
    " 3    IINUHHNUNUNUNUNUUIIIII
    " 4    IINUINNUNUNUNUNUUIIIII
    " 5    INNUUNNUNUNUNUNbUVUIII
    " 6    INHHUINN.a.UNUNUHNUUII
    " 7    IIIIINVN..UNUUNUUNNUUI
    " 8    IIIIINN.UNUNUUN.UUNNUI
    " 9    IIIIIINUUNUNUUNUNUINUI
    " 10   IIIIINNUVUINUINUNUIIII
    " 11   IIIIINHHHUINUINUNUIIII
    " 12   IIIIIIIIIIIIIIIIIIIIII

    " Row  TABLE_LINE
    " ==================
    " 1    .............
    " 2    .............
    " 3    ..UHHHHHHHH..
    " 4    ..OVVVVVVUo..
    " 5    ..Oo.....Oo..
    " 6    ..Oo.....Oo..
    " 7    ..ONHH.UHHo..
    " 8    ..O..o.O..o..
    " 9    ..VVVN.VVVN..
    " 10   .............
    " 11   .............

    " Row  TABLE_LINE
    " ===========================
    " 1    ......................
    " 2    ..CVVVVCCCCCCCCVC.....
    " 3    ..NCHHCOoOoOoOoCC.....
    " 4    ..NOIoCOoOoOoOoCC.....
    " 5    .CCCCCCCCCCOoCCbCVC...
    " 6    .CHHC.CC.a.CCCCCHCCC..
    " 7    .....CVC..CCCCNCCCCCC.
    " 8    .....CC.CCOoCCN.CCCCOI
    " 9    IIIIIIoCCCCNCCNCCOIoC.
    " 10   .....CCCVC.NOIoOoOIIII
    " 11   IIIIIoHHHC.CC.CCCC....
    " 12   ......................

    " Row  TABLE_LINE
    " ===========================
    " 1    ......................
    " 2    ..hHHHHnhnhnhnhHn.....
    " 3    ..UnVVvNUNUNUNUnv.....
    " 4    ..UN.huNUNUNUNUhn.....
    " 5    .huhnuvhuhuNUhubhHn...
    " 6    .uVVv.uv.a.huFvnVvhn..
    " 7    .....hHu..nvnvUhnuvhn.
    " 8    .....uv.nvNUhnU.hnuvN.
    " 9    ......UnvuvUnvUnvN.uv.
    " 10   .....huhHn.UN.UNUN....
    " 11   .....uVVVv.uv.uvuv....
    " 12   ......................

    " Row  TABLE_LINE
    " ===========================
    " 1    ......................
    " 2    ..hHHHHnhnhnhnhHn.....
    " 3    ..OoVVvNOoOoOoOov.....
    " 4    ..Oo.hOoOoOoOoOho.....
    " 5    .hOhoOvhIhIoOhIIhHo...
    " 6    .OVVvIIvIIIhIIvoVvhn..
    " 7    .....hHOIIovnvOhoOvho.
    " 8    .....OvIovNOhoOIhoOvo.
    " 9    ......OovOvIovOovN.OvI
    " 10   IIIIIhIhHo.Oo.OoOo....
    " 11   .....OVVVvIIvIIvIvIIII
    " 12   IIIIIIIIIIIIIIIIIIIIII

  ENDMETHOD.

  METHOD part_2_nope.

    DATA steps  TYPE i.
    DATA pipe   TYPE ty_c_1.
    DATA line   LIKE LINE OF map.

    initialize_things( input ).

    determine_start( IMPORTING start_offset = DATA(x)
                               start_line   = DATA(y)
                               direction    = DATA(direction) ).

    DO.
      CASE direction.
        WHEN 'V'.
          x -= 1.
        WHEN 'U'.
          y -= 1.
        WHEN 'H'.
          x += 1.
        WHEN 'N'.
          y += 1.
      ENDCASE.

      line = map[ y ].
      pipe = line+x(1).

      steps += 1.
      IF pipe = '|'.
        line+x(1) = 'V'.
      ELSEIF pipe = '-'.
        line+x(1) = 'H'.
      ELSE.
        line+x(1) = 'C'.
      ENDIF.
      map[ y ] = line.
*      ENDIF.
      IF pipe = 'S'.
        EXIT.
      ENDIF.

      DATA(old_direction) = direction.
      direction = guide[ direction = direction pipe = pipe ]-next.

    ENDDO.

    DATA horizontal_x TYPE i.
    DATA vertical_x   TYPE i.
    DATA total_x_line TYPE i.

    DATA(map_width) = strlen( map[ 1 ] ).
    DATA(map_height) = lines( map ).


    DATA vertical_x_count TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA total_x_column TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    DO map_width TIMES.
      x = sy-index - 1.
      INSERT INITIAL LINE INTO TABLE vertical_x_count.

      DATA(x_count) = 0.
      LOOP AT map ASSIGNING FIELD-SYMBOL(<map>) WHERE table_line+x(1) = 'H' OR table_line+x(1) = 'C'.
        x_count += 1.
      ENDLOOP.
      APPEND x_count TO total_x_column.
    ENDDO.

    DO lines( map ) - 2 TIMES.
      y = sy-index + 1.
      line = map[ y ].

      FIND ALL OCCURRENCES OF 'V' IN line MATCH COUNT total_x_line.
      FIND ALL OCCURRENCES OF 'C' IN line MATCH COUNT DATA(plus_h).
      total_x_line += plus_h.
      horizontal_x = 0.

      DO map_width  TIMES.
        x = sy-index.

        DATA(spot) = line+x(1).
        IF spot = 'a'.
          DATA(nisse) = 1.
        ENDIF.
        IF spot = 'V'.
          horizontal_x += 1.
        ENDIF.

        IF spot = 'H'.
          vertical_x_count[ x ] += 1.
        ENDIF.

        IF spot = 'C'.
          horizontal_x += 1.
          vertical_x_count[ x ] += 1.
        ENDIF.

        IF spot = 'V' OR spot = 'H' OR spot = 'C'.
          CONTINUE.
        ENDIF.

        vertical_x = vertical_x_count[ x ].

        IF vertical_x = 0 OR horizontal_x = 0.
          CONTINUE.
        ENDIF.

        DATA(odd_x) = xsdbool( horizontal_x > 0
                           AND horizontal_x < total_x_line
                           AND horizontal_x MOD 2 = 1 ).
        DATA(odd_y) = xsdbool( vertical_x > 0
                           AND vertical_x < total_x_column[ y ]
                           AND vertical_x MOD 2 = 1 ).

        DATA(even_both) = xsdbool( horizontal_x > 0
                           AND horizontal_x < total_x_line
                           AND horizontal_x MOD 2 = 0
                           AND vertical_x > 0
                           AND vertical_x < total_x_column[ y ]
                           AND vertical_x MOD 2 = 0 ).

*        IF ( odd_x = abap_true AND odd_y = abap_false )
*        OR ( odd_y = abap_true AND odd_x = abap_false ).
*          line+x(1) = 'I'.
*          map[ y ] = line.
*        ENDIF.

        IF odd_x = abap_true. "horizontal_x MOD 2 = 1 AND horizontal_x < total_x_line.
          " might be in.
          line+x(1) = 'I'.
          map[ y ] = line.
        ENDIF.

        IF odd_y = abap_true. "vertical_x MOD 2 = 1 AND vertical_x < total_x_column[ y ].
          " might be in.
          line+x(1) = 'I'.
          map[ y ] = line.
        ENDIF.

        IF even_both = abap_true.
          " might be in.
          line+x(1) = 'I'.
          map[ y ] = line.
        ENDIF.

      ENDDO.
    ENDDO.

    FIND ALL OCCURRENCES OF 'I' IN TABLE map MATCH COUNT DATA(inner_tiles).

    result = inner_tiles.

*    result = |todo|.

    " Row  TABLE_LINE
    " ===========================
    " 1    ......................
    " 2    ..XXXXXXXXXXXXXXX.....
    " 3    ..XXXXXXXXXXXXXXX.....
    " 4    ..XX.XXXXXXXXXXXX.....
    " 5    .XXXXXXXXXXXXXX.XXX...
    " 6    .XXXX.XX...XXSXXXXXX..
    " 7    .....XXXIIXXXXXXXXXXX.
    " 8    .....XX.XXXXXXXIXXXXX.
    " 9    ......XXXXXXXXXXXX.XX.
    " 10   .....XXXXXIXXIXXXX....
    " 11   .....XXXXXIXXIXXXX....
    " 12   ......................

    " Row  TABLE_LINE
    " ============================
    " 1    ......................
    " 2    ..CHHHHCCCCCCCCHC.....
    " 3    ..VCHHCVVVVVVVVCC.....
    " 4    ..VV.CCVVVVVVVVCC.....
    " 5    .CCCCCCCCCCVVCC.CHC...
    " 6    .CHHC.CC...CCSCCHCCC..
    " 7    .....CHCIICCCCVCCCCCC.
    " 8    .....CC.CCVVCCVICCCCV.
    " 9    ......VCCCCVCCVCCV.CC.
    " 10   .....CCCHCIVVIVVVVIIIII
    " 11   .....CHHHC.CC.CCCC....
    " 12   ......................

    " Row  TABLE_LINE
    " ============================
    " 1    ......................
    " 2    ..CHHHHCCCCCCCCHC.....
    " 3    ..VCHHCVVVVVVVVCC.....
    " 4    ..VV.CCVVVVVVVVCC.....
    " 5    .CCCCCCCCCCVVCC.CHC...
    " 6    .CHHC.CC...CCSCCHCCC..
    " 7    .....CHCIICCCCVCCCCCC.
    " 8    .....CC.CCVVCCVICCCCV.
    " 9    ......VCCCCVCCVCCV.CC.
    " 10   .....CCCHCIVVIVVVVIIIII
    " 11   .....CHHHC.CC.CCCC....
    " 12   ......................

    " Row  TABLE_LINE
    " ===========================
    " 1    ......................
    " 2    ..CHHHHCCCCCCCCHC.....
    " 3    ..VCHHCVVVVVVVVCC.....
    " 4    ..VV.CCVVVVVVVVCC.....
    " 5    .CCCCCCCCCCVVCCICHC...
    " 6    .CHHC.CC...CCCCCHCCC..
    " 7    .....CHC..CCCCVCCCCCC.
    " 8    .....CC.CCVVCCVICCCCV.
    " 9    ......VCCCCVCCVCCV.CC.
    " 10   .....CCCHC.VV.VVVV....
    " 11   .....CHHHC.CC.CCCC....
    " 12   ......................

    " Row  TABLE_LINE
    " ===========================
    " 1    ......................
    " 2    ..LLLLLULULULULLU.....
    " 3    ..DURRRUDUDUDUDUR.....
    " 4    ..DU.LDUDUDUDUDLU.....
    " 5    .LDLUDRLDLDUDLDbLLU...
    " 6    .DRRR.DR.a.LDURURRLU..
    " 7    .....LLD..URURDLUDRLU.
    " 8    .....DR.URUDLUD.LUDRU.
    " 9    ......DURDRDURDURU.DR.
    " 10   .....LDLLU.DU.DUDU....
    " 11   .....DRRRR.DR.DRDR....
    " 12   ......................


    " Row  TABLE_LINE
    " ==================
    " 1    .............
    " 2    .............
    " 3    ..SRRRRRRRR..
    " 4    ..|F-----7D..
    " 5    ..||.....|D..
    " 6    ..||.....|D..
    " 7    ..|L-7.F-JD..
    " 8    ..|..|.|..D..
    " 9    ..L--J.LLLD..
    " 10   .............
    " 11   .............

    " Row  TABLE_LINE
    " ==================
    " 1    .............
    " 2    .............
    " 3    ..UHHHHHHHH..
    " 4    ..UVVVVVVUN..
    " 5    ..UN.....UN..
    " 6    ..UN.....UN..
    " 7    ..UNHH.UHHN..
    " 8    ..U..N.U..N..
    " 9    ..VVVN.VVVN..
    " 10   .............
    " 11   .............


  ENDMETHOD.

  METHOD set_map_with_safety_padding.

    IF lines( map ) > 0.
      RETURN.
    ENDIF.

    DATA(map_width) = strlen( input[ 1 ] ) + 2.
    APPEND |{ '' WIDTH = map_width PAD = '.' }| TO map.
    LOOP AT input ASSIGNING FIELD-SYMBOL(<input>).
      APPEND |.{ <input> }.| TO map.
    ENDLOOP.
    APPEND |{ '' WIDTH = map_width PAD = '.' }| TO map.

  ENDMETHOD.

  METHOD initialize_things.

    set_map_with_safety_padding( input ).

    guide = VALUE #( direction = 'V' ( pipe = 'L' next = 'U' )
                                     ( pipe = '-' next = 'V' )
                                     ( pipe = 'F' next = 'N' )
                     direction = 'U' ( pipe = '7' next = 'V' )
                                     ( pipe = '|' next = 'U' )
                                     ( pipe = 'F' next = 'H' )
                     direction = 'H' ( pipe = 'J' next = 'U' )
                                     ( pipe = '-' next = 'H' )
                                     ( pipe = '7' next = 'N' )
                     direction = 'N' ( pipe = 'J' next = 'V' )
                                     ( pipe = '|' next = 'N' )
                                     ( pipe = 'L' next = 'H' ) ).

  ENDMETHOD.

  METHOD determine_start.

    FIND FIRST OCCURRENCE OF 'S' IN TABLE map
      MATCH LINE start_line
      MATCH OFFSET start_offset.

    DATA(line) = map[ start_line ].

    DATA(offset) = start_offset - 1.
    DATA(left) = line+offset(1).

    offset += 2.
    DATA(right) = line+offset(1).

    line = map[ start_line - 1 ].
    DATA(up) = line+start_offset(1).

    line = map[ start_line + 1 ].
    DATA(down) = line+start_offset(1).

    DATA(link_left) = xsdbool( matches( val = left regex = 'L|-|F' ) ).

    DATA(link_right) = xsdbool( matches( val = right regex = 'J|-|7' ) ).

    DATA(link_up) = xsdbool( matches( val = up regex = '7|\||F' ) ).

    DATA(link_down) = xsdbool( matches( val = down regex = 'J|\||L' ) ).

    IF link_left = abap_true AND link_up = abap_true.
      direction = 'U'.
      start_tile = 'J'.
      RETURN.
    ENDIF.

    IF link_left = abap_true AND link_right = abap_true.
      direction = 'H'.
      start_tile = '-'.
      RETURN.
    ENDIF.

    IF link_left = abap_true AND link_down = abap_true.
      direction = 'N'.
      start_tile = '7'.
      RETURN.
    ENDIF.

    IF link_up = abap_true AND link_right = abap_true.
      direction = 'H'.
      start_tile = 'L'.
      RETURN.
    ENDIF.

    IF link_up = abap_true AND link_down = abap_true.
      direction = 'U'.
      start_tile = '|'.
      RETURN.
    ENDIF.

    IF link_right = abap_true AND link_down = abap_true.
      direction = 'H'.
      start_tile = 'F'.
      RETURN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
