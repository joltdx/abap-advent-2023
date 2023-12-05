CLASS zcl_advent_2023_05 DEFINITION
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
      BEGIN OF ty_map,
        source_from      TYPE int8,
        source_to        TYPE int8,
        destination_from TYPE int8,
        destination_to   TYPE int8,
        diff             TYPE int8,
      END OF ty_map.
    TYPES ty_map_tt TYPE SORTED TABLE OF ty_map WITH UNIQUE KEY source_from.

    DATA seed_to_soil   TYPE ty_map_tt.
    DATA soil_to_fert   TYPE ty_map_tt.
    DATA fert_to_water  TYPE ty_map_tt.
    DATA water_to_light TYPE ty_map_tt.
    DATA light_to_temp  TYPE ty_map_tt.
    DATA temp_to_humid  TYPE ty_map_tt.
    DATA humid_to_loc   TYPE ty_map_tt.

    DATA input_is_already_parsed TYPE abap_bool.

    METHODS parse_input_into_maps
      IMPORTING
        input TYPE zif_advent_2023=>ty_input_table.

    METHODS get_location_for_seed
      IMPORTING
        seed            TYPE int8
      RETURNING
        VALUE(location) TYPE int8.

    METHODS get_seed_for_location
      IMPORTING
        location    TYPE int8
      RETURNING
        VALUE(seed) TYPE int8.

ENDCLASS.



CLASS zcl_advent_2023_05 IMPLEMENTATION.

  METHOD part_1.

    DATA locations TYPE SORTED TABLE OF int8 WITH UNIQUE KEY table_line.

    parse_input_into_maps( input ).

    SPLIT input[ 1 ] AT ' ' INTO TABLE DATA(seeds).
    DELETE seeds INDEX 1.

    LOOP AT seeds ASSIGNING FIELD-SYMBOL(<seed>).
      INSERT get_location_for_seed( CONV int8( <seed> ) ) INTO TABLE locations.

    ENDLOOP.

    result = locations[ 1 ].

  ENDMETHOD.


  METHOD part_2.

    DATA seed TYPE int8.
    DATA location TYPE int8.
    DATA locations TYPE SORTED TABLE OF int8 WITH UNIQUE KEY table_line.
    DATA valid_seeds TYPE RANGE OF int8.

    parse_input_into_maps( input ).

    SPLIT input[ 1 ] AT ' ' INTO TABLE DATA(seeds).
    DELETE seeds INDEX 1.

    WHILE lines( seeds ) > 0.
      seed = seeds[ 1 ].
      DATA(range_length) = seeds[ 2 ].

      INSERT VALUE #( sign = 'I'
                      option = 'BT'
                      low = seed
                      high = seed + range_length ) INTO TABLE valid_seeds.

      DELETE seeds INDEX 2.
      DELETE seeds INDEX 1.

    ENDWHILE.

    " Brute force this one backwards, because we just want to find lowest valid
    " and data quantities are huge >_<
    DO.
      seed = get_seed_for_location( location ).
      IF seed IN valid_seeds.
        EXIT.
      ENDIF.

      location += 1.

    ENDDO.

    result = location.

  ENDMETHOD.


  METHOD parse_input_into_maps.

    DATA source TYPE int8.
    DATA destination TYPE int8.
    DATA range_length TYPE int8.

    IF input_is_already_parsed = abap_true.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <map> TYPE ty_map_tt.
    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>) FROM 2 WHERE table_line IS NOT INITIAL.
      FIND REGEX '(.+) map:' IN <line> SUBMATCHES DATA(map_name).
      IF sy-subrc = 0.
        CASE map_name.
          WHEN 'seed-to-soil'.
            ASSIGN seed_to_soil TO <map>.
          WHEN 'soil-to-fertilizer'.
            ASSIGN soil_to_fert TO <map>.
          WHEN 'fertilizer-to-water'.
            ASSIGN fert_to_water TO <map>.
          WHEN 'water-to-light'.
            ASSIGN water_to_light TO <map>.
          WHEN 'light-to-temperature'.
            ASSIGN light_to_temp TO <map>.
          WHEN 'temperature-to-humidity'.
            ASSIGN temp_to_humid TO <map>.
          WHEN 'humidity-to-location'.
            ASSIGN humid_to_loc TO <map>.
        ENDCASE.
        CONTINUE.
      ENDIF.

      SPLIT <line> AT ' ' INTO DATA(line_destination) DATA(line_source) DATA(line_range_length).
      INSERT VALUE ty_map( source_from      = line_source
                           source_to        = line_source + line_range_length - 1
                           destination_from = line_destination
                           destination_to   = line_destination + line_range_length - 1
                           diff             = line_destination - line_source ) INTO TABLE <map>.

    ENDLOOP.

    input_is_already_parsed = abap_true.

  ENDMETHOD.

  METHOD get_location_for_seed.

    DATA soil TYPE int8.
    DATA fert TYPE int8.
    DATA water TYPE int8.
    DATA light TYPE int8.
    DATA temp TYPE int8.
    DATA humid TYPE int8.
    DATA diff TYPE int8.

    SELECT SINGLE FROM @seed_to_soil AS map FIELDS diff WHERE source_from <= @seed AND source_to >= @seed INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    soil = seed + diff.

    SELECT SINGLE FROM @soil_to_fert AS map FIELDS diff WHERE source_from <= @soil AND source_to >= @soil INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    fert = soil + diff.

    SELECT SINGLE FROM @fert_to_water AS map FIELDS diff WHERE source_from <= @fert AND source_to >= @fert INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    water = fert + diff.

    SELECT SINGLE FROM @water_to_light AS map FIELDS diff WHERE source_from <= @water AND source_to >= @water INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    light = water + diff.

    SELECT SINGLE FROM @light_to_temp AS map FIELDS diff WHERE source_from <= @light AND source_to >= @light INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    temp = light + diff.

    SELECT SINGLE FROM @temp_to_humid AS map FIELDS diff WHERE source_from <= @temp AND source_to >= @temp INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    humid = temp + diff.

    SELECT SINGLE FROM @humid_to_loc AS map FIELDS diff WHERE source_from <= @humid AND source_to >= @humid INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    location = humid + diff.

  ENDMETHOD.


  METHOD get_seed_for_location.

    DATA soil TYPE int8.
    DATA fert TYPE int8.
    DATA water TYPE int8.
    DATA light TYPE int8.
    DATA temp TYPE int8.
    DATA humid TYPE int8.
    DATA diff TYPE int8.

    SELECT SINGLE FROM @humid_to_loc AS map FIELDS diff WHERE destination_from <= @location AND destination_to >= @location INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    humid = location - diff.

    SELECT SINGLE FROM @temp_to_humid AS map FIELDS diff WHERE destination_from <= @humid AND destination_to >= @humid INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    temp = humid - diff.

    SELECT SINGLE FROM @light_to_temp AS map FIELDS diff WHERE destination_from <= @temp AND destination_to >= @temp INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    light = temp - diff.

    SELECT SINGLE FROM @water_to_light AS map FIELDS diff WHERE destination_from <= @light AND destination_to >= @light INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    water = light - diff.

    SELECT SINGLE FROM @fert_to_water AS map FIELDS diff WHERE destination_from <= @water AND destination_to >= @water INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    fert = water - diff.

    SELECT SINGLE FROM @soil_to_fert AS map FIELDS diff WHERE destination_from <= @fert AND destination_to >= @fert INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    soil = fert - diff.

    SELECT SINGLE FROM @seed_to_soil AS map FIELDS diff WHERE destination_from <= @soil AND destination_to >= @soil INTO @diff.
    IF sy-subrc <> 0.
      diff = 0.
    ENDIF.
    seed = soil - diff.

  ENDMETHOD.

ENDCLASS.
