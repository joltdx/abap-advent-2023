CLASS zcl_advent_2023_19 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_c_1 TYPE c LENGTH 1.
    TYPES ty_c_3 TYPE c LENGTH 3.

    TYPES:
      BEGIN OF ty_rule,
        field      TYPE ty_c_1,
        comparison TYPE ty_c_1,
        value      TYPE i,
        next       TYPE ty_c_3,
      END OF ty_rule.
    TYPES ty_rules_tt TYPE STANDARD TABLE OF ty_rule WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_workflow,
        workflow TYPE ty_c_3,
        rules    TYPE ty_rules_tt,
      END OF ty_workflow.
    TYPES ty_workflows_tt TYPE HASHED TABLE OF ty_workflow WITH UNIQUE KEY workflow.

    TYPES:
      BEGIN OF ty_part,
        x TYPE i,
        m TYPE i,
        a TYPE i,
        s TYPE i,
      END OF ty_part.
    TYPES ty_parts_tt TYPE STANDARD TABLE OF ty_part WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_comparison,
        field      TYPE ty_c_1,
        comparison TYPE ty_c_1,
        value      TYPE i,
      END OF ty_comparison.
    TYPES ty_comparsons_tt TYPE STANDARD TABLE OF ty_comparison WITH EMPTY KEY.
    TYPES ty_paths_tt TYPE STANDARD TABLE OF ty_comparsons_tt WITH EMPTY KEY.

    DATA workflows TYPE ty_workflows_tt.
    DATA accepting_paths TYPE ty_paths_tt.
    DATA rejecting_paths TYPE ty_paths_tt.

    METHODS parse_input
      IMPORTING
        input     TYPE zif_advent_2023=>ty_input_table
      EXPORTING
        workflows TYPE ty_workflows_tt
        parts     TYPE ty_parts_tt.

    METHODS traverse_workflow
      IMPORTING
        rules        TYPE ty_rules_tt
        paths_so_far TYPE ty_comparsons_tt.

    METHODS debug_print_paths
      IMPORTING
        paths         TYPE ty_paths_tt
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_advent_2023_19 IMPLEMENTATION.

  METHOD part_1.

    DATA accepted TYPE ty_parts_tt.
    DATA next TYPE ty_c_3.
    DATA comparison TYPE ty_c_1.
    DATA value TYPE i.

    parse_input( EXPORTING input     = input
                 IMPORTING workflows = workflows
                           parts     = DATA(parts) ).

    LOOP AT parts ASSIGNING FIELD-SYMBOL(<part>).
      next = 'in'.
      WHILE next <> 'A' AND next <> 'R'.
        ASSIGN workflows[ workflow = next ]-rules TO FIELD-SYMBOL(<rules>).

        LOOP AT <rules> ASSIGNING FIELD-SYMBOL(<rule>).
          CASE <rule>-field.
            WHEN 'x'.
              value = <part>-x.
            WHEN 'm'.
              value = <part>-m.
            WHEN 'a'.
              value = <part>-a.
            WHEN 's'.
              value = <part>-s.
            WHEN OTHERS.
              next = <rule>-next.
              EXIT.
          ENDCASE.

          IF <rule>-comparison = '<' AND value < <rule>-value OR
             <rule>-comparison = '>' AND value > <rule>-value.
            next = <rule>-next.
            EXIT.
          ENDIF.

        ENDLOOP.
      ENDWHILE.

      IF next = 'A'.
        APPEND <part> TO accepted.
      ENDIF.

    ENDLOOP.

    DATA sum TYPE int8.

    LOOP AT accepted ASSIGNING FIELD-SYMBOL(<accepted>).
      sum += <accepted>-x + <accepted>-m + <accepted>-a + <accepted>-s.
    ENDLOOP.

    result = sum.

  ENDMETHOD.

  METHOD part_2.

    parse_input( EXPORTING input     = input
                 IMPORTING workflows = workflows ).

    traverse_workflow( rules        = workflows[ workflow = 'in' ]-rules
                       paths_so_far = VALUE #( ) ).

    TYPES:
      BEGIN OF ty_range,
        x_from TYPE i,
        x_to   TYPE i,
        m_from TYPE i,
        m_to   TYPE i,
        a_from TYPE i,
        a_to   TYPE i,
        s_from TYPE i,
        s_to   TYPE i,
      END OF ty_range.
    TYPES ty_ranges_tt TYPE STANDARD TABLE OF ty_range WITH EMPTY KEY.

    DATA ranges TYPE ty_ranges_tt.

    LOOP AT accepting_paths ASSIGNING FIELD-SYMBOL(<accepting>).
      APPEND VALUE #( x_from = 1 x_to = 4000
                      m_from = 1 m_to = 4000
                      a_from = 1 a_to = 4000
                      s_from = 1 s_to = 4000 ) TO ranges ASSIGNING FIELD-SYMBOL(<range>).

      LOOP AT <accepting> ASSIGNING FIELD-SYMBOL(<acc>).
        IF <acc>-comparison = '>'.
          DATA(value_field) = |{ <acc>-field }_from|.
          ASSIGN COMPONENT value_field OF STRUCTURE <range> TO FIELD-SYMBOL(<value_field>).
          IF <value_field> <= <acc>-value.
            <value_field> = <acc>-value + 1.
          ENDIF.

        ELSE.
          value_field = |{ <acc>-field }_to|.
          ASSIGN COMPONENT value_field OF STRUCTURE <range> TO <value_field>.
          IF <value_field> >= <acc>-value.
            <value_field> = <acc>-value - 1.
          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDLOOP.

    DATA sum TYPE int8.

    LOOP AT ranges ASSIGNING <range>.
      DATA(x) = <range>-x_to - <range>-x_from + 1.
      DATA(m) = <range>-m_to - <range>-m_from + 1.
      DATA(a) = <range>-a_to - <range>-a_from + 1.
      DATA(s) = <range>-s_to - <range>-s_from + 1.

      sum += ( x * m * a * s ).

    ENDLOOP.

    result = sum.

  ENDMETHOD.

  METHOD parse_input.

    " px{a<2006:qkq,m>2090:A,rfg}
    " pv{a>1716:R,A}
    " lnx{m>1548:A,A}
    " rfg{s<537:gd,x>2440:R,A}
    " qs{s>3448:A,lnx}
    " qkq{x<1416:A,crn}
    " crn{x>2662:A,R}
    " in{s<1351:px,qqz}
    " qqz{s>2770:qs,m<1801:hdj,R}
    " gd{a>3333:R,R}
    " hdj{m>838:A,pv}
    "
    " {x=787,m=2655,a=1222,s=2876}
    " {x=1679,m=44,a=2067,s=496}
    " {x=2036,m=264,a=79,s=2244}
    " {x=2461,m=1339,a=466,s=291}
    " {x=2127,m=1623,a=2188,s=1013}

    IF workflows IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      IF <line> IS INITIAL.
        DATA(parts_start_index) = sy-tabix + 1.
        " Workflow is done, skip to parts.
        EXIT.
      ENDIF.

      FIND REGEX '(.*)\{(.*)\}' IN <line> SUBMATCHES DATA(workflow) DATA(rules).

      INSERT VALUE #( workflow = workflow ) INTO TABLE workflows ASSIGNING FIELD-SYMBOL(<workflow>).
      SPLIT rules AT ',' INTO TABLE DATA(rule_tab).
      LOOP AT rule_tab ASSIGNING FIELD-SYMBOL(<rule>).
        IF <rule> CA '><'.
          FIND REGEX '([xmas])([<>])(\d*):(.*)' IN <rule> SUBMATCHES DATA(field) DATA(comparison) DATA(value) DATA(next).
          APPEND VALUE #( field      = field
                          comparison = comparison
                          value      = value
                          next       = next ) TO <workflow>-rules.
        ELSE.
          APPEND VALUE #( next = <rule> ) TO <workflow>-rules.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    LOOP AT input FROM parts_start_index ASSIGNING <line>.
      FIND REGEX 'x=(\d*),m=(\d*),a=(\d*),s=(\d*)' IN <line> SUBMATCHES DATA(x) DATA(m) DATA(a) DATA(s).
      APPEND VALUE #( x = x
                      m = m
                      a = a
                      s = s ) TO parts.
    ENDLOOP.

  ENDMETHOD.

  METHOD traverse_workflow.

    DATA(paths_here) = paths_so_far.

    LOOP AT rules ASSIGNING FIELD-SYMBOL(<rule>).

      IF <rule>-comparison IS NOT INITIAL.
        APPEND <rule> TO paths_here ASSIGNING FIELD-SYMBOL(<last_rule>).
      ENDIF.

      IF <rule>-next = 'A'.
        APPEND paths_here TO accepting_paths.

      ELSEIF <rule>-next = 'R'.
        APPEND paths_here TO rejecting_paths.

      ELSE.
        traverse_workflow( rules        = workflows[ workflow = <rule>-next ]-rules
                           paths_so_far = paths_here ).

      ENDIF.

      IF <rule>-comparison IS NOT INITIAL.
        IF <last_rule>-comparison = '>'.
          <last_rule>-comparison = '<'.
          <last_rule>-value += 1.
        ELSE.
          <last_rule>-comparison = '>'.
          <last_rule>-value -= 1.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD debug_print_paths.

    LOOP AT paths ASSIGNING FIELD-SYMBOL(<paths>).
      LOOP AT <paths> ASSIGNING FIELD-SYMBOL(<rule>).
        IF result IS INITIAL.
          result = |{ <rule>-field } { <rule>-comparison } { <rule>-value }|.
        ELSE.
          result = |{ result }\n{ <rule>-field } { <rule>-comparison } { <rule>-value }|.
        ENDIF.
      ENDLOOP.
      result = |{ result }\n|.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
