CLASS zcl_blame_html DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    METHODS add
      IMPORTING
        !i_data TYPE any .

    METHODS render
      IMPORTING
        !iv_no_indent_jscss TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(r_html)       TYPE string .

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_indent_size TYPE i VALUE 2 ##NO_TEXT.

    TYPES:
      BEGIN OF ty_indent_context,
        no_indent_jscss TYPE abap_bool,
        within_style    TYPE abap_bool,
        within_js       TYPE abap_bool,
        indent          TYPE i,
        indent_str      TYPE string,
      END OF ty_indent_context,

      BEGIN OF ty_study_result,
        style_open   TYPE abap_bool,
        style_close  TYPE abap_bool,
        script_open  TYPE abap_bool,
        script_close TYPE abap_bool,
        tag_close    TYPE abap_bool,
        curly_close  TYPE abap_bool,
        openings     TYPE i,
        closings     TYPE i,
        singles      TYPE i,
      END OF ty_study_result.

    CLASS-DATA: go_single_tags_re TYPE REF TO cl_abap_regex.

    DATA gt_buffer TYPE string_table.

    METHODS indent_line
      CHANGING
        cs_context TYPE ty_indent_context
        cv_line    TYPE string.

    METHODS study_line
      IMPORTING
        iv_line          TYPE string
        is_context       TYPE ty_indent_context
      RETURNING
        VALUE(rs_result) TYPE ty_study_result.
ENDCLASS.



CLASS zcl_blame_html IMPLEMENTATION.
  METHOD class_constructor.
    CREATE OBJECT go_single_tags_re
      EXPORTING
        pattern     = '<(AREA|BASE|BR|COL|COMMAND|EMBED|HR|IMG|INPUT|LINK|META|PARAM|SOURCE|!)'
        ignore_case = abap_false.
  ENDMETHOD.


  METHOD add.
    DATA: lv_type TYPE c,
          lo_html TYPE REF TO zcl_blame_html.

    FIELD-SYMBOLS: <lt_tab> TYPE string_table.

    DESCRIBE FIELD i_data TYPE lv_type. " Describe is faster than RTTI classes

    CASE lv_type.
      WHEN 'C' OR 'g'.  " Char or string
        APPEND i_data TO gt_buffer.
      WHEN 'h'.         " Table
        ASSIGN i_data TO <lt_tab>. " Assuming table of strings ! Will dump otherwise
        APPEND LINES OF <lt_tab> TO gt_buffer.
      WHEN 'r'.         " Object ref
        ASSERT i_data IS BOUND. " Dev mistake
        TRY.
            lo_html ?= i_data.
          CATCH cx_sy_move_cast_error.
            ASSERT 1 = 0. " Dev mistake
        ENDTRY.
        APPEND LINES OF lo_html->gt_buffer TO gt_buffer.
      WHEN OTHERS.
        ASSERT 1 = 0. " Dev mistake
    ENDCASE.
  ENDMETHOD.

  METHOD render.
    DATA s_context TYPE ty_indent_context.
    DATA t_temp TYPE string_table.

    s_context-no_indent_jscss = iv_no_indent_jscss.

    LOOP AT gt_buffer ASSIGNING FIELD-SYMBOL(<lv_line>).
      APPEND <lv_line> TO t_temp ASSIGNING FIELD-SYMBOL(<lv_line_c>).
      indent_line( CHANGING cs_context = s_context cv_line = <lv_line_c> ).
    ENDLOOP.

    CONCATENATE LINES OF t_temp INTO r_html SEPARATED BY cl_abap_char_utilities=>newline.
  ENDMETHOD.


  METHOD indent_line.
    DATA: ls_study TYPE ty_study_result,
          lv_x_str TYPE string.

    ls_study = study_line(
      is_context = cs_context
      iv_line    = cv_line ).

    " First closing tag - shift back exceptionally
    IF ( ls_study-script_close = abap_true
        OR ls_study-style_close = abap_true
        OR ls_study-curly_close = abap_true
        OR ls_study-tag_close = abap_true )
        AND cs_context-indent > 0.
      lv_x_str = repeat( val = ` ` occ = ( cs_context-indent - 1 ) * c_indent_size ).
      cv_line  = lv_x_str && cv_line.
    ELSE.
      cv_line = cs_context-indent_str && cv_line.
    ENDIF.

    " Context status update
    CASE abap_true.
      WHEN ls_study-script_open.
        cs_context-within_js    = abap_true.
        cs_context-within_style = abap_false.
      WHEN ls_study-style_open.
        cs_context-within_js    = abap_false.
        cs_context-within_style = abap_true.
      WHEN ls_study-script_close OR ls_study-style_close.
        cs_context-within_js    = abap_false.
        cs_context-within_style = abap_false.
        ls_study-closings       = ls_study-closings + 1.
    ENDCASE.

    " More-less logic chosen due to possible double tags in a line '<a><b>'
    IF ls_study-openings <> ls_study-closings.
      IF ls_study-openings > ls_study-closings.
        cs_context-indent = cs_context-indent + 1.
      ELSEIF cs_context-indent > 0. " AND ls_study-openings < ls_study-closings
        cs_context-indent = cs_context-indent - 1.
      ENDIF.
      cs_context-indent_str = repeat( val = ` ` occ = cs_context-indent * c_indent_size ).
    ENDIF.
  ENDMETHOD.


  METHOD study_line.
    DATA: lv_line TYPE string,
          lv_len  TYPE i.

    lv_line = to_upper( shift_left( val = iv_line sub = ` ` ) ).
    lv_len  = strlen( lv_line ).

    " Some assumptions for simplification and speed
    " - style & scripts tag should be opened/closed in a separate line
    " - style & scripts opening and closing in one line is possible but only once

    " TODO & Issues
    " - What if the string IS a well formed html already not just single line ?

    IF is_context-within_js = abap_true OR is_context-within_style = abap_true.

      IF is_context-within_js = abap_true AND lv_len >= 8 AND lv_line(8) = '</SCRIPT'.
        rs_result-script_close = abap_true.
      ELSEIF is_context-within_style = abap_true AND lv_len >= 7 AND lv_line(7) = '</STYLE'.
        rs_result-style_close = abap_true.
      ENDIF.

      IF is_context-no_indent_jscss = abap_false.
        IF lv_len >= 1 AND lv_line(1) = '}'.
          rs_result-curly_close = abap_true.
        ENDIF.

        FIND ALL OCCURRENCES OF '{' IN lv_line MATCH COUNT rs_result-openings.
        FIND ALL OCCURRENCES OF '}' IN lv_line MATCH COUNT rs_result-closings.
      ENDIF.

    ELSE.
      IF lv_len >= 7 AND lv_line(7) = '<SCRIPT'.
        FIND FIRST OCCURRENCE OF '</SCRIPT' IN lv_line.
        IF sy-subrc > 0. " Not found
          rs_result-script_open = abap_true.
        ENDIF.
      ENDIF.
      IF lv_len >= 6 AND lv_line(6) = '<STYLE'.
        FIND FIRST OCCURRENCE OF '</STYLE' IN lv_line.
        IF sy-subrc > 0. " Not found
          rs_result-style_open = abap_true.
        ENDIF.
      ENDIF.
      IF lv_len >= 2 AND lv_line(2) = '</'.
        rs_result-tag_close = abap_true.
      ENDIF.

      FIND ALL OCCURRENCES OF '<'  IN lv_line MATCH COUNT rs_result-openings.
      FIND ALL OCCURRENCES OF '</' IN lv_line MATCH COUNT rs_result-closings.
      FIND ALL OCCURRENCES OF REGEX go_single_tags_re IN lv_line MATCH COUNT rs_result-singles.
      rs_result-openings = rs_result-openings - rs_result-closings - rs_result-singles.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
