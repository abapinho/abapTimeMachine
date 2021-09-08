"! Abstract class representing a generic syntax highlighter. It was copied from
"! the abapGit project as-is and used as a black box that just works :)
class ZCL_TIMEM_SYNTAX_HIGHLIGHTER definition
  public
  abstract
  create public .

public section.

  class-methods CREATE
    importing
      !IV_FILENAME type STRING
    returning
      value(RO_INSTANCE) type ref to ZCL_TIMEM_SYNTAX_HIGHLIGHTER .
  methods PROCESS_LINE
    importing
      !IV_LINE type STRING
    returning
      value(RESULT) type STRING .
protected section.

  types:
    BEGIN OF ty_match,
        token    TYPE c LENGTH 1,  " Type of matches
        offset   TYPE i,      " Beginning position of the string that should be formatted
        length   TYPE i,      " Length of the string that should be formatted
        text_tag TYPE string, " Type of text tag
      END OF ty_match .
  types:
    ty_match_tt  TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY .
  types:
    BEGIN OF ty_rule,
        regex             TYPE REF TO cl_abap_regex,
        token             TYPE c LENGTH 1,
        style             TYPE string,
        relevant_submatch TYPE i,
      END OF ty_rule .

  constants C_TOKEN_NONE type C value '.' ##NO_TEXT.
  data:
    mt_rules TYPE STANDARD TABLE OF ty_rule .

  methods ADD_RULE
    importing
      !IV_REGEX type STRING
      !IV_TOKEN type C
      !IV_STYLE type STRING
      !IV_SUBMATCH type I optional .
  methods PARSE_LINE
    importing
      !IV_LINE type STRING
    returning
      value(RESULT) type TY_MATCH_TT .
  methods ORDER_MATCHES
  abstract
    importing
      !IV_LINE type STRING
    changing
      !CT_MATCHES type TY_MATCH_TT .
  methods EXTEND_MATCHES
    importing
      !IV_LINE type STRING
    changing
      !CT_MATCHES type TY_MATCH_TT .
  methods FORMAT_LINE
    importing
      !IV_LINE type STRING
      !IT_MATCHES type TY_MATCH_TT
    returning
      value(RESULT) type STRING .
  methods APPLY_STYLE
    importing
      !IV_LINE type STRING
      !IV_CLASS type STRING
    returning
      value(RESULT) type STRING .
private section.
ENDCLASS.



CLASS ZCL_TIMEM_SYNTAX_HIGHLIGHTER IMPLEMENTATION.


  METHOD add_rule.

    DATA ls_rule LIKE LINE OF mt_rules.

    ls_rule-regex = NEW #( pattern     = iv_regex
                           ignore_case = abap_true ).

    ls_rule-token         = iv_token.
    ls_rule-style         = iv_style.
    ls_rule-relevant_submatch = iv_submatch.
    APPEND ls_rule TO mt_rules.

  ENDMETHOD.


  METHOD apply_style.

    DATA lv_escaped TYPE string.

    lv_escaped = escape( val = iv_line  format = cl_abap_format=>e_html_attr ).
    IF iv_class IS NOT INITIAL.
      result = |<span class="{ iv_class }">{ lv_escaped }</span>|.
    ELSE.
      result = lv_escaped.
    ENDIF.

  ENDMETHOD.


  METHOD create.

    " Create instance of highighter dynamically dependent on syntax type
    IF iv_filename CP '*.abap'.
      ro_instance = NEW zcl_timem_syntax_abap( ).
    ELSE.
      CLEAR ro_instance.
    ENDIF.

  ENDMETHOD.


  METHOD extend_matches.

    DATA lv_line_len TYPE i.
    DATA lv_last_pos TYPE i VALUE 0.
    DATA lv_length   TYPE i.
    DATA ls_match    TYPE ty_match.

    lv_line_len = strlen( iv_line ).

    SORT ct_matches BY offset ASCENDING.

    " Add entries refering to parts of text that should not be formatted
    LOOP AT ct_matches ASSIGNING field-symbol(<ls_match>).
      IF <ls_match>-offset > lv_last_pos.
        lv_length = <ls_match>-offset - lv_last_pos.
        ls_match-token  = c_token_none.
        ls_match-offset = lv_last_pos.
        ls_match-length = lv_length.
        INSERT ls_match INTO ct_matches INDEX sy-tabix.
      ENDIF.
      lv_last_pos = <ls_match>-offset + <ls_match>-length.
    ENDLOOP.

    " Add remainder of the string
    IF lv_line_len > lv_last_pos.
      lv_length = lv_line_len - lv_last_pos.
      ls_match-token  = c_token_none.
      ls_match-offset = lv_last_pos.
      ls_match-length = lv_length.
      APPEND ls_match TO ct_matches.
    ENDIF.

  ENDMETHOD.


  METHOD format_line.

    DATA lv_chunk TYPE string.
    DATA ls_rule  LIKE LINE OF mt_rules.

    LOOP AT it_matches ASSIGNING field-symbol(<ls_match>).
      lv_chunk = substring( val = iv_line off = <ls_match>-offset len = <ls_match>-length ).

      " Failed read equals no style
      CLEAR ls_rule.
      READ TABLE mt_rules INTO ls_rule WITH KEY token = <ls_match>-token. "#EC CI_SUBRC

      lv_chunk = me->apply_style( iv_line  = lv_chunk
                                  iv_class = ls_rule-style ).

      result = result && lv_chunk.
    ENDLOOP.

  ENDMETHOD.


  METHOD parse_line.

    DATA lo_regex   TYPE REF TO cl_abap_regex.
    DATA lo_matcher TYPE REF TO cl_abap_matcher.
    DATA lt_result  TYPE match_result_tab.
    DATA ls_match   TYPE ty_match.

    " Process syntax-dependent regex table and find all matches
    LOOP AT mt_rules ASSIGNING FIELD-SYMBOL(<ls_regex>).
      lo_regex   = <ls_regex>-regex.
      lo_matcher = lo_regex->create_matcher( text = iv_line ).
      lt_result  = lo_matcher->find_all( ).

      " Save matches into custom table with predefined tokens
      LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
        CLEAR: ls_match.
        IF <ls_regex>-relevant_submatch = 0.
          ls_match-token  = <ls_regex>-token.
          ls_match-offset = <ls_result>-offset.
          ls_match-length = <ls_result>-length.
          APPEND ls_match TO result.
        ELSE.
          READ TABLE <ls_result>-submatches ASSIGNING FIELD-SYMBOL(<ls_submatch>) INDEX <ls_regex>-relevant_submatch.
          "submatch might be empty if only discarted parts matched
          IF sy-subrc = 0 AND <ls_submatch>-offset >= 0 AND <ls_submatch>-length > 0.
            ls_match-token  = <ls_regex>-token.
            ls_match-offset = <ls_submatch>-offset.
            ls_match-length = <ls_submatch>-length.
            APPEND ls_match TO result.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD process_line.

    DATA: lt_matches TYPE ty_match_tt.

    IF strlen( iv_line ) = 0.
      RETURN.
    ENDIF.

    lt_matches = me->parse_line( iv_line ).

    order_matches( EXPORTING iv_line    = iv_line
                   CHANGING  ct_matches = lt_matches ).

    extend_matches( EXPORTING iv_line    = iv_line
                    CHANGING  ct_matches = lt_matches ).

    result = format_line( iv_line    = iv_line
                          it_matches = lt_matches ).

  ENDMETHOD.
ENDCLASS.
