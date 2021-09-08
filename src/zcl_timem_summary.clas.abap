CLASS zcl_timem_summary DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !fieldname TYPE name_feld .

    METHODS build
      IMPORTING
        lines         TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE ztimem_summary
      RAISING
        zcx_timem .

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_value_request,
        value   TYPE string,
        request TYPE verskorrno,
      END OF ty_s_value_request,
      ty_t_value_request TYPE SORTED TABLE OF ty_s_value_request WITH UNIQUE KEY value request.

    DATA fieldname TYPE name_feld.

    METHODS build_lines
      IMPORTING
        !lines        TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE ztimem_summary_line_t
      RAISING
        zcx_timem .

    METHODS total_lines_count
      IMPORTING
        summary_lines TYPE ztimem_summary_line_t
      RETURNING
        VALUE(result) TYPE int2.

    METHODS build_value_request_list
      IMPORTING
        !lines        TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE ty_t_value_request.

    METHODS add_line_to_summary_lines
      IMPORTING
        !line          TYPE ztimem_line
      CHANGING
        summary_lines  TYPE ztimem_summary_line_t.

    METHODS calc_missing_data
      IMPORTING
        lines          TYPE ztimem_line_t
      CHANGING
        summary_lines  TYPE ztimem_summary_line_t
      RAISING
        zcx_timem.

    METHODS request_count_for_value
      IMPORTING
                value          TYPE string
                value_requests TYPE ty_t_value_request
      RETURNING VALUE(result)  TYPE i.
ENDCLASS.



CLASS ZCL_TIMEM_SUMMARY IMPLEMENTATION.


  METHOD add_line_to_summary_lines.
    ASSIGN COMPONENT fieldname OF STRUCTURE line TO FIELD-SYMBOL(<field>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Insert if it doesn't yet exist
    IF NOT line_exists( summary_lines[ value = <field> ] ).
      INSERT VALUE #( value = <field> ) INTO TABLE summary_lines. "#EC CI_SUBRC
    ENDIF.

    READ TABLE summary_lines ASSIGNING FIELD-SYMBOL(<summary_line>) WITH KEY value = <field>.
    IF sy-subrc = 0.
      <summary_line>-line_count = <summary_line>-line_count + 1.
    ENDIF.
  ENDMETHOD.


  METHOD build.
    result = VALUE #(
      fieldname = fieldname
      lines = build_lines( lines )
      title = |{ fieldname } list|
      value_title = fieldname ).
    NEW zcl_timem_userexits( )->modify_summary( CHANGING summary = result ).
  ENDMETHOD.


  METHOD build_lines.
    LOOP AT lines INTO DATA(line).
      add_line_to_summary_lines(
        EXPORTING
          line = line
        CHANGING
          summary_lines = result ).
    ENDLOOP.

    " If we only have one line and no value was found... this summary is pointless
    IF lines( result ) = 1 AND result[ 1 ]-value IS INITIAL.
      CLEAR result.
    ENDIF.

    calc_missing_data(
      EXPORTING
        lines = lines
      CHANGING
        summary_lines = result ).
  ENDMETHOD.


  METHOD build_value_request_list.
    LOOP AT lines INTO DATA(line).
      ASSIGN COMPONENT fieldname OF STRUCTURE line TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.
        " Store fieldname+request (it will not have duplicates because the table has a UNIQUE KEY
        INSERT VALUE #( value = <field> request = line-request ) INTO TABLE result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD calc_missing_data.
    DATA(total_lines) = total_lines_count( summary_lines ).
    DATA(value_requests) = build_value_request_list( lines ).
    LOOP AT summary_lines ASSIGNING FIELD-SYMBOL(<summary_line>).
      <summary_line>-percentage = <summary_line>-line_count / total_lines.
      <summary_line>-request_count = request_count_for_value(
        value = CONV #( <summary_line>-value )
        value_requests = value_requests ).
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    me->fieldname = fieldname.
  ENDMETHOD.


  METHOD request_count_for_value.
    result = REDUCE int2(
      INIT x = 0
      FOR value_request IN value_requests
      WHERE ( value = value )
      NEXT x = x + 1 ).
  ENDMETHOD.


  METHOD total_lines_count.
    result = REDUCE int2(
      INIT x = 0
      FOR summary_line IN summary_lines
      NEXT x = x + summary_line-line_count ).
  ENDMETHOD.
ENDCLASS.
