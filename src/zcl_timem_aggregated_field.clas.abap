CLASS zcl_timem_aggregated_field DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !fieldname TYPE feldname.

    METHODS build
      IMPORTING
        !lines        TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE ztimem_aggregated_line_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_value_request,
        value   TYPE string,
        request TYPE verskorrno,
      END OF ty_s_value_request,
      ty_t_value_request TYPE SORTED TABLE OF ty_s_value_request WITH UNIQUE KEY value request.

    DATA fieldname TYPE feldname.

    METHODS get_description
      IMPORTING
                value         TYPE any
                !lines        TYPE ztimem_line_t
      RETURNING VALUE(result) TYPE string.

    METHODS get_description_author
      IMPORTING
                author        TYPE string
                !lines        TYPE ztimem_line_t
      RETURNING VALUE(result) TYPE string.

    METHODS process_line
      IMPORTING
        !line            TYPE ztimem_line
      CHANGING
        aggregated_lines TYPE ztimem_aggregated_line_t
        value_requests   TYPE ty_t_value_request.

    METHODS insert_aggregated_line
      IMPORTING
        !value           TYPE any
      CHANGING
        aggregated_lines TYPE ztimem_aggregated_line_t.

    METHODS fill_missing_data
      IMPORTING
        lines            TYPE ztimem_line_t
        value_requests   TYPE ty_t_value_request
      CHANGING
        aggregated_lines TYPE ztimem_aggregated_line_t.

    METHODS get_request_count_for_value
      IMPORTING
                value          TYPE string
                value_requests TYPE ty_t_value_request
      RETURNING VALUE(result)  TYPE i.
ENDCLASS.



CLASS ZCL_TIMEM_AGGREGATED_FIELD IMPLEMENTATION.


  METHOD build.
    DATA value_requests TYPE ty_t_value_request.

    LOOP AT lines INTO DATA(line).
      process_line(
        EXPORTING
          line = line
        CHANGING
          aggregated_lines = result
          value_requests = value_requests ).
    ENDLOOP.

    " If we only have one line and no value was found... this is pointless
    IF lines( result ) = 1 AND result[ 1 ]-value IS INITIAL.
      REFRESH result.
    ENDIF.

    fill_missing_data(
      EXPORTING
        lines = lines
        value_requests = value_requests
      CHANGING
        aggregated_lines = result ).
  ENDMETHOD.


  METHOD constructor.
    me->fieldname = fieldname.
  ENDMETHOD.


  METHOD fill_missing_data.
    DATA(total_lines) = REDUCE int2(
      INIT x = 0
      FOR aggregated_line IN aggregated_lines
      NEXT x = x + aggregated_line-line_count ).

    LOOP AT aggregated_lines ASSIGNING FIELD-SYMBOL(<aggregated_line>).
      <aggregated_line>-description = get_description( value = <aggregated_line>-value lines = lines ).
      <aggregated_line>-percentage = <aggregated_line>-line_count / total_lines.
      <aggregated_line>-request_count = get_request_count_for_value(
        value = CONV #( <aggregated_line>-value )
        value_requests = value_requests ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_description.
    result = SWITCH #(
      fieldname
      WHEN 'AUTHOR' THEN get_description_author( author = CONV #( value ) lines = lines )
      WHEN 'REQUEST' THEN NEW zcl_timem_request( CONV #( value ) )->description
      ELSE space ).
  ENDMETHOD.


  METHOD get_description_author.
    result = lines[ author = author ]-author_name.
  ENDMETHOD.


  METHOD get_request_count_for_value.
    result = REDUCE int2(
      INIT x = 0
      FOR value_request IN value_requests
      WHERE ( value = value )
      NEXT x = x + 1 ).
  ENDMETHOD.


  METHOD insert_aggregated_line.
    " Insert so that we're sure it exists later on. If it already exists nothing happens
    DATA aggregated_line LIKE LINE OF aggregated_lines.
    CLEAR aggregated_line.
    aggregated_line-value = value.
    INSERT aggregated_line INTO TABLE aggregated_lines.
  ENDMETHOD.


  METHOD process_line.
    ASSIGN COMPONENT fieldname OF STRUCTURE line TO FIELD-SYMBOL(<field>).

    insert_aggregated_line(
      EXPORTING
        value = <field>
      CHANGING
        aggregated_lines = aggregated_lines ).

    READ TABLE aggregated_lines ASSIGNING FIELD-SYMBOL(<aggregated_line>) WITH KEY value = <field>.

    <aggregated_line>-line_count = <aggregated_line>-line_count + 1.

    " Store fieldname+request (it will not have duplicates because the table has an UNIQUE KEY
    INSERT VALUE #( value = <field> request = line-request )
      INTO TABLE value_requests.
  ENDMETHOD.
ENDCLASS.
