CLASS zcl_timem_userexit_default DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_timem_userexit .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS modify_summary_author
      CHANGING
        !summary TYPE ztimem_summary
      RAISING
        zcx_timem.

    METHODS modify_summary_request
      CHANGING
        !summary TYPE ztimem_summary
      RAISING
        zcx_timem.

    METHODS build_request_imported_systems
      IMPORTING
        request       TYPE REF TO zcl_timem_request
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.



CLASS ZCL_TIMEM_USEREXIT_DEFAULT IMPLEMENTATION.


  METHOD build_request_imported_systems.
    LOOP AT request->get_imported_systems( ) INTO DATA(system) WHERE sysid <> sy-sysid.
      result = |{ result } { system-sysid }({ system-subrc })|.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_summary_author.
    summary-title = 'Contributors'.
    summary-value_title = 'Username'.
    summary-text1_title = 'Name'.
    LOOP AT summary-lines REFERENCE INTO DATA(line).
      line->text1 = NEW zcl_timem_author( )->get_name( CONV #( line->value ) ).
      line->value = |<a href="SAPEVENT:author?{ line->value }">{ line->value }</a>|.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_summary_request.
    summary-title = 'Requests'.
    summary-value_title = 'Request'.
    summary-text1_title = 'Description'.
    summary-text2_title = 'Systems'.
    LOOP AT summary-lines REFERENCE INTO DATA(line).
      DATA(request) = NEW zcl_timem_request( CONV #( line->value ) ).
      line->text1 = request->description.
      line->text2 = build_request_imported_systems( request ).
      line->value = |<a href="SAPEVENT:request?{ line->value }">{ line->value }</a>|.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_timem_userexit~modify_summary.
    CASE summary-fieldname.
      WHEN zif_timem_consts=>fieldname-author.
        modify_summary_author( CHANGING summary = summary ).
      WHEN zif_timem_consts=>fieldname-request.
        modify_summary_request( CHANGING summary = summary ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
