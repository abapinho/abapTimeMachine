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
      DATA(one_system) = |<a style="text-decoration:none" href="" title="Return code { system-subrc } @ { system-date DATE = USER } { system-time TIME = USER }">{ system-sysid }</a>|.
      IF system-subrc > 4.
        one_system = |<strike>{ one_system }</strike>|.
      ENDIF.
      result = |{ result } { one_system }|.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_summary_author.
    summary-title = 'Contributors'.   ##NO_TEXT
    summary-value_title = 'Username'. ##NO_TEXT
    summary-text1_title = 'Name'.     ##NO_TEXT
    LOOP AT summary-lines REFERENCE INTO DATA(line).
      line->text1 = NEW zcl_timem_author( )->get_name( CONV #( line->value ) ).
      line->value = |<a href="SAPEVENT:author?{ line->value }">{ line->value }</a>|.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_summary_request.
    summary-title = 'Requests'.          ##NO_TEXT
    summary-value_title = 'Request'.     ##NO_TEXT
    summary-text1_title = 'Description'. ##NO_TEXT
    summary-text2_title = 'Systems'.     ##NO_TEXT
    LOOP AT summary-lines REFERENCE INTO DATA(line).
      DATA(request) = NEW zcl_timem_request( CONV #( line->value ) ).
      line->text1 = request->description.
      line->text2 = build_request_imported_systems( request ).
      line->value = |<a href="SAPEVENT:request?{ line->value }">{ line->value }</a>|.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_timem_userexit~before_rendering.
    RETURN.
  ENDMETHOD.


  METHOD zif_timem_userexit~modify_asset_content.
    RETURN.
  ENDMETHOD.


  METHOD zif_timem_userexit~modify_parts.
    RETURN.
  ENDMETHOD.


  METHOD zif_timem_userexit~modify_part_list.
    RETURN.
  ENDMETHOD.


  METHOD zif_timem_userexit~modify_summary.
    CASE summary-fieldname.
      WHEN zif_timem_consts=>fieldname-author.
        modify_summary_author( CHANGING summary = summary ).
      WHEN zif_timem_consts=>fieldname-request.
        modify_summary_request( CHANGING summary = summary ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_timem_userexit~on_sapevent.
    RETURN.
  ENDMETHOD.
ENDCLASS.
