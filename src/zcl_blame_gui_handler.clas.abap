"! HTML screen event handler to deal with user interaction during the HTML
"! presentation. It will decode the request and process it depending on the
"! requested action. For example, it will navigate to the requested source code.
CLASS zcl_blame_gui_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_gui TYPE REF TO zcl_blame_gui.


    "! Handler method
    "! @parameter action | Action
    "! @parameter getdata | Data details
    METHODS on_html_events
          FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING
          !action
          !getdata .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA go_gui TYPE REF TO zcl_blame_gui.

    METHODS display_request
      IMPORTING
        i_request TYPE trkorr.

    METHODS display_user
      IMPORTING
        i_user TYPE uname.

    METHODS display_source
      IMPORTING
        i_type        TYPE versobjtyp
        i_object_name TYPE versobjnam.

    METHODS display_version.

    METHODS decode_source_type_and_name
      IMPORTING
        i_getdata     TYPE c
      EXPORTING
        e_type        TYPE versobjtyp
        e_object_name TYPE versobjnam.
ENDCLASS.



CLASS zcl_blame_gui_handler IMPLEMENTATION.


  METHOD constructor.
    go_gui = io_gui.
  ENDMETHOD.


  METHOD decode_source_type_and_name.
    DATA mtdkey TYPE seocpdkey.
    SPLIT i_getdata AT '|' INTO e_type e_object_name.
    SHIFT e_type LEFT DELETING LEADING space.
    SHIFT e_object_name LEFT DELETING LEADING space.

    CASE e_type.
      WHEN 'CPUB'.
        e_object_name = cl_oo_classname_service=>get_pubsec_name( CONV #( e_object_name ) ).
        e_type = 'PROG'.

      WHEN 'CPRO'.
        e_object_name = cl_oo_classname_service=>get_prosec_name( CONV #( e_object_name ) ).
        e_type = 'PROG'.

      WHEN 'CPRI'.
        e_object_name = cl_oo_classname_service=>get_prisec_name( CONV #( e_object_name ) ).
        e_type = 'PROG'.

      WHEN 'CDEF' OR 'CINC' OR 'CMAC' OR 'REPS' OR 'CCAU'.
        " object_name is already the include so treat it as a program
        e_type = 'PROG'.

      WHEN 'METH'.
        SPLIT e_object_name AT space INTO mtdkey-clsname mtdkey-cpdname.
        cl_oo_classname_service=>get_method_include(
          EXPORTING
            mtdkey                = mtdkey
          RECEIVING
            result                = e_object_name
          EXCEPTIONS
            class_not_existing    = 1
            method_not_existing   = 2
            OTHERS                = 3 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        e_type = 'PROG'.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.


  METHOD display_request.
    CALL FUNCTION 'TR_DISPLAY_REQUEST'
      EXPORTING
        i_trkorr = i_request.
  ENDMETHOD.


  METHOD display_source.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = i_object_name
        object_type         = i_type
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD display_user.
    CALL FUNCTION 'SUID_IDENTITY_MAINT'
      EXPORTING
        i_username       = i_user
        i_tcode_mode     = 6
      EXCEPTIONS
        no_authorisation = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD display_version.
    TRY.
        go_gui->display( ).
      CATCH zcx_blame.
        RETURN. " Ignore error
    ENDTRY.
  ENDMETHOD.


  METHOD on_html_events.
    action = condense( action ).
    getdata = condense( getdata ).
    CASE action.
      WHEN 'author'.
        display_user( CONV #( getdata ) ).
      WHEN 'request'.
        display_request( CONV #( getdata ) ).
      WHEN 'source'.
        decode_source_type_and_name(
          EXPORTING
            i_getdata = getdata
          IMPORTING
            e_type = DATA(type)
            e_object_name = DATA(object_name) ).
        display_source( i_type = type
                        i_object_name = object_name ).
      WHEN 'timestamp'.
        " Depending on the link, getdata may be just the timestamp xxx or be like timestamp=xxx
        DATA(ts) = COND timestamp( WHEN getdata(10) = 'timestamp=' THEN getdata+10 ELSE getdata ).
        zcl_blame_options=>get_instance( )->set( i_timestamp = ts ).
        display_version( ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
