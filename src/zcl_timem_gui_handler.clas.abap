"! HTML screen event handler to deal with user interaction during the HTML
"! presentation. It will decode the request and process it depending on the
"! requested action. For example, it will navigate to the requested source code.
CLASS zcl_timem_gui_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !gui TYPE REF TO zcl_timem_gui .

    "! Handler method
    "! @parameter action | Action
    "! @parameter getdata | Data details
    METHODS on_sapevent
          FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING
          !action
          !getdata .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA gui TYPE REF TO zcl_timem_gui.
    DATA userexits TYPE REF TO zcl_timem_userexits.

    METHODS display_request
      IMPORTING
        request TYPE trkorr.

    METHODS display_user
      IMPORTING
        user TYPE uname.

    METHODS display_source
      IMPORTING
        type        TYPE versobjtyp
        object_name TYPE versobjnam.

    METHODS display_version.

    METHODS decode_source_type_and_name
      IMPORTING
        i_getdata     TYPE c
      EXPORTING
        e_type        TYPE versobjtyp
        e_object_name TYPE versobjnam.

    METHODS revert
      IMPORTING
        ts TYPE timestamp.
ENDCLASS.



CLASS ZCL_TIMEM_GUI_HANDLER IMPLEMENTATION.


  METHOD constructor.
    me->gui = gui.
    userexits = NEW #( ).
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
        i_trkorr = request.
  ENDMETHOD.


  METHOD display_source.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = object_name
        object_type         = type
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
        i_username       = user
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
        gui->display( ).
      CATCH zcx_timem.
        RETURN. " Ignore error
    ENDTRY.
  ENDMETHOD.


  METHOD on_sapevent.
    DATA ts TYPE timestamp.

    action = condense( action ).
    getdata = condense( getdata ).
    CASE action.
      WHEN 'author'.
        display_user( CONV #( getdata ) ).

      WHEN 'request'.
        display_request( CONV #( getdata ) ).

      WHEN 'revert'.
        " Revert to the requested timestmap
        ts = CONV #( getdata+7 ).
        revert( ts ).
        " And then display the new present time
        GET TIME STAMP FIELD ts.
        zcl_timem_options=>get_instance( )->set( timestamp = ts ).
        display_version( ).

      WHEN 'source'.
        decode_source_type_and_name(
          EXPORTING
            i_getdata = getdata
          IMPORTING
            e_type = DATA(type)
            e_object_name = DATA(object_name) ).
        display_source( type        = type
                        object_name = object_name ).

      WHEN 'timestamp'.
        " Depending on the link, getdata may be just the timestamp xxx or be like timestamp=xxx
        ts = COND #( WHEN getdata(10) = 'timestamp=' THEN getdata+10 ELSE getdata ).
        zcl_timem_options=>get_instance( )->set( timestamp = ts ).
        display_version( ).

      WHEN OTHERS.
        userexits->on_sapevent(
          action  = action
          getdata = getdata ).
    ENDCASE.
  ENDMETHOD.


  METHOD revert.
    TRY.
        gui->revert( ts ).
      CATCH zcx_timem INTO DATA(exc).
        DATA(text) = exc->get_text( ).
        MESSAGE text TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
