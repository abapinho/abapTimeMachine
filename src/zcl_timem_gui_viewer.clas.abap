"! Takes a deep structure with all the information of the requested object,
"! renders the HTML and CSS assets based on the requested theme and displays it.
CLASS zcl_timem_gui_viewer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_theme,
        light TYPE ztimem_theme VALUE 'LIGHT',
        dark  TYPE ztimem_theme VALUE 'DARK',
      END OF c_theme .

    "! Constructor which takes a theme as input
    "! @parameter i_theme | Theme name
    METHODS constructor
      IMPORTING
        !io_handler TYPE REF TO zcl_timem_gui_handler .
    "! Takes a deep structure with all the information of the object, renders
    "! the HTML and CSS assets and displays them.
    METHODS render
      IMPORTING
        !data TYPE ztimem_data
      RAISING
        zcx_timem .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA html_viewer TYPE REF TO cl_gui_html_viewer.
    DATA userexits TYPE REF TO zcl_timem_userexits.

    METHODS add_asset
      IMPORTING
        !asset        TYPE REF TO zif_timem_asset
      RETURNING
        VALUE(result) TYPE w3url .

    METHODS string_2_xstring
      IMPORTING
        !input        TYPE string
      RETURNING
        VALUE(result) TYPE xstring .

    METHODS register_events
      IMPORTING
        io_handler TYPE REF TO zcl_timem_gui_handler.

    CLASS-METHODS xstring_2_bintab
      IMPORTING
        !xstr         TYPE xstring
      RETURNING
        VALUE(result) TYPE lvc_t_mime .
ENDCLASS.



CLASS ZCL_TIMEM_GUI_VIEWER IMPLEMENTATION.


  METHOD add_asset.
    DATA(content) = asset->get_content( ).

    userexits->modify_asset_content(
       EXPORTING
         subtype = asset->get_subtype( )
       CHANGING
         content = content ).

    DATA(xstr) = string_2_xstring( content ).

    DATA(t_bintab) = xstring_2_bintab( xstr ).

    html_viewer->load_data(
      EXPORTING
        url                    =  asset->get_url( )
        type                   = 'text'
        subtype                = asset->get_subtype( )
      CHANGING
        data_table             = t_bintab
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    result = asset->get_url( ).
  ENDMETHOD.


  METHOD constructor.
    userexits = NEW #( ).
    html_viewer = NEW cl_gui_html_viewer( parent                   = cl_gui_container=>screen0
                                          query_table_disabled     = abap_true ).
    register_events( io_handler ).
  ENDMETHOD.


  METHOD register_events.
    DATA t_event TYPE cntl_simple_events.
    t_event = VALUE #( ( appl_event = abap_true
                         eventid    = html_viewer->m_id_sapevent ) ).
    html_viewer->set_registered_events( t_event ).
    SET HANDLER io_handler->on_sapevent FOR html_viewer.
  ENDMETHOD.


  METHOD render.
    SKIP. " Creates the screen0 container
    add_asset( NEW zcl_timem_asset_factory( )->create_instance(
      asset_type = zif_timem_consts=>asset_type-css
      data       = data ) ).

    DATA(url) = add_asset( NEW zcl_timem_asset_factory( )->create_instance(
      asset_type = zif_timem_consts=>asset_type-html
      data       = data ) ).

    html_viewer->show_url(
      EXPORTING
        url = url
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD string_2_xstring.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = input
      IMPORTING
        buffer = result
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD xstring_2_bintab.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = xstr
      TABLES
        binary_tab = result.
  ENDMETHOD.
ENDCLASS.
