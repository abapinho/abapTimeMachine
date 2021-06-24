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
        !is_parts TYPE ztimem_parts
      RAISING
        zcx_timem .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA go_html_viewer TYPE REF TO cl_gui_html_viewer.

    METHODS add_asset
      IMPORTING
        !io_asset     TYPE REF TO zif_timem_asset
      RETURNING
        VALUE(result) TYPE w3url .

    METHODS string_2_xstring
      IMPORTING
        !i_input      TYPE string
      RETURNING
        VALUE(result) TYPE xstring .

    METHODS register_events
      IMPORTING
        io_handler TYPE REF TO zcl_timem_gui_handler.

    CLASS-METHODS xstring_2_bintab
      IMPORTING
        !i_xstr    TYPE xstring
      EXPORTING
        !e_size    TYPE i
        !et_bintab TYPE lvc_t_mime .
ENDCLASS.



CLASS ZCL_TIMEM_GUI_VIEWER IMPLEMENTATION.


  METHOD add_asset.
    DATA(content) = io_asset->get_content( ).

    NEW zcl_timem_userexits( )->modify_asset_content(
      EXPORTING
        subtype = io_asset->get_subtype( )
      CHANGING
        content = content ).

    DATA(xstr) = string_2_xstring( content ).

    xstring_2_bintab(
      EXPORTING
        i_xstr    = xstr
      IMPORTING
        e_size    = DATA(size)
        et_bintab = DATA(t_bintab) ).

    go_html_viewer->load_data(
      EXPORTING
        url                    =  io_asset->get_url( )
        type                   = 'text'
        subtype                = io_asset->get_subtype( )
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
    result = io_asset->get_url( ).
  ENDMETHOD.


  METHOD constructor.
    go_html_viewer = NEW cl_gui_html_viewer( parent                   = cl_gui_container=>screen0
                                             query_table_disabled     = abap_true ).
    register_events( io_handler ).
  ENDMETHOD.


  METHOD register_events.
    DATA t_event TYPE cntl_simple_events.
    t_event = VALUE #( ( appl_event = abap_true
                         eventid    = go_html_viewer->m_id_sapevent ) ).
    go_html_viewer->set_registered_events( t_event ).
    SET HANDLER io_handler->on_sapevent FOR go_html_viewer.
  ENDMETHOD.


  METHOD render.
    SKIP. " Creates the screen0 container
    add_asset( NEW zcl_timem_asset_factory( )->create_instance(
      i_asset_type = zif_timem_consts=>asset_type-css
      is_parts     = is_parts ) ).

    DATA(url) = add_asset( NEW zcl_timem_asset_factory( )->create_instance(
      i_asset_type = zif_timem_consts=>asset_type-html
      is_parts     = is_parts ) ).

    go_html_viewer->show_url(
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
        text   = i_input
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
        buffer        = i_xstr
      IMPORTING
        output_length = e_size
      TABLES
        binary_tab    = et_bintab.
  ENDMETHOD.
ENDCLASS.
