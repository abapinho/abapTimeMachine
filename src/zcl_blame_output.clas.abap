class ZCL_BLAME_OUTPUT definition
  public
  final
  create public .

public section.

  constants:
    BEGIN OF c_theme,
        light TYPE zblame_theme VALUE 'LIGHT',
        dark  TYPE zblame_theme VALUE 'DARK',
      END OF c_theme .
  data MT_DATA type LVC_T_MIME .

  methods CONSTRUCTOR
    importing
      !I_THEME type ZBLAME_THEME .
  methods RENDER
    importing
      !IS_PARTS type ZBLAME_PARTS .
  PROTECTED SECTION.
  PRIVATE SECTION.
    data g_theme type zblame_theme.
    DATA go_html_viewer TYPE REF TO cl_gui_html_viewer .

    METHODS add_asset
      IMPORTING
        !io_asset    TYPE REF TO zif_blame_asset
      RETURNING
        VALUE(r_url) TYPE w3url .

    METHODS add_main_css .

    METHODS add_html
      IMPORTING
        !is_parts    TYPE zblame_parts
      RETURNING
        VALUE(r_url) TYPE w3url .

    METHODS string_2_xstring
      IMPORTING
        !i_input        TYPE string
      RETURNING
        VALUE(r_output) TYPE xstring .

    METHODS on_html_events
          FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING
          !action
          !getdata .

    METHODS register_events .

    CLASS-METHODS xstring_2_bintab
      IMPORTING
        !i_xstr    TYPE xstring
      EXPORTING
        !e_size    TYPE i
        !et_bintab TYPE lvc_t_mime .
ENDCLASS.



CLASS ZCL_BLAME_OUTPUT IMPLEMENTATION.


  METHOD add_asset.
    DATA(content) = io_asset->get_content( ).
    DATA(xstr) = string_2_xstring( content ).

    xstring_2_bintab(
      EXPORTING
        i_xstr    = xstr
      IMPORTING
        e_size    = DATA(size)
        et_bintab = mt_data ). "DATA(t_bintab) ).

    go_html_viewer->load_data(
      EXPORTING
        url                    =  io_asset->get_url( )
        type                   = 'text'
        subtype                = io_asset->get_subtype( )
      CHANGING
        data_table             = mt_data
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
    r_url = io_asset->get_url( ).
  ENDMETHOD.


  METHOD add_html.
    r_url = add_asset( NEW zcl_blame_asset_html( is_parts ) ).
  ENDMETHOD.


  METHOD add_main_css.
    add_asset( NEW zcl_blame_asset_css( g_theme ) ).
  ENDMETHOD.


  METHOD constructor.
    g_theme = i_theme.
    go_html_viewer = NEW cl_gui_html_viewer( parent = cl_gui_container=>screen0 ).
  ENDMETHOD.


  METHOD on_html_events.
    "BREAK-POINT.
*    write: / action, getdata.
    CASE action.
      WHEN 'request'.
        DATA: lv_trkorr    TYPE e070-trkorr.
        DATA: lv_highlight TYPE c VALUE 'X'.
        DATA: ls_popup     TYPE strhi_popup.
        DATA: lv_showonly  TYPE c . "VALUE 'X'.

        lv_trkorr = condense( getdata ).
        ls_popup = VALUE #(
          end_column = 255
          end_row    = 200
        ).

        CALL FUNCTION 'TR_PRESENT_REQUEST'
          EXPORTING
            iv_trkorr    = lv_trkorr
            iv_highlight = lv_highlight
            is_popup     = ls_popup
            iv_showonly  = lv_showonly.

      WHEN 'save'.
        "save( mt_data ).
      WHEN OTHERS.
    ENDCASE.


  ENDMETHOD.


  METHOD register_events.
    DATA t_event TYPE cntl_simple_events.
    t_event = VALUE #( ( appl_event = abap_true
                         eventid    = go_html_viewer->m_id_sapevent ) ).
    go_html_viewer->set_registered_events( t_event ).
    SET HANDLER me->on_html_events FOR go_html_viewer.
  ENDMETHOD.


  METHOD render.
    SKIP. " Creates the screen0 container
    add_main_css( ).
    DATA(url) = add_html( is_parts ).
    register_events( ).
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
        buffer = r_output
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
