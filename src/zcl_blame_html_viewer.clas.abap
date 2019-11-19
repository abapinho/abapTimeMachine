CLASS zcl_blame_html_viewer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS display
      IMPORTING
        !i_html TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS cache_html
      IMPORTING
                !io_html_viewer TYPE REF TO cl_gui_html_viewer
                !i_html         TYPE string
      RETURNING VALUE(r_url)    TYPE w3url.

    METHODS string_2_xstring
      IMPORTING
                !i_input        TYPE string
      RETURNING VALUE(r_output) TYPE xstring.

    CLASS-METHODS xstring_2_bintab
      IMPORTING
        i_xstr    TYPE xstring
      EXPORTING
        e_size    TYPE i
        et_bintab TYPE lvc_t_mime.

ENDCLASS.



CLASS zcl_blame_html_viewer IMPLEMENTATION.
  METHOD display.
    SKIP. " Creates the screen0 container
    DATA(o_html_viewer) = NEW cl_gui_html_viewer( parent = cl_gui_container=>screen0 ).
    DATA(url) = cache_html( io_html_viewer = o_html_viewer
                            i_html         = i_html ).
    o_html_viewer->show_url(
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

  METHOD cache_html.
    DATA(xstr) = string_2_xstring( i_html ).

    xstring_2_bintab(
      EXPORTING
        i_xstr    = xstr
      IMPORTING
        e_size    = DATA(size)
        et_bintab = DATA(t_bintab) ).

    io_html_viewer->load_data(
          EXPORTING
            type         = 'text'
            subtype      = 'html'
            size         = size
          IMPORTING
            assigned_url = r_url
          CHANGING
            data_table   = t_bintab
          EXCEPTIONS
            OTHERS       = 1 ) ##NO_TEXT.
  ENDMETHOD.


  METHOD string_2_xstring.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = i_input
      IMPORTING
        buffer = r_output
      EXCEPTIONS
        OTHERS = 1.
    ASSERT sy-subrc = 0.
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
