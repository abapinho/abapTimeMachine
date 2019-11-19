CLASS zcl_blame_gui_html DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_blame_gui_factory.

  PUBLIC SECTION.
    INTERFACES zif_blame_renderable.

    METHODS display
      IMPORTING
        !io_html_viewer TYPE REF TO cl_gui_html_viewer
        !i_url          TYPE w3url.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS html_full_page
      IMPORTING
        !it_blame      TYPE zblame_line_t
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_blame_html .

    METHODS html_head
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_blame_html .

    METHODS title
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_blame_html .

    METHODS footer
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_blame_html .

    METHODS render_content
      IMPORTING
        !it_blame      TYPE zblame_line_t
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_blame_html .

    METHODS render_content_header
      IMPORTING
        !it_blame      TYPE zblame_line_t
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_blame_html .

    METHODS render_content_line
      IMPORTING
                is_blame       TYPE zblame_line
      RETURNING VALUE(ro_html) TYPE REF TO zcl_blame_html.
ENDCLASS.



CLASS zcl_blame_gui_html IMPLEMENTATION.
  METHOD zif_blame_renderable~render.
    DATA(o_html) = html_full_page( it_blame ).
    DATA(html) = o_html->render( abap_true ).
    NEW zcl_blame_html_viewer( )->display( html ).
  ENDMETHOD.


  METHOD display.
    io_html_viewer->show_url( i_url ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD footer.
    ro_html = NEW #( ).
    ro_html->add( '<div id="footer">' ).                    "#EC NOTEXT
    ro_html->add( 'footer' ).                               "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT
  ENDMETHOD.


  METHOD html_full_page.
    ro_html = NEW #( ).
    ro_html->add( '<!DOCTYPE html>' ).                      "#EC NOTEXT
    ro_html->add( '<html>' ).                               "#EC NOTEXT
    ro_html->add( html_head( ) ).
    ro_html->add( '<body>' ).                               "#EC NOTEXT
    ro_html->add( title( ) ).
    ro_html->add( render_content( it_blame ) ).
    ro_html->add( footer( ) ).
    ro_html->add( '</body>' ).                              "#EC NOTEXT
    ro_html->add( '</html>' ).                              "#EC NOTEXT
  ENDMETHOD.


  METHOD html_head.
    ro_html = NEW #( ).
    ro_html->add( '<head>' ).                               "#EC NOTEXT
    ro_html->add( '<meta http-equiv="content-type" content="text/html; charset=utf-8">' ). "#EC NOTEXT
    ro_html->add( '<meta http-equiv="X-UA-Compatible" content="IE=11,10,9,8" />' ). "#EC NOTEXT
    ro_html->add( '<title>AbapBlame</title>' ).             "#EC NOTEXT
    "ro_html->add( '<link rel="stylesheet" type="text/css" href="css/common.css">' ).
    "ro_html->add( '<link rel="stylesheet" type="text/css" href="css/ag-icons.css">' ).
    "ro_html->add( '<link rel="stylesheet" type="text/css" href="css/theme-default.css">' ). " Theme basis
    "ro_html->add( '<script type="text/javascript" src="js/common.js"></script>' ). "#EC NOTEXT
    "ro_html->add( '<style>.icon { font-size: 200% }</style>' ).
    ro_html->add( '</head>' ).                              "#EC NOTEXT
  ENDMETHOD.


  METHOD render_content.
    ro_html = NEW #( ).
    ro_html->add( '<TABLE>' ).
    ro_html->add( render_content_header( it_blame ) ).
    LOOP AT it_blame INTO DATA(s_blame).
      ro_html->add( render_content_line( s_blame ) ).
    ENDLOOP.
    ro_html->add( '</TABLE>' ).
  ENDMETHOD.


  METHOD title.
    ro_html = NEW #( ).
    ro_html->add( '<div id="header">' ).                    "#EC NOTEXT
    ro_html->add( '<table class="w100"><tr>' ).             "#EC NOTEXT
    ro_html->add( |<td><span class="page_title">ZBLAME </span></td>| ). "#EC NOTEXT
    ro_html->add( '</tr></table>' ).                        "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT
  ENDMETHOD.


  METHOD render_content_header.
    ro_html = NEW #( ).
    ro_html->add( '<TR>' ).
    ro_html->add( |<TD>Version</TD>| ).
    ro_html->add( |<TD>Author</TD>| ).
    ro_html->add( |<TD>Source</TD>| ).
    ro_html->add( |<TD>Date</TD>| ).
    ro_html->add( |<TD>Request</TD>| ).
    ro_html->add( '</TR>' ).
  ENDMETHOD.


  METHOD render_content_line.
    ro_html = NEW #( ).
    ro_html->add( '<TR>' ).
    ro_html->add( |<TD>{ is_blame-version_number }</TD>| ).
    ro_html->add( |<TD>{ is_blame-author }</TD>| ).
    ro_html->add( |<TD>{ is_blame-source }</TD>| ).
    ro_html->add( |<TD>{ is_blame-date DATE = USER } { is_blame-time TIME = USER }</TD>| ).
    ro_html->add( |<TD>{ is_blame-request }</TD>| ).
    ro_html->add( '</TR>' ).
  ENDMETHOD.
ENDCLASS.
