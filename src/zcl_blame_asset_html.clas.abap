CLASS zcl_blame_asset_html DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_blame_asset .

    METHODS constructor
      IMPORTING
        it_blame TYPE zblame_line_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA gt_blame TYPE zblame_line_t.

    METHODS pre_render_lines
      IMPORTING
                VALUE(it_blame) TYPE zblame_line_t
      RETURNING VALUE(rt_blame) TYPE zblame_line_t.
ENDCLASS.



CLASS ZCL_BLAME_ASSET_HTML IMPLEMENTATION.


  METHOD constructor.
    gt_blame = pre_render_lines( it_blame ).
  ENDMETHOD.


  METHOD pre_render_lines.
    data(o_highlighter) = NEW zcl_blame_syntax_abap( ).
    DATA s_previous LIKE LINE OF it_blame.

    LOOP AT it_blame INTO DATA(s_blame).
      IF s_blame-version_number = s_previous-version_number.
        CLEAR s_blame-author.
        CLEAR s_blame-version_number.
        CLEAR s_blame-request.
        CLEAR s_blame-date.
        CLEAR s_blame-time.
      ELSE.
        s_previous = s_blame.
      ENDIF.
      s_blame-source = o_highlighter->process_line(  CONV #( s_blame-source ) ).
      INSERT s_blame INTO TABLE rt_blame.
    ENDLOOP.
*     date_time = |{ is_current-date DATE = USER } { is_current-time TIME = USER }|.
*    ro_html->add( |<TD class="num">{ condense( val = is_current-line_num del = '0' ) }</TD>| ).
*    ro_html->add( |<TD class="code">{ o_highlighter->process_line( CONV #( is_current-source ) ) }</TD>| ).
*    ro_html->add( |<TD class="num">{ condense( val = version_number del = '0' ) }</TD>| ).
  ENDMETHOD.


  METHOD zif_blame_asset~get_content.
    CALL TRANSFORMATION zblame_html
    SOURCE lines = gt_blame
    RESULT XML r_content
    OPTIONS xml_header = 'NO' .
  ENDMETHOD.


  METHOD zif_blame_asset~get_subtype.
    r_subtype = 'html'.
  ENDMETHOD.


  METHOD zif_blame_asset~get_url.
    r_url = 'blame.html'.
  ENDMETHOD.
ENDCLASS.
