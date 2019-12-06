CLASS zcl_blame_asset_html DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_blame_asset .

    METHODS constructor
      IMPORTING
        is_parts TYPE zblame_parts.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA gs_parts TYPE zblame_parts.

    METHODS pre_render_lines
      IMPORTING
                VALUE(it_blame) TYPE zblame_line_t
      RETURNING VALUE(rt_blame) TYPE zblame_line_t.
ENDCLASS.



CLASS zcl_blame_asset_html IMPLEMENTATION.


  METHOD constructor.
    gs_parts = is_parts.
    LOOP AT gs_parts-t_part REFERENCE INTO DATA(s_part).
      s_part->t_blame = pre_render_lines( s_part->t_blame ).
    ENDLOOP.
  ENDMETHOD.


  METHOD pre_render_lines.
    DATA(o_highlighter) = NEW zcl_blame_syntax_abap( ).
    DATA s_previous LIKE LINE OF it_blame.

    LOOP AT it_blame INTO DATA(s_blame).
      IF s_blame-version_number = s_previous-version_number.
        CLEAR s_blame-author.
        CLEAR s_blame-author_name.
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
    SOURCE parts = gs_parts
    RESULT XML r_content
    OPTIONS xml_header = 'NO'.
  ENDMETHOD.


  METHOD zif_blame_asset~get_subtype.
    r_subtype = 'html'.
  ENDMETHOD.


  METHOD zif_blame_asset~get_url.
    r_url = 'blame.html'.
  ENDMETHOD.
ENDCLASS.
