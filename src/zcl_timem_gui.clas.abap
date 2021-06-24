CLASS zcl_timem_gui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_parts TYPE REF TO zcl_timem_parts .

    METHODS display
      RAISING
        zcx_timem .

  PROTECTED SECTION.
private section.

  data GO_PARTS type ref to ZCL_TIMEM_PARTS .
  data GO_VIEWER type ref to ZCL_TIMEM_GUI_VIEWER .
  data GO_HANDLER type ref to ZCL_TIMEM_GUI_HANDLER .

  methods HIGHLIGHT_SOURCE
    changing
      !PARTS type ZTIMEM_PARTS .

  methods DEDUPLICATE_HEADER_FIELDS
    changing
      !PARTS type ZTIMEM_PARTS .
ENDCLASS.



CLASS ZCL_TIMEM_GUI IMPLEMENTATION.


  METHOD constructor.
    me->go_parts = io_parts.
    me->go_handler = NEW #( me ).
    me->go_viewer = NEW #( go_handler ).
  ENDMETHOD.


  METHOD deduplicate_header_fields.
    DATA previous TYPE ztimem_line.

    LOOP AT parts-t_part REFERENCE INTO DATA(part).
      LOOP AT part->t_line REFERENCE INTO DATA(line).
        IF line->line_num <> 1 AND line->version_number = previous-version_number.
          CLEAR line->author.
          CLEAR line->author_name.
          CLEAR line->version_number.
          CLEAR line->request.
          CLEAR line->task.
          CLEAR line->date.
          CLEAR line->time.
        ELSE.
          previous = line->*.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD display.
    data(parts) = me->go_parts->get_data( ).
    highlight_source( CHANGING parts = parts ).
    deduplicate_header_fields( changing parts = parts ).
    go_viewer->render( parts ).
  ENDMETHOD.


  method HIGHLIGHT_SOURCE.
    DATA(highlighter) = NEW zcl_timem_syntax_abap( ).

    LOOP AT parts-t_part REFERENCE INTO DATA(part).
      LOOP AT part->t_line REFERENCE INTO DATA(line).
        line->source = highlighter->process_line(  CONV #( line->source ) ).
      ENDLOOP.
    ENDLOOP.
  endmethod.
ENDCLASS.
