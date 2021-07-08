CLASS zcl_timem_gui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !parts TYPE REF TO zcl_timem_parts .

    METHODS display
      RAISING
        zcx_timem .

  PROTECTED SECTION.
private section.

  data PARTS type ref to ZCL_TIMEM_PARTS .
  data VIEWER type ref to ZCL_TIMEM_GUI_VIEWER .
  data HANDLER type ref to ZCL_TIMEM_GUI_HANDLER .

  methods HIGHLIGHT_SOURCE
    changing
      !DATA type ZTIMEM_DATA .
  methods DEDUPLICATE_HEADER_FIELDS
    changing
      !DATA type ZTIMEM_DATA .
ENDCLASS.



CLASS ZCL_TIMEM_GUI IMPLEMENTATION.


  METHOD constructor.
    me->parts = parts.
    me->handler = NEW #( me ).
    me->viewer = NEW #( handler ).
  ENDMETHOD.


  METHOD deduplicate_header_fields.
    DATA previous TYPE ztimem_line.

    LOOP AT data-parts REFERENCE INTO DATA(part).
      LOOP AT part->lines REFERENCE INTO DATA(line).
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
    DATA(data) = me->parts->get_data( ).
    highlight_source( CHANGING data = data ).
    deduplicate_header_fields( CHANGING data = data ).
    viewer->render( data ).
  ENDMETHOD.


  METHOD highlight_source.
    DATA(highlighter) = NEW zcl_timem_syntax_abap( ).

    LOOP AT data-parts REFERENCE INTO DATA(part).
      LOOP AT part->lines REFERENCE INTO DATA(line).
        line->source = highlighter->process_line(  CONV #( line->source ) ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
