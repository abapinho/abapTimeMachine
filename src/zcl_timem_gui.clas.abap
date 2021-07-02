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
  PRIVATE SECTION.

    DATA parts TYPE REF TO zcl_timem_parts .
    DATA viewer TYPE REF TO zcl_timem_gui_viewer .
    DATA handler TYPE REF TO zcl_timem_gui_handler .

    METHODS highlight_source
      CHANGING
        !parts TYPE ztimem_parts .

    METHODS deduplicate_header_fields
      CHANGING
        !parts TYPE ztimem_parts .
ENDCLASS.



CLASS ZCL_TIMEM_GUI IMPLEMENTATION.


  METHOD constructor.
    me->parts = parts.
    me->handler = NEW #( me ).
    me->viewer = NEW #( handler ).
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
    DATA(data) = me->parts->get_data( ).
    highlight_source( CHANGING parts = data ).
    deduplicate_header_fields( CHANGING parts = data ).
    viewer->render( data ).
  ENDMETHOD.


  METHOD highlight_source.
    DATA(highlighter) = NEW zcl_timem_syntax_abap( ).

    LOOP AT parts-t_part REFERENCE INTO DATA(part).
      LOOP AT part->t_line REFERENCE INTO DATA(line).
        line->source = highlighter->process_line(  CONV #( line->source ) ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
