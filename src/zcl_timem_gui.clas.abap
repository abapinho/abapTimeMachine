CLASS zcl_timem_gui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !object_type TYPE ztimem_object_type
        !object_name TYPE sobj_name
      RAISING
        zcx_timem .

    METHODS display
      RAISING
        zcx_timem .

    METHODS revert
      IMPORTING
        ts TYPE timestamp
      RAISING
        zcx_timem.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA object_type TYPE ztimem_object_type.
    DATA object_name TYPE sobj_name.
    DATA parts TYPE REF TO zcl_timem_parts .
    DATA viewer TYPE REF TO zcl_timem_gui_viewer .
    DATA handler TYPE REF TO zcl_timem_gui_handler .

    METHODS highlight_source
      CHANGING
        !data TYPE ztimem_data.

    METHODS deduplicate_header_fields
      CHANGING
        !data TYPE ztimem_data .

    METHODS load_parts.
ENDCLASS.



CLASS ZCL_TIMEM_GUI IMPLEMENTATION.


  METHOD constructor.
    me->object_type = object_type.
    me->object_name = object_name.
    load_parts( ).
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
          CLEAR line->custom1.
          CLEAR line->custom2.
          CLEAR line->custom3.
        ELSE.
          previous = line->*.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD display.
    DATA(data) = parts->get_data( ).
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


  METHOD load_parts.
    me->parts = NEW zcl_timem_parts( object_type = object_type
                                     object_name = object_name ).
  ENDMETHOD.


  METHOD revert.
    parts->revert( ts ).
    load_parts( ).
  ENDMETHOD.
ENDCLASS.
