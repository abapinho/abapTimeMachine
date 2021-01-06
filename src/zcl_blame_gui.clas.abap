CLASS zcl_blame_gui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_options TYPE REF TO zcl_blame_options
        !io_parts   TYPE REF TO zcl_blame_parts .

    METHODS display
      RAISING
        zcx_blame .

    METHODS filter_parts
      IMPORTING
        io_parts_filter TYPE ref to zcl_blame_parts_filter.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA go_options TYPE REF TO zcl_blame_options.
    DATA go_parts TYPE REF TO zcl_blame_parts.
    DATA go_viewer TYPE REF TO zcl_blame_gui_viewer.
    DATA go_handler TYPE REF TO zcl_blame_gui_handler.
ENDCLASS.



CLASS ZCL_BLAME_GUI IMPLEMENTATION.


  METHOD constructor.
    me->go_options = io_options.
    me->go_parts = io_parts.
    me->go_handler = NEW #( me ).
    me->go_viewer = NEW #( io_options = io_options
                           io_handler = go_handler ).
  ENDMETHOD.


  METHOD display.
    go_viewer->render( me->go_parts ).
  ENDMETHOD.


  METHOD filter_parts.
    me->go_parts->filter( io_parts_filter ).
  ENDMETHOD.
ENDCLASS.
