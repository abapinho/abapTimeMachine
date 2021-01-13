CLASS zcl_blame_gui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_parts   TYPE REF TO zcl_blame_parts .

    METHODS display
      RAISING
        zcx_blame .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA go_parts TYPE REF TO zcl_blame_parts.
    DATA go_viewer TYPE REF TO zcl_blame_gui_viewer.
    DATA go_handler TYPE REF TO zcl_blame_gui_handler.
ENDCLASS.



CLASS ZCL_BLAME_GUI IMPLEMENTATION.


  METHOD constructor.
    me->go_parts = io_parts.
    me->go_handler = NEW #( me ).
    me->go_viewer = NEW #( go_handler ).
  ENDMETHOD.


  METHOD display.
    go_viewer->render( me->go_parts ).
  ENDMETHOD.
ENDCLASS.
