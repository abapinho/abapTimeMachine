"! Main entry point. Instantiated by the program to run the whole show:
"! 1. Load all the information
"! 2. Display results in HTML page
CLASS zcl_blame_run DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! Execute everything
    METHODS go
      IMPORTING
                !i_object_type TYPE zblame_object_type
                !i_object_name TYPE sobj_name
      RAISING   zcx_blame.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS on_percentage_changed
          FOR EVENT percentage_changed OF zcl_blame_counter
      IMPORTING
          !percentage
          !text.
ENDCLASS.



CLASS zcl_blame_run IMPLEMENTATION.


  METHOD go.
    DATA(o_parts) = NEW zcl_blame_parts( i_object_type = i_object_type
                                         i_object_name = i_object_name ).
    DATA(o_counter) = NEW zcl_blame_counter( ).
    SET HANDLER me->on_percentage_changed FOR o_counter.
    o_parts->load( o_counter ).
    NEW zcl_blame_gui( o_parts )->display( ).
  ENDMETHOD.


  METHOD on_percentage_changed.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = percentage
        text       = |Loading { text }|.
  ENDMETHOD.
ENDCLASS.
