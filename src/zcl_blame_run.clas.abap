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
    METHODS on_loading_version_source
          FOR EVENT loading_source OF zcl_blame_version
      IMPORTING
          !type
          !name
          !version_number.
ENDCLASS.



CLASS ZCL_BLAME_RUN IMPLEMENTATION.


  METHOD go.
    SET HANDLER me->on_loading_version_source FOR ALL INSTANCES.
    DATA(o_parts) = NEW zcl_blame_parts( i_object_type = i_object_type
                                         i_object_name = i_object_name ).
    NEW zcl_blame_gui( o_parts )->display( ).
  ENDMETHOD.


  METHOD on_loading_version_source.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text       = |Loading { type } { name } { version_number }|.
  ENDMETHOD.
ENDCLASS.
