CLASS zcl_blame_run DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS go
      IMPORTING
                !i_object_type TYPE zblame_object_type
                !i_object_name TYPE sobj_name
                !io_options    TYPE REF TO zcl_blame_options
      RAISING   zcx_blame.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS on_parts_percentage_complete
          FOR EVENT percentage_complete OF zcl_blame_parts
      IMPORTING
          !percentage
          !text.
ENDCLASS.



CLASS zcl_blame_run IMPLEMENTATION.
  METHOD go.
    DATA(o_parts) = NEW zcl_blame_parts( i_object_type = i_object_type
                                         i_object_name = i_object_name ).
    SET HANDLER me->on_parts_percentage_complete FOR o_parts.
    o_parts->load_parts( ).
    DATA(s_parts) = o_parts->get_data( io_options ).
    NEW zcl_blame_output( io_options->theme )->render( s_parts ).
  ENDMETHOD.


  METHOD on_parts_percentage_complete.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = percentage
        text       = |Loading { text }|.
  ENDMETHOD.
ENDCLASS.
