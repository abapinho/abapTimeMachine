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
ENDCLASS.



CLASS zcl_blame_run IMPLEMENTATION.
  METHOD go.
    DATA(o_parts) = NEW zcl_blame_parts( i_object_type = i_object_type
                                          i_object_name = i_object_name ).
    DATA(s_parts) = o_parts->get_data( io_options ).
    NEW zcl_blame_output( )->render( s_parts ).
  ENDMETHOD.
ENDCLASS.
