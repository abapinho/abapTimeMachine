CLASS zcl_blame_run DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS go
      IMPORTING
                !i_object_type TYPE zblame_object_type
                !i_object_name TYPE sobj_name
      RAISING   zcx_blame.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_blame_run IMPLEMENTATION.
  METHOD go.
    DATA(o_parts) = NEW zcl_blame_parts( i_object_type = i_object_type
                                          i_object_name = i_object_name ).

    NEW zcl_blame_output( )->render( o_parts->get_data( ) ).
  ENDMETHOD.
ENDCLASS.
