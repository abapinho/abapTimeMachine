CLASS zcl_blame_run DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS go
      IMPORTING
        !i_object_type TYPE zblame_object_type
        !i_object_name TYPE sobj_name.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BLAME_RUN IMPLEMENTATION.
  METHOD go.
    DATA(t_blame) = NEW zcl_blame_parts( i_object_type = i_object_type
                                        i_object_name = i_object_name )->compute_blame( ).
    NEW zcl_blame_output( )->render( t_blame ).
  ENDMETHOD.
ENDCLASS.
