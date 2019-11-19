CLASS zcl_blame_run DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS go
      IMPORTING
        !i_object_type TYPE zblame_object_type
        !i_object_name TYPE versobjnam
        !i_output_type type zcl_blame_gui_factory=>ty_output_type.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_blame_run IMPLEMENTATION.
  METHOD go.
    DATA(t_blame) = NEW zcl_blame_part( i_object_type = 'REPS'
                                        i_object_name = i_object_name )->compute_blame( ).

    data(o_out) = NEW zcl_blame_gui_factory( )->get_instance( i_output_type ).

    o_out->render( t_blame ).
  ENDMETHOD.
ENDCLASS.
