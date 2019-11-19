CLASS zcl_blame_gui_salv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_blame_gui_factory.

  PUBLIC SECTION.
    INTERFACES zif_blame_renderable.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_blame_gui_salv IMPLEMENTATION.


  METHOD zif_blame_renderable~render.
    DATA: o_salv TYPE REF TO cl_salv_table,
          o_exp  TYPE REF TO cx_salv_msg.

    DATA(t_blame) = it_blame.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = o_salv
                                CHANGING  t_table      = t_blame ).
        o_salv->get_functions( )->set_default( ).
        o_salv->get_columns( )->set_optimize( ).
        o_salv->display( ).
      CATCH cx_salv_msg INTO o_exp.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
