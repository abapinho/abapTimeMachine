CLASS zcl_blame_object_prog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_blame_object .

    METHODS constructor
      IMPORTING
        !i_name TYPE sobj_name .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_name TYPE sobj_name .
ENDCLASS.



CLASS zcl_blame_object_prog IMPLEMENTATION.
  METHOD constructor.
    g_name = i_name.
  ENDMETHOD.

  METHOD zif_blame_object~get_includes.
    rt_include = VALUE #( ( me->g_name ) ).
  ENDMETHOD.

  METHOD zif_blame_object~get_name.
    r_name = g_name.
  ENDMETHOD.
ENDCLASS.
