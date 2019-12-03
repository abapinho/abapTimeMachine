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


  METHOD zif_blame_object~get_part_list.
    rt_part = VALUE #( ( NEW #( i_name      = CONV #( me->g_name )
                                i_vrsd_name = CONV #( me->g_name )
                                i_vrsd_type = 'REPS' ) ) ).
  ENDMETHOD.


  METHOD zif_blame_object~get_name.
    r_name = g_name.
  ENDMETHOD.


  METHOD zif_blame_object~check_exists.
    SELECT SINGLE name INTO @data(name)
      FROM trdir
      WHERE name   = @g_name.
    IF sy-subrc = 0.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
