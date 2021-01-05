"! Representation of a function module object. It will be able to create and
"! return a list of all the parts the function module is made of.
CLASS zcl_blame_object_func DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_blame_object .

    "! Constructor for the function module object.
    "! @parameter i_name | Function module name
    METHODS constructor
      IMPORTING
        !i_name TYPE rs38l_fnam .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_name TYPE rs38l_fnam .
ENDCLASS.



CLASS zcl_blame_object_func IMPLEMENTATION.


  METHOD constructor.
    g_name = i_name.
  ENDMETHOD.


  METHOD zif_blame_object~check_exists.
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = me->g_name
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    r_result = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_blame_object~get_name.
    r_name = g_name.
  ENDMETHOD.


  METHOD zif_blame_object~get_part_list.
    io_counter->initialize( 1 ).
    rt_part = VALUE #( ( NEW #( i_name      = CONV #( me->g_name )
                                i_vrsd_name = CONV #( me->g_name )
                                i_vrsd_type = 'FUNC' ) ) ).
    io_counter->next( CONV #( me->g_name ) ).
  ENDMETHOD.
ENDCLASS.
