"! Representation of a function module object. It will be able to create and
"! return a list of all the parts the function module is made of.
class ZCL_TIMEM_OBJECT_FUNC definition
  public
  final
  create public .

public section.

  interfaces ZIF_TIMEM_OBJECT .

    "! Constructor for the function module object.
    "! @parameter i_name | Function module name
  methods CONSTRUCTOR
    importing
      !I_NAME type RS38L_FNAM .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_name TYPE rs38l_fnam .
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_FUNC IMPLEMENTATION.


  METHOD constructor.
    g_name = i_name.
  ENDMETHOD.


  METHOD zif_timem_object~check_exists.
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = me->g_name
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    r_result = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_timem_object~get_name.
    r_name = g_name.
  ENDMETHOD.


  METHOD zif_timem_object~get_part_list.
    rt_part = VALUE #( ( NEW #( i_name      = CONV #( me->g_name )
                                i_vrsd_name = CONV #( me->g_name )
                                i_vrsd_type = 'FUNC' ) ) ).
  ENDMETHOD.
ENDCLASS.
