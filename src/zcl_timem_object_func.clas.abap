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
protected section.
private section.

  data G_NAME type RS38L_FNAM .
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
    result = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_timem_object~get_name.
    result = g_name.
  ENDMETHOD.


  METHOD zif_timem_object~get_part_list.
    result = VALUE #( (
      name        = CONV #( me->g_name )
      object_name = CONV #( me->g_name )
      type        = 'FUNC' ) ).
  ENDMETHOD.
ENDCLASS.
