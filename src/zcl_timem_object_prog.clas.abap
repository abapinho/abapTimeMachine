"! Representation of a program/include object. This object consists of a single part which
"! this class  will be able to create and return.
class ZCL_TIMEM_OBJECT_PROG definition
  public
  final
  create public .

public section.

  interfaces ZIF_TIMEM_OBJECT .

    "! Constructor for the program/include object.
    "! @parameter i_name | Program/include name
  methods CONSTRUCTOR
    importing
      !I_NAME type SOBJ_NAME .
protected section.
private section.

  data G_NAME type SOBJ_NAME .
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_PROG IMPLEMENTATION.


  METHOD constructor.
    g_name = i_name.
  ENDMETHOD.


  METHOD zif_timem_object~check_exists.
    SELECT SINGLE name INTO g_name
      FROM trdir
      WHERE name   = g_name.
    result = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_timem_object~get_name.
    result = g_name.
  ENDMETHOD.


  METHOD zif_timem_object~get_part_list.
    result = VALUE #( (
      name        = CONV #( me->g_name )
      object_name = CONV #( me->g_name )
      type        = 'REPS' ) ).
  ENDMETHOD.
ENDCLASS.
