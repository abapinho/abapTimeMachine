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
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_name TYPE sobj_name .
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_PROG IMPLEMENTATION.


  METHOD constructor.
    g_name = i_name.
  ENDMETHOD.


  METHOD zif_timem_object~check_exists.
    SELECT SINGLE name INTO @data(name)
      FROM trdir
      WHERE name   = @g_name.
    r_result = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_timem_object~get_name.
    r_name = g_name.
  ENDMETHOD.


  METHOD zif_timem_object~get_part_list.
    rt_part = VALUE #( ( NEW #( i_name      = CONV #( me->g_name )
                                 i_vrsd_name = CONV #( me->g_name )
                                 i_vrsd_type = 'REPS' ) ) ).
  ENDMETHOD.
ENDCLASS.
