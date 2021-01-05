"! Representation of a program/include object. This object consists of a single part which
"! this class  will be able to create and return.
CLASS zcl_blame_object_prog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_blame_object .

    "! Constructor for the program/include object.
    "! @parameter i_name | Program/include name
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
    io_counter->initialize( 1 ).
    rt_part = VALUE #( ( NEW #( i_name      = CONV #( me->g_name )
                                 i_vrsd_name = CONV #( me->g_name )
                                 i_vrsd_type = 'REPS' ) ) ).
    io_counter->next( CONV #( me->g_name ) ).
  ENDMETHOD.


  METHOD zif_blame_object~get_name.
    r_name = g_name.
  ENDMETHOD.


  METHOD zif_blame_object~check_exists.
    SELECT SINGLE name INTO @data(name)
      FROM trdir
      WHERE name   = @g_name.
    r_result = boolc( sy-subrc = 0 ).
  ENDMETHOD.
ENDCLASS.
