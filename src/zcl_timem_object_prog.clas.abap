"! Representation of a program/include object. This object consists of a single part which
"! this class  will be able to create and return.
CLASS zcl_timem_object_prog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_timem_object .

    "! Constructor for the program/include object.
    "! @parameter i_name | Program/include name
    METHODS constructor
      IMPORTING
        !name TYPE sobj_name .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA name TYPE sobj_name .
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_PROG IMPLEMENTATION.


  METHOD constructor.
    me->name = name.
  ENDMETHOD.


  METHOD zif_timem_object~check_exists.
    SELECT SINGLE name INTO name
      FROM trdir
      WHERE name   = name.
    result = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_timem_object~get_name.
    result = name.
  ENDMETHOD.


  METHOD zif_timem_object~get_part_list.
    result = VALUE #( (
      name        = CONV #( name )
      object_name = CONV #( name )
      type        = 'REPS' ) ).
  ENDMETHOD.
ENDCLASS.
