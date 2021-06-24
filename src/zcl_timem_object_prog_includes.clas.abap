"! Representation of a program/include object. This object consists of a single part which
"! this class  will be able to create and return.
CLASS zcl_timem_object_prog_includes DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_timem_object .

    "! Constructor for the program/include object.
    "! @parameter i_name | Program/include name
    METHODS constructor
      IMPORTING
        !i_name TYPE sobj_name .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_t_rpy_repo TYPE STANDARD TABLE OF rpy_repo WITH KEY inclname.

    DATA g_name TYPE sobj_name .

    METHODS get_includes
      RETURNING VALUE(result) TYPE ty_t_rpy_repo.
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_PROG_INCLUDES IMPLEMENTATION.


  METHOD constructor.
    g_name = i_name.
  ENDMETHOD.


  METHOD get_includes.
    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = me->g_name
        only_texts       = abap_true
      TABLES
        include_tab      = result
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD zif_timem_object~check_exists.
    SELECT SINGLE name INTO @DATA(name)
      FROM trdir
      WHERE name   = @g_name.
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

    DATA(includes) = get_includes( ).

    result = VALUE #(
      BASE result
      FOR include IN includes
      ( name = include-title
        object_name = include-inclname
        type = 'REPS' ) ).
  ENDMETHOD.
ENDCLASS.
