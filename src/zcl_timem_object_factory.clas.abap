"! Factory class for the object family:
"! - Class
"! - Function group
"! - Function module
"! - Program
CLASS zcl_timem_object_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF gc_object_type,
        program               TYPE ztimem_object_type VALUE 'PROG',
        program_includes TYPE ztimem_object_type VALUE 'PRGI',
        class                 TYPE ztimem_object_type VALUE 'CLAS',
        function_group        TYPE ztimem_object_type VALUE 'FUGR',
        function              TYPE ztimem_object_type VALUE 'FUNC',
      END OF gc_object_type .

    "! Creates and returns an instance to the requested object
    "! @parameter i_object_type | Object type
    "! @parameter i_object_name | Object name
    METHODS get_instance
      IMPORTING
        !object_type TYPE ztimem_object_type
        !object_name TYPE sobj_name
      RETURNING
        VALUE(result)  TYPE REF TO zif_timem_object
      RAISING
        zcx_timem .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_FACTORY IMPLEMENTATION.


  METHOD get_instance.
    result = SWITCH #(
      object_type
      WHEN gc_object_type-program THEN NEW zcl_timem_object_prog( object_name )
      WHEN gc_object_type-program_includes THEN NEW zcl_timem_object_prog_includes( object_name )
      WHEN gc_object_type-class THEN NEW zcl_timem_object_clas( CONV #( object_name ) )
      WHEN gc_object_type-function_group THEN NEW zcl_timem_object_fugr( CONV #( object_name ) )
      WHEN gc_object_type-function THEN NEW zcl_timem_object_func( CONV #( object_name ) ) ).
    IF result IS NOT BOUND OR NOT result->check_exists( ).
      RAISE EXCEPTION TYPE zcx_timem
        EXPORTING
          textid = zcx_timem=>object_not_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
