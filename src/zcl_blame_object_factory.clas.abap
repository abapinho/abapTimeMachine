"! Factory class for the object family:
"! - Class
"! - Function group
"! - Function module
"! - Program
CLASS zcl_blame_object_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_object_type,
        program        TYPE zblame_object_type VALUE 'PROG',
        class          TYPE zblame_object_type VALUE 'CLAS',
        function_group TYPE zblame_object_type VALUE 'FUGR',
        function       TYPE zblame_object_type VALUE 'FUNC',
      END OF gc_object_type.

    "! Creates and returns an instance to the requested object
    "! @parameter i_object_type | Object type
    "! @parameter i_object_name | Object name
    METHODS get_instance
      IMPORTING
                i_object_type    TYPE zblame_object_type
                i_object_name    TYPE sobj_name
      RETURNING VALUE(ro_object) TYPE REF TO zif_blame_object
      RAISING   zcx_blame.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BLAME_OBJECT_FACTORY IMPLEMENTATION.


  METHOD get_instance.
    ro_object = SWITCH #( i_object_type
                        WHEN gc_object_type-program THEN NEW zcl_blame_object_prog( i_object_name )
                        WHEN gc_object_type-class THEN NEW zcl_blame_object_clas( CONV #( i_object_name ) )
                        WHEN gc_object_type-function_group THEN NEW zcl_blame_object_fugr( CONV #( i_object_name ) )
                        WHEN gc_object_type-function THEN NEW zcl_blame_object_func( CONV #( i_object_name ) ) ).
    IF ro_object IS NOT BOUND OR NOT ro_object->check_exists( ).
      RAISE EXCEPTION TYPE zcx_blame. " TODO
    ENDIF.
  ENDMETHOD.
ENDCLASS.
