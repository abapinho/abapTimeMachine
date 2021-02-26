"! Factory class for the object family:
"! - Class
"! - Function group
"! - Function module
"! - Program
class ZCL_TIMEM_OBJECT_FACTORY definition
  public
  final
  create public .

public section.

  constants:
    BEGIN OF gc_object_type,
        program        TYPE ztimem_object_type VALUE 'PROG',
        class          TYPE ztimem_object_type VALUE 'CLAS',
        function_group TYPE ztimem_object_type VALUE 'FUGR',
        function       TYPE ztimem_object_type VALUE 'FUNC',
      END OF gc_object_type .

    "! Creates and returns an instance to the requested object
    "! @parameter i_object_type | Object type
    "! @parameter i_object_name | Object name
  methods GET_INSTANCE
    importing
      !I_OBJECT_TYPE type ZTIMEM_OBJECT_TYPE
      !I_OBJECT_NAME type SOBJ_NAME
    returning
      value(RO_OBJECT) type ref to ZIF_TIMEM_OBJECT
    raising
      ZCX_TIMEM .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_FACTORY IMPLEMENTATION.


  METHOD get_instance.
    ro_object = SWITCH #( i_object_type
                        WHEN gc_object_type-program THEN NEW zcl_timem_object_prog( i_object_name )
                        WHEN gc_object_type-class THEN NEW zcl_timem_object_clas( CONV #( i_object_name ) )
                        WHEN gc_object_type-function_group THEN NEW zcl_timem_object_fugr( CONV #( i_object_name ) )
                        WHEN gc_object_type-function THEN NEW zcl_timem_object_func( CONV #( i_object_name ) ) ).
    IF ro_object IS NOT BOUND OR NOT ro_object->check_exists( ).
      RAISE EXCEPTION TYPE zcx_timem
        EXPORTING
          textid = zcx_timem=>object_not_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
