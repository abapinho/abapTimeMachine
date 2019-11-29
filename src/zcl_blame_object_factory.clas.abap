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
      END OF gc_object_type.

    METHODS get_instance
      IMPORTING
                i_object_type    TYPE zblame_object_type
                i_object_name    TYPE sobj_name
      RETURNING VALUE(ro_object) TYPE REF TO zif_blame_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_blame_object_factory IMPLEMENTATION.
  METHOD get_instance.
    ro_object = SWITCH #( i_object_type
                        WHEN gc_object_type-program THEN NEW zcl_blame_object_prog( i_object_name )
                        WHEN gc_object_type-class THEN NEW zcl_blame_object_clas( CONV #( i_object_name ) ) ).
  ENDMETHOD.
ENDCLASS.
