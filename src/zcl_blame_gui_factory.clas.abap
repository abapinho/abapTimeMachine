CLASS zcl_blame_gui_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES ty_output_type TYPE char10.

    CONSTANTS:
      BEGIN OF gc_output_type,
        salv TYPE ty_output_type VALUE 'SALV',
        html TYPE ty_output_type VALUE 'HTML',
      END OF gc_output_type.

    METHODS get_instance
      IMPORTING
                i_output_type        TYPE ty_output_type
      RETURNING VALUE(ro_renderable) TYPE REF TO zif_blame_renderable.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_blame_gui_factory IMPLEMENTATION.
  METHOD get_instance.
    ro_renderable = SWITCH #( i_output_type
                              WHEN gc_output_type-html THEN NEW zcl_blame_gui_html( )
                              WHEN gc_output_type-salv THEN NEW zcl_blame_gui_salv( ) ).
  ENDMETHOD.
ENDCLASS.
