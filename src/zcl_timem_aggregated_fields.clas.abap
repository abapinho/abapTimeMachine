CLASS zcl_timem_aggregated_fields DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS build
      IMPORTING
        !lines         TYPE ztimem_line_t
        !custom1_title TYPE string
        !custom2_title TYPE string
        !custom3_title TYPE string
      RETURNING
        VALUE(result)  TYPE ztimem_aggregated_field_t .

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS build_aggregated_field
      IMPORTING
                lines         TYPE ztimem_line_t
                fieldname     TYPE name_feld
                title         TYPE string OPTIONAL
      RETURNING VALUE(result) TYPE ztimem_aggregated_field.
ENDCLASS.



CLASS ZCL_TIMEM_AGGREGATED_FIELDS IMPLEMENTATION.


  METHOD build.
    result = VALUE #(
      ( build_aggregated_field( fieldname = 'AUTHOR' lines = lines ) )
      ( build_aggregated_field( fieldname = 'REQUEST' lines = lines ) )
      ( build_aggregated_field( fieldname = 'CUSTOM1' lines = lines title = custom1_title ) )
      ( build_aggregated_field( fieldname = 'CUSTOM2' lines = lines title = custom2_title ) )
      ( build_aggregated_field( fieldname = 'CUSTOM3' lines = lines title = custom3_title ) ) ).
  ENDMETHOD.


  METHOD build_aggregated_field.
    result = VALUE #(
      fieldname = fieldname
      title = title
      lines =  NEW zcl_timem_aggregated_field( fieldname )->build( lines ) ).
  ENDMETHOD.
ENDCLASS.
