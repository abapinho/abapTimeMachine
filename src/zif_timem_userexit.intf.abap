INTERFACE zif_timem_userexit
  PUBLIC .


  METHODS before_rendering
    IMPORTING
      !options TYPE REF TO zcl_timem_options
    CHANGING
      !data    TYPE ztimem_data .

  METHODS modify_parts
    IMPORTING
      !options TYPE REF TO zcl_timem_options
    CHANGING
      !parts   TYPE ztimem_part_source_t.

  METHODS modify_summary
    IMPORTING
      options TYPE REF TO zcl_timem_options
    CHANGING
      summary TYPE ztimem_summary
    RAISING
      zcx_timem.

  METHODS on_sapevent
    IMPORTING
      !options TYPE REF TO zcl_timem_options
      !action  TYPE c
      !getdata TYPE c .

  METHODS modify_tadir_list
    IMPORTING
      !options   TYPE REF TO zcl_timem_options
    CHANGING
      !tadir_list TYPE ztimem_part_t
    RAISING
      zcx_timem.

  METHODS modify_asset_content
    IMPORTING
      !options TYPE REF TO zcl_timem_options
      !subtype TYPE c
    CHANGING
      !content TYPE string .
ENDINTERFACE.
