INTERFACE zif_timem_userexit
  PUBLIC .


  METHODS before_rendering
    CHANGING
      !parts TYPE ztimem_parts .

  METHODS on_sapevent
    IMPORTING
      !action  TYPE c
      !getdata TYPE c .

  METHODS modify_part_list
    CHANGING
      !part_list TYPE ztimem_part_t .

  METHODS modify_asset_content
    IMPORTING
      subtype TYPE c
    CHANGING
      content TYPE string.
ENDINTERFACE.
