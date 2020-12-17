INTERFACE zif_blame_object
  PUBLIC .


  EVENTS percentage_complete
    EXPORTING
      VALUE(percentage) TYPE i
      VALUE(text) TYPE string.

  METHODS get_part_list
    RETURNING
      VALUE(rt_part) TYPE zblame_part_ref_t
    RAISING
      zcx_blame .
  METHODS get_name
    RETURNING
      VALUE(r_name) TYPE string .
  METHODS check_exists
    RETURNING
      VALUE(r_result) TYPE boolean .
ENDINTERFACE.
