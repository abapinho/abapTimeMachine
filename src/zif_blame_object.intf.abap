INTERFACE zif_blame_object
  PUBLIC .


  METHODS get_part_list
    RETURNING
      VALUE(rt_part) TYPE zblame_part_ref_t
    RAISING
      zcx_blame .

  METHODS get_name
    RETURNING
      VALUE(r_name) TYPE string .

  METHODS check_exists
    RETURNING VALUE(r_result) TYPE boolean.
ENDINTERFACE.
