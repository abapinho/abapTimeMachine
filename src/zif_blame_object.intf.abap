INTERFACE zif_blame_object
  PUBLIC .

  "! Returns a table of references to part instances
  METHODS get_part_list
    RETURNING
      VALUE(rt_part) TYPE zblame_part_ref_t
    RAISING
      zcx_blame .

  "! Returns the object name
  METHODS get_name
    RETURNING
      VALUE(r_name) TYPE string .

  "! Checks if the object exists in the system
  METHODS check_exists
    RETURNING
      VALUE(r_result) TYPE boolean .
ENDINTERFACE.
