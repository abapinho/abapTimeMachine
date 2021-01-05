INTERFACE zif_blame_object
  PUBLIC .

  "! So that external instances can keep track of the percentage of the object
  "! which is already loaded
  EVENTS percentage_complete
    EXPORTING
      VALUE(percentage) TYPE i
      VALUE(text) TYPE string.

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
