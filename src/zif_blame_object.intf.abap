INTERFACE zif_blame_object
  PUBLIC .

  TYPES: ty_t_part_ref TYPE STANDARD TABLE OF REF TO zcl_blame_part with key TABLE_LINE.

  "! Returns a list of references to part instances
  METHODS get_part_list
    RETURNING
      VALUE(rt_part) TYPE ty_t_part_ref
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
