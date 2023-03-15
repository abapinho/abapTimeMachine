INTERFACE zif_timem_object
  PUBLIC .


  TYPES:
    ty_t_part_ref TYPE STANDARD TABLE OF REF TO zcl_timem_part WITH KEY table_line .

  "! Returns a list of references to part instances
  METHODS get_tadir_list
    RETURNING
      VALUE(result) TYPE ztimem_part_t
    RAISING
      zcx_timem .

  "! Returns the object name
  METHODS get_name
    RETURNING
      VALUE(result) TYPE string .

  "! Checks if the object exists in the system
  METHODS check_exists
    RETURNING
      VALUE(result) TYPE boolean .
ENDINTERFACE.
