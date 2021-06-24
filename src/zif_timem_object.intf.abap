interface ZIF_TIMEM_OBJECT
  public .


  types:
    ty_t_part_ref TYPE STANDARD TABLE OF REF TO zcl_timem_part WITH KEY table_line .

  "! Returns a list of references to part instances
  methods GET_PART_LIST
    returning
      value(RESULT) type ZTIMEM_PART_T
    raising
      ZCX_TIMEM .
  "! Returns the object name
  methods GET_NAME
    returning
      value(RESULT) type STRING .
  "! Checks if the object exists in the system
  methods CHECK_EXISTS
    returning
      value(RESULT) type BOOLEAN .
endinterface.
