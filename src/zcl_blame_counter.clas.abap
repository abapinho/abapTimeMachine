"! Counter to keep track of a percentage value. Taking a number of parts, it is
"! then incremented and it returns the calculated percentage corresponding to
"! the number of increments that already happened. It is used to help display
"! the interactive load progress.
CLASS zcl_blame_counter DEFINITION
  PUBLIC
  CREATE PUBLIC
  FINAL.

  PUBLIC SECTION.
    "! So that external instances can keep track of the percentage of the object
    "! which is already loaded.
    EVENTS percentage_changed
      EXPORTING
        VALUE(percentage) TYPE i
        VALUE(text) TYPE string.

    "! Initialize with total parts
    "! @parameter i_total_parts | Total number of parts
    METHODS initialize
      IMPORTING
        i_total_parts TYPE i.

    "! Increment counter and raises event with the new calculated percentage of
    "! increments that were done against the total of parts (which represents 100%).
    METHODS next
      IMPORTING
                i_text TYPE string
      RAISING   zcx_blame.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA g_cursor TYPE p .
    DATA g_total_parts TYPE p.
ENDCLASS.



CLASS ZCL_BLAME_COUNTER IMPLEMENTATION.


  METHOD initialize.
    g_total_parts = i_total_parts.
  ENDMETHOD.


  METHOD next.
    IF g_total_parts IS INITIAL.
      RAISE EXCEPTION TYPE zcx_blame.
    ENDIF.

    DATA percentage TYPE i.
    g_cursor = g_cursor + 1.
    percentage = 100 * g_cursor / g_total_parts.
    RAISE EVENT percentage_changed EXPORTING percentage = percentage text = i_text.
  ENDMETHOD.
ENDCLASS.
