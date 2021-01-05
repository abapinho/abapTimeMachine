"! Counter to keep track of a percentage value. Taking a number of parts, it is
"! then incremented and it returns the calculated percentage corresponding to
"! the number of increments that already happened. It is used to help display
"! the interactive load progress.
CLASS zcl_blame_counter DEFINITION
  PUBLIC
  CREATE PUBLIC
  FINAL.

  PUBLIC SECTION.
    "! @parameter i_total_parts | Total number of parts
    METHODS constructor
      IMPORTING
        !i_total_parts TYPE i .

    "! Increment counter and return the new calculated percentage of increments
    "! that were done against the total of parts (which represents 100%)
    METHODS next
      RETURNING VALUE(r_percentage) TYPE i.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA g_cursor TYPE p .
    DATA g_total_parts TYPE p.
ENDCLASS.



CLASS ZCL_BLAME_COUNTER IMPLEMENTATION.


  METHOD constructor.
    g_total_parts = i_total_parts.
  ENDMETHOD.


  METHOD next.
    g_cursor = g_cursor + 1.
    r_percentage = 100 * g_cursor / g_total_parts.
  ENDMETHOD.
ENDCLASS.
