CLASS zcl_blame_counter DEFINITION
  PUBLIC
  CREATE PUBLIC
  FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !i_total_parts TYPE i .

    METHODS next
      RETURNING VALUE(r_percentage) TYPE i.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA g_cursor TYPE p .
    DATA g_total_parts TYPE p.
ENDCLASS.



CLASS zcl_blame_counter IMPLEMENTATION.


  METHOD constructor.
    g_total_parts = i_total_parts.
  ENDMETHOD.


  METHOD next.
    g_cursor = g_cursor + 1.
    r_percentage = 100 * g_cursor / g_total_parts.
  ENDMETHOD.
ENDCLASS.
