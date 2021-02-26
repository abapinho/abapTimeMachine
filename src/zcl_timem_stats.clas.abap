"! Takes a list of code with blame information and fills a structure
"! with several calculated statistic indicators:
"! - total lines of code
"! - number of empty lines
"! - number of comment lines
"! - number of versions
"! - date of oldest version
"! - date of newest version
CLASS zcl_timem_stats DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Statistics structure
    DATA stats TYPE ztimem_stats READ-ONLY .

    "! Constructor which receives a source code and immediately fills the stats
    "! structure attribute with all the calculated values.
    METHODS constructor
      IMPORTING
        !it_line TYPE ztimem_line_t .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_comment_lines
      IMPORTING
        !it_line       TYPE ztimem_line_t
      RETURNING
        VALUE(r_count) TYPE i .
    METHODS get_empty_lines
      IMPORTING
        !it_line       TYPE ztimem_line_t
      RETURNING
        VALUE(r_count) TYPE i .
    METHODS get_date_oldest
      IMPORTING
        !it_line      TYPE ztimem_line_t
      RETURNING
        VALUE(r_date) TYPE datum .
    METHODS get_date_latest
      IMPORTING
        !it_line      TYPE ztimem_line_t
      RETURNING
        VALUE(r_date) TYPE datum .
ENDCLASS.



CLASS ZCL_TIMEM_STATS IMPLEMENTATION.


  METHOD constructor.
    me->stats-total_lines = lines( it_line ).
    me->stats-comment_lines = get_comment_lines( it_line ).
    me->stats-empty_lines = get_empty_lines( it_line ).
    me->stats-date_oldest = get_date_oldest( it_line ).
    me->stats-date_latest = get_date_latest( it_line ).
  ENDMETHOD.


  METHOD get_comment_lines.
    DATA first_char TYPE char1.
    LOOP AT it_line REFERENCE INTO DATA(os_line).
      first_char = shift_left( os_line->source ).
      IF first_char CO '*"'.
        r_count = r_count + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_date_latest.
    r_date = '00000000'.
    LOOP AT it_line REFERENCE INTO DATA(os_line).
      IF os_line->date > r_date.
        r_date = os_line->date.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_date_oldest.
    r_date = '999999999'.
    LOOP AT it_line REFERENCE INTO DATA(os_line).
      IF os_line->date < r_date.
        r_date = os_line->date.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_empty_lines.
    DATA(t_line) = it_line.
    DELETE t_line WHERE source IS NOT INITIAL.
    r_count = lines( t_line ).
  ENDMETHOD.
ENDCLASS.
