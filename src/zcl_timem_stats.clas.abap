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
        !lines TYPE ztimem_line_t .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_comment_lines
      IMPORTING
        !lines        TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE i .

    METHODS get_empty_lines
      IMPORTING
        !lines        TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE i .

    METHODS get_date_oldest
      IMPORTING
        !lines        TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE datum .

    METHODS get_date_latest
      IMPORTING
        !lines        TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE datum .
ENDCLASS.



CLASS ZCL_TIMEM_STATS IMPLEMENTATION.


  METHOD constructor.
    stats = VALUE #(
      total_lines = lines( lines )
      comment_lines = get_comment_lines( lines )
      empty_lines = get_empty_lines( lines )
      date_oldest = get_date_oldest( lines )
      date_latest = get_date_latest( lines ) ).
  ENDMETHOD.


  METHOD get_comment_lines.
    DATA first_char TYPE char1.
    LOOP AT lines REFERENCE INTO DATA(line).
      first_char = shift_left( line->source ).
      IF first_char CO '*"'.
        result = result + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_date_latest.
    result = '00000000'.
    LOOP AT lines REFERENCE INTO DATA(line).
      IF line->date > result.
        result = line->date.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_date_oldest.
    result = '999999999'.
    LOOP AT lines REFERENCE INTO DATA(line).
      IF line->date < result.
        result = line->date.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_empty_lines.
    DATA(lines_aux) = lines.
    DELETE lines_aux WHERE source IS NOT INITIAL.
    result = lines( lines_aux ).
  ENDMETHOD.
ENDCLASS.
