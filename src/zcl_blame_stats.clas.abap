CLASS zcl_blame_stats DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA stats TYPE zblame_stats READ-ONLY.

    METHODS constructor
      IMPORTING
        !it_blame TYPE zblame_line_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_comment_lines
      IMPORTING
                !it_blame      TYPE zblame_line_t
      RETURNING VALUE(r_count) TYPE i.

    METHODS get_empty_lines
      IMPORTING
                !it_blame      TYPE zblame_line_t
      RETURNING VALUE(r_count) TYPE i.

    METHODS get_date_oldest
      IMPORTING
                !it_blame     TYPE zblame_line_t
      RETURNING VALUE(r_date) TYPE datum.

    METHODS get_date_latest
      IMPORTING
                !it_blame     TYPE zblame_line_t
      RETURNING VALUE(r_date) TYPE datum.

    METHODS get_version_count
      IMPORTING
                !it_blame      TYPE zblame_line_t
      RETURNING VALUE(r_count) TYPE i.

ENDCLASS.



CLASS zcl_blame_stats IMPLEMENTATION.


  METHOD constructor.
    me->stats-total_lines = lines( it_blame ).
    me->stats-comment_lines = get_comment_lines( it_blame ).
    me->stats-empty_lines = get_empty_lines( it_blame ).
    me->stats-version_count = get_version_count( it_blame ).
    me->stats-date_oldest = get_date_oldest( it_blame ).
    me->stats-date_latest = get_date_latest( it_blame ).
  ENDMETHOD.


  METHOD get_comment_lines.
    DATA first_char TYPE char1.
    LOOP AT it_blame REFERENCE INTO DATA(os_blame).
      first_char = shift_left( os_blame->source ).
      IF first_char CO '*"'.
        r_count = r_count + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_empty_lines.
    DATA(t_blame) = it_blame.
    DELETE t_blame WHERE source IS NOT INITIAL.
    r_count = lines( t_blame ).
  ENDMETHOD.


  METHOD get_date_latest.
    r_date = '00000000'.
    LOOP AT it_blame REFERENCE INTO DATA(os_blame).
      IF os_blame->date > r_date.
        r_date = os_blame->date.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_date_oldest.
    r_date = '999999999'.
    LOOP AT it_blame REFERENCE INTO DATA(os_blame).
      IF os_blame->date < r_date.
        r_date = os_blame->date.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_version_count.
    r_count = REDUCE #( INIT count = 0
                        FOR GROUPS version_number OF s_blame IN it_blame
                        GROUP BY ( version = s_blame-version_number )
                        NEXT count = count + 1 ).
  ENDMETHOD.
ENDCLASS.
