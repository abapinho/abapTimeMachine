CLASS zcl_blame_version DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA: version_number TYPE versno READ-ONLY.

    METHODS constructor
      IMPORTING
        !io_part          TYPE REF TO zcl_blame_part
        !i_version_number TYPE versno.

    METHODS get_source_with_blame
      RETURNING VALUE(rt_blame) TYPE zblame_line_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA go_part TYPE REF TO zcl_blame_part.

    METHODS get_attributes
      EXPORTING
        e_request TYPE verskorrno
        e_author  TYPE versuser
        e_date    TYPE versdate
        e_time    TYPE verstime.

    METHODS get_source_int
      RETURNING VALUE(rt_source) TYPE abaptxt255_tab.
ENDCLASS.



CLASS zcl_blame_version IMPLEMENTATION.


  METHOD constructor.
    me->go_part = io_part.
    me->version_number = i_version_number.
  ENDMETHOD.


  METHOD get_attributes.
    CLEAR: e_request, e_author, e_date, e_time.

    SELECT SINGLE korrnum author datum zeit INTO (e_request, e_author, e_date, e_time)
      FROM vrsd
      WHERE objtype = me->go_part->object_type
        AND objname = me->go_part->object_name
        AND versno  = me->version_number.
    IF sy-subrc <> 0.
      ASSERT 1 = 0. " TODO
    ENDIF.
  ENDMETHOD.


  METHOD get_source_int.
    DATA: t_trdir TYPE trdir_it.
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        object_name = me->go_part->object_name
        object_type = me->go_part->object_type
        versno      = me->version_number
      TABLES
        repos_tab   = rt_source
        trdir_tab   = t_trdir
      EXCEPTIONS
        no_version  = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      ASSERT 1 = 0. " TODO
    ENDIF.
  ENDMETHOD.


  METHOD get_source_with_blame.
    DATA: s_source LIKE LINE OF rt_blame.

    s_source-version_number = me->version_number.
    get_attributes( IMPORTING e_request = s_source-request
                              e_author  = s_source-author
                              e_date    = s_source-date
                              e_time    = s_source-time ).

    LOOP AT get_source_int( ) INTO DATA(source_int).
      s_source-source = source_int.
      INSERT s_source INTO TABLE rt_blame.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
