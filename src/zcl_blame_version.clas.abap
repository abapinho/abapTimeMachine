CLASS zcl_blame_version DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS c_latest_version TYPE versno VALUE 99999.

    DATA version_number TYPE versno READ-ONLY.
    DATA request TYPE verskorrno READ-ONLY.
    DATA author  TYPE versuser READ-ONLY.
    DATA author_name TYPE ad_namtext READ-ONLY.
    DATA date    TYPE versdate READ-ONLY.
    DATA time    TYPE verstime READ-ONLY.

    METHODS constructor
      IMPORTING
                !io_part          TYPE REF TO zcl_blame_part
                !i_version_number TYPE versno
      RAISING   zcx_blame.

    METHODS get_source_with_blame
      RETURNING VALUE(rt_blame) TYPE zblame_line_t
      RAISING   zcx_blame.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA go_part TYPE REF TO zcl_blame_part.
    DATA gt_source TYPE abaptxt255_tab.

    METHODS load_attributes
      RAISING zcx_blame.

    METHODS load_source
      RAISING zcx_blame.

    METHODS get_real_version
      RETURNING VALUE(r_version) TYPE versno.
ENDCLASS.



CLASS zcl_blame_version IMPLEMENTATION.
  METHOD constructor.
    me->go_part = io_part.
    me->version_number = i_version_number.
    load_source( ).
    load_attributes( ).
  ENDMETHOD.


  METHOD load_attributes.
    DATA(versno) = get_real_version( ).
    SELECT SINGLE korrnum author datum zeit name_textc INTO (me->request, me->author, me->date, me->time, me->author_name)
      FROM vrsd
      LEFT JOIN user_addr ON user_addr~bname = vrsd~author
      WHERE objtype = me->go_part->vrsd_type
        AND objname = me->go_part->vrsd_name
        AND versno  = versno.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_blame. " TODO
    ENDIF.
  ENDMETHOD.


  METHOD get_source_with_blame.
    DATA: s_source_with_blame LIKE LINE OF rt_blame.

    s_source_with_blame-version_number = me->version_number.
    s_source_with_blame-request = me->request.
    s_source_with_blame-author = me->author.
    s_source_with_blame-author_name = me->author_name.
    s_source_with_blame-date = me->date.
    s_source_with_blame-time = me->time.

    LOOP AT gt_source INTO DATA(source_int).
      s_source_with_blame-line_num = sy-tabix.
      s_source_with_blame-source = source_int.
      INSERT s_source_with_blame INTO TABLE rt_blame.
    ENDLOOP.
  ENDMETHOD.


  METHOD load_source.
    DATA t_trdir TYPE trdir_it.

    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        object_name = me->go_part->vrsd_name
        object_type = me->go_part->vrsd_type
        versno      = get_real_version( )
      TABLES
        repos_tab   = gt_source
        trdir_tab   = t_trdir
      EXCEPTIONS
        no_version  = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_blame. " TODO
    ENDIF.
  ENDMETHOD.


  METHOD get_real_version.
    r_version = COND #( WHEN me->version_number = c_latest_version THEN 0
                        ELSE me->version_number ).
  ENDMETHOD.
ENDCLASS.
