CLASS zcl_blame_version DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_version,
        latest_db TYPE versno VALUE 0,
        latest    TYPE versno VALUE 99998,
        active    TYPE versno VALUE 99998,
        modified  TYPE versno VALUE 99999,
      END OF c_version.

    DATA version_number TYPE versno READ-ONLY.
    DATA request TYPE verskorrno READ-ONLY.
    DATA task    TYPE verskorrno READ-ONLY.
    DATA author  TYPE versuser READ-ONLY.
    DATA author_name TYPE ad_namtext READ-ONLY.
    DATA date    TYPE versdate READ-ONLY.
    DATA time    TYPE verstime READ-ONLY.

    METHODS constructor
      IMPORTING
                !is_vrsd TYPE vrsd
      RAISING   zcx_blame.

    METHODS get_source_with_blame
      RETURNING VALUE(rt_blame) TYPE zblame_line_t
      RAISING   zcx_blame.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA s_vrsd TYPE vrsd.
    DATA gt_source TYPE abaptxt255_tab.

    METHODS load_attributes
      RAISING zcx_blame.

    METHODS load_source
      RAISING zcx_blame.

    METHODS get_real_version
      RETURNING VALUE(r_version) TYPE versno.

    METHODS load_latest_task.
ENDCLASS.



CLASS ZCL_BLAME_VERSION IMPLEMENTATION.


  METHOD constructor.
    me->s_vrsd = is_vrsd.
    load_attributes( ).
    load_latest_task( ).
    load_source( ).
  ENDMETHOD.


  METHOD get_real_version.
    " Technically the current version is 0 but in order to keep them properly sorted we're
    " setting it to magic number 99997 (because 'ACTIVE' is 99998 and 'MODIFIED' is 99999.
    " But when we're going to fetch it from the database we must use 0.
    r_version = COND #( WHEN me->version_number = c_version-latest THEN 0
                        ELSE me->version_number ).
  ENDMETHOD.


  METHOD get_source_with_blame.
    DATA: s_source_with_blame LIKE LINE OF rt_blame.

    s_source_with_blame-version_number = me->version_number.
    s_source_with_blame-request = me->request.
    s_source_with_blame-task = me->task.
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


  METHOD load_attributes.
    me->version_number = s_vrsd-versno.
    me->author = s_vrsd-author.
    me->date = s_vrsd-datum.
    me->time = s_vrsd-zeit.
    me->author_name = NEW zcl_blame_author( )->get_name( s_vrsd-author ).
    me->request = s_vrsd-korrnum.
  ENDMETHOD.


  METHOD load_latest_task.
    " Try to find the object in the request tasks because sometimes the request was created
    " by someone who was not the actual developer. The tasks better reflects the object's author.
    " If we find a task, we overwrite the author. We choose to pick the latest task.
    SELECT e070~trkorr as4user as4date as4time name_textc
      INTO (me->task, me->author, me->date, me->time, me->author_name)
      FROM e070
      INNER JOIN e071 ON e071~trkorr = e070~trkorr
      LEFT JOIN user_addr ON user_addr~bname = e070~as4user
      UP TO 1 ROWS
      WHERE strkorr = me->request
        AND object = s_vrsd-objtype
        AND obj_name = s_vrsd-objname
      ORDER BY as4date DESCENDING as4time DESCENDING.
      EXIT.
    ENDSELECT.
  ENDMETHOD.


  METHOD load_source.
    DATA t_trdir TYPE trdir_it.

    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        object_name = s_vrsd-objname
        object_type = s_vrsd-objtype
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
ENDCLASS.
