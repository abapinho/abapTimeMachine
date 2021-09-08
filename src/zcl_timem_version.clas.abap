"! Represents a version of a part of an object, including its source code
"! and several other attributes like author, request, etc.
CLASS zcl_timem_version DEFINITION
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
      END OF c_version .

    "! Version number from the VRSD table
    DATA version_number TYPE versno READ-ONLY .

    "! Transport request ID
    DATA request TYPE verskorrno READ-ONLY .

    "! Task ID (if exists)
    DATA task TYPE verskorrno READ-ONLY .

    " Username
    DATA author TYPE versuser READ-ONLY .

    " Name of the user (or username if no longer exists)
    DATA author_name TYPE ad_namtext READ-ONLY .

    " Date of version
    DATA date TYPE versdate READ-ONLY .

    " Time of version
    DATA time TYPE verstime READ-ONLY .

    "! Loading source event
    EVENTS loading_source
      EXPORTING
        VALUE(type) TYPE versobjtyp
        VALUE(name) TYPE versobjnam
        VALUE(version_number) TYPE versno .

    "! Takes a line of the VRSD table and fills all the attributes, including
    "! the source code already with blame information.
    METHODS constructor
      IMPORTING
        !vrsd TYPE vrsd
      RAISING
        zcx_timem .

    "! Returns the version source code including blame information.
    METHODS get_source
      RETURNING
        VALUE(result) TYPE ztimem_line_t
      RAISING
        zcx_timem .

    METHODS retrieve
      RAISING
        zcx_timem.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA vrsd TYPE vrsd.
    DATA gt_source TYPE abaptxt255_tab.

    METHODS load_attributes
      RAISING zcx_timem.

    METHODS load_source
      RAISING zcx_timem.

    METHODS get_real_version
      RETURNING VALUE(result) TYPE versno.

    "! Try to find the object in the request tasks because sometimes the request was created
    "! by someone who was not the actual developer. The tasks better reflects the object's author.
    "! If we find a task, we overwrite the author. We choose to pick the latest task.
    METHODS load_latest_task.
ENDCLASS.



CLASS ZCL_TIMEM_VERSION IMPLEMENTATION.


  METHOD constructor.
    me->vrsd = vrsd.
    load_attributes( ).
    load_latest_task( ).
  ENDMETHOD.


  METHOD get_real_version.
    " Technically the current version is 0 but in order to keep them properly sorted we're
    " setting it to magic number 99997 (because 'ACTIVE' is 99998 and 'MODIFIED' is 99999.
    " But when we're going to fetch it from the database we must use 0.
    result = COND #( WHEN me->version_number = c_version-latest THEN 0
                     ELSE me->version_number ).
  ENDMETHOD.


  METHOD get_source.
    DATA: s_line LIKE LINE OF result.

    s_line-version_number = me->version_number.
    s_line-request = me->request.
    s_line-task = me->task.
    s_line-author = me->author.
    s_line-author_name = me->author_name.
    s_line-date = me->date.
    s_line-time = me->time.
    s_line-timestamp = |{ me->date }{ me->time }|.

    load_source( ).

    LOOP AT gt_source INTO DATA(source_int).
      s_line-line_num = sy-tabix.
      s_line-source = source_int.
      INSERT s_line INTO TABLE result.
    ENDLOOP.
  ENDMETHOD.


  METHOD load_attributes.
    me->version_number = vrsd-versno.
    me->author = vrsd-author.
    me->date = vrsd-datum.
    me->time = vrsd-zeit.
    me->author_name = NEW zcl_timem_author( )->get_name( vrsd-author ).
    me->request = vrsd-korrnum.
  ENDMETHOD.


  METHOD load_latest_task.
    IF me->request IS INITIAL.
      RETURN.
    ENDIF.
    SELECT e070~trkorr as4user as4date as4time name_textc
      INTO (me->task, me->author, me->date, me->time, me->author_name)
      FROM e070
      INNER JOIN e071 ON e071~trkorr = e070~trkorr
      LEFT JOIN user_addr ON user_addr~bname = e070~as4user
      UP TO 1 ROWS
      WHERE strkorr = me->request
        AND object = vrsd-objtype
        AND obj_name = vrsd-objname
      ORDER BY as4date DESCENDING as4time DESCENDING.
      EXIT.
    ENDSELECT.
  ENDMETHOD.


  METHOD load_source.
    DATA t_trdir TYPE trdir_it.

    " If already loaded, skip it
    IF gt_source IS NOT INITIAL.
      RETURN.
    ENDIF.

    RAISE EVENT loading_source
      EXPORTING
        type           = vrsd-objtype
        name           = vrsd-objname
        version_number = me->version_number.

    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        object_name = vrsd-objname
        object_type = vrsd-objtype
        versno      = get_real_version( )
      TABLES
        repos_tab   = gt_source
        trdir_tab   = t_trdir
      EXCEPTIONS
        no_version  = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      " Ignore errors, just exit.
      ASSERT 1 = 1.
    ENDIF.
  ENDMETHOD.


  METHOD retrieve.
    DATA(real_version) = get_real_version( ).
    SUBMIT rsedtve1 AND RETURN                           "#EC CI_SUBMIT
             WITH objtype = vrsd-objtype
             WITH objname = vrsd-objname
             WITH versno  = real_version.
  ENDMETHOD.
ENDCLASS.
