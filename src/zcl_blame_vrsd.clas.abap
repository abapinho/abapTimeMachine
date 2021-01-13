"! List of VRSD lines for existing versions of a given object.
CLASS zcl_blame_vrsd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! List of relevant lines of VRSD table (plus artificial lines created
    "! for the active and modified versions).
    DATA t_vrsd TYPE vrsd_tab READ-ONLY.

    "! Constructor which takes an object type and name and loads all the VRSD
    "! data, as well as the eventual artificial lines for the active and modified
    "! versions.
    METHODS constructor
      IMPORTING
                i_type     TYPE versobjtyp
                i_name     TYPE versobjnam
      RAISING   zcx_blame.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA type TYPE versobjtyp.
    DATA name TYPE versobjnam.
    DATA request_active_modif TYPE trkorr.

    METHODS load_from_table.

    METHODS load_active_or_modified
      IMPORTING
                i_versno TYPE versno
      RAISING   zcx_blame.

    METHODS get_request_active_modif
      RETURNING VALUE(r_request) TYPE trkorr
      RAISING   zcx_blame.
ENDCLASS.



CLASS ZCL_BLAME_VRSD IMPLEMENTATION.


  METHOD constructor.
    me->type = i_type.
    me->name = i_name.
    load_from_table( ).
    load_active_or_modified( zcl_blame_version=>c_version-active ).
    load_active_or_modified( zcl_blame_version=>c_version-modified ).
    SORT me->t_vrsd BY versno.
  ENDMETHOD.


  METHOD get_request_active_modif.
    IF request_active_modif IS NOT INITIAL.
      r_request = request_active_modif.
    ENDIF.

    DATA s_ko100 TYPE ko100.
    CALL FUNCTION 'TR_GET_PGMID_FOR_OBJECT'
      EXPORTING
        iv_object      = me->type
      IMPORTING
        es_type        = s_ko100
      EXCEPTIONS
        illegal_object = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_blame. " TODO
    ENDIF.

    DATA(s_e071) = VALUE e071( pgmid = s_ko100-pgmid
                               object = me->type
                               obj_name = me->name ).
    DATA locked TYPE trparflag.
    DATA s_tlock_key TYPE tlock_int.
    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071     = s_e071
      IMPORTING
        pe_result   = locked
        we_lock_key = s_tlock_key.
    IF locked <> 'L'.
      RETURN.
    ENDIF.

    DATA s_tlock TYPE tlock.
    CALL FUNCTION 'TRINT_CHECK_LOCKS'
      EXPORTING
        wi_lock_key = s_tlock_key
      IMPORTING
        we_lockflag = locked
        we_tlock    = s_tlock
      EXCEPTIONS
        empty_key   = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_blame
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF locked IS INITIAL.
      RETURN.
    ENDIF.

    r_request = request_active_modif = s_tlock-trkorr.
  ENDMETHOD.


  METHOD load_active_or_modified.
    DATA(s_obj) = VALUE svrs2_versionable_object(
      objtype = me->type
      data_pointer = me->type
      objname = me->name
      header_only = abap_true ).
    CALL FUNCTION 'SVRS_INITIALIZE_DATAPOINTER'
      CHANGING
        objtype      = me->type
        data_pointer = me->type.
    DATA(mode) = SWITCH char1(
      i_versno
      WHEN zcl_blame_version=>c_version-active THEN 'A'
      WHEN zcl_blame_version=>c_version-modified THEN 'M' ).
    CALL FUNCTION 'SVRS_GET_VERSION_REPOSITORY'
      EXPORTING
        mode      = mode
      CHANGING
        obj       = s_obj
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA s_vrsd TYPE vrsd.
    CALL FUNCTION 'SVRS_EXTRACT_INFO_FROM_OBJECT'
      EXPORTING
        object    = s_obj
      CHANGING
        vrsd_info = s_vrsd.
    IF s_vrsd-author IS INITIAL.
      RAISE EXCEPTION TYPE zcx_blame. " TODO
    ENDIF.

    s_vrsd-versno = i_versno.
    s_vrsd-objtype = me->type.
    s_vrsd-objname = me->name.
    s_vrsd-korrnum = get_request_active_modif( ).
    INSERT s_vrsd INTO TABLE me->t_vrsd.
  ENDMETHOD.


  METHOD load_from_table.
    SELECT * INTO TABLE me->t_vrsd
      FROM vrsd
      WHERE objtype = me->type
        AND objname = me->name.

    " We consider the current version to be 99998 instead of 0
    LOOP AT me->t_vrsd ASSIGNING FIELD-SYMBOL(<s_vrsd>)
      WHERE versno = zcl_blame_version=>c_version-latest_db.
      <s_vrsd>-versno = zcl_blame_version=>c_version-latest.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
