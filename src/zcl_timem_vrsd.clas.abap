"! List of VRSD lines for existing versions of a given object.
CLASS zcl_timem_vrsd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! List of relevant lines of VRSD table (plus artificial lines created
    "! for the active and modified versions).
    DATA vrsd_list TYPE vrsd_tab READ-ONLY .

    "! Constructor which takes an object type and name and loads all the VRSD
    "! data, as well as the eventual artificial lines for the active and modified
    "! versions.
    METHODS constructor
      IMPORTING
        !type TYPE versobjtyp
        !name TYPE versobjnam
      RAISING
        zcx_timem .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA type TYPE versobjtyp.
    DATA name TYPE versobjnam.
    DATA request_active_modif TYPE trkorr.
    DATA options TYPE REF TO zcl_timem_options.

    METHODS load_from_table.

    METHODS load_active_or_modified
      IMPORTING
                versno TYPE versno
      RAISING   zcx_timem.

    METHODS get_request_active_modif
      RETURNING VALUE(result) TYPE trkorr
      RAISING   zcx_timem.

    METHODS determine_request_active_modif
      RETURNING VALUE(result) TYPE trkorr
      RAISING   zcx_timem.
ENDCLASS.



CLASS zcl_timem_vrsd IMPLEMENTATION.


  METHOD constructor.
    me->type = type.
    me->name = name.
    me->options = zcl_timem_options=>get_instance( ).
    load_from_table( ).
    IF options->ignore_unreleased = abap_false.
      " Even released parts have an active version. We know it is unreleased if the
      " request is not empty. Otherwise we will disregard it.
      IF get_request_active_modif(  ) IS NOT INITIAL.
        load_active_or_modified( zcl_timem_version=>c_version-active ).
      ENDIF.
      load_active_or_modified( zcl_timem_version=>c_version-modified ).
    ENDIF.
    SORT me->vrsd_list BY versno ASCENDING.
  ENDMETHOD.


  METHOD determine_request_active_modif.
    DATA s_ko100 TYPE ko100.
    DATA locked TYPE trparflag.
    DATA s_tlock_key TYPE tlock_int.
    DATA s_tlock TYPE tlock.

    CALL FUNCTION 'TR_GET_PGMID_FOR_OBJECT'
      EXPORTING
        iv_object      = me->type
      IMPORTING
        es_type        = s_ko100
      EXCEPTIONS
        illegal_object = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_timem.
    ENDIF.

    DATA(s_e071) = VALUE e071( pgmid = s_ko100-pgmid
                               object = me->type
                               obj_name = me->name ).
    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071     = s_e071
      IMPORTING
        pe_result   = locked
        we_lock_key = s_tlock_key.
    IF locked <> 'L'.
      RETURN.
    ENDIF.

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
      zcx_timem=>raise_from_syst( ).
    ENDIF.

    IF locked IS INITIAL.
      RETURN.
    ENDIF.

    result = s_tlock-trkorr.
  ENDMETHOD.


  METHOD get_request_active_modif.
    IF request_active_modif IS INITIAL.
      request_active_modif = determine_request_active_modif( ).
    ENDIF.
    result = request_active_modif.
  ENDMETHOD.


  METHOD load_active_or_modified.
    DATA vrsd TYPE vrsd.

    DATA(obj) = VALUE svrs2_versionable_object(
      objtype = me->type
      data_pointer = me->type
      objname = me->name
      header_only = abap_true ).

    CALL FUNCTION 'SVRS_INITIALIZE_DATAPOINTER'
      CHANGING
        objtype      = me->type
        data_pointer = me->type.

    DATA(mode) = SWITCH char1(
      versno
      WHEN zcl_timem_version=>c_version-active THEN 'A'
      WHEN zcl_timem_version=>c_version-modified THEN 'M' ).

    CALL FUNCTION 'SVRS_GET_VERSION_REPOSITORY'
      EXPORTING
        mode      = mode
      CHANGING
        obj       = obj
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SVRS_EXTRACT_INFO_FROM_OBJECT'
      EXPORTING
        object    = obj
      CHANGING
        vrsd_info = vrsd.
    IF vrsd-author IS INITIAL.
      RAISE EXCEPTION TYPE zcx_timem.
    ENDIF.

    " Unreleased versions will be set to current date and time because, while
    " different parts will most probably have different date+time creation date,
    " each one is not really a version. At least not yet. And without this hack
    " we'd get one version moment for each unreleased part, which is not realistic.
    vrsd-datum = sy-datum.
    vrsd-zeit = sy-uzeit.

    vrsd-versno = versno.
    vrsd-objtype = me->type.
    vrsd-objname = me->name.
    vrsd-korrnum = get_request_active_modif( ).

    INSERT vrsd INTO TABLE me->vrsd_list.
  ENDMETHOD.


  METHOD load_from_table.
    DATA: versno_range TYPE RANGE OF versno.

    IF options->ignore_unreleased = abap_true.
      versno_range = VALUE #(
        option = 'NE'
        sign = 'I'
        ( low = '00000' ) ).
    ENDIF.

    SELECT * INTO TABLE me->vrsd_list
      FROM vrsd
      WHERE objtype = me->type
        AND objname = me->name
        AND versno IN versno_range
      ORDER BY PRIMARY KEY.

    " We consider the current version to be 99998 instead of 0
    LOOP AT me->vrsd_list ASSIGNING FIELD-SYMBOL(<s_vrsd>)
      WHERE versno = zcl_timem_version=>c_version-latest_db.
      <s_vrsd>-versno = zcl_timem_version=>c_version-latest.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
