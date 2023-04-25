"! Represents an SAP transport request
CLASS zcl_timem_request DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_s_system,
        sysid TYPE sysid,
        subrc TYPE sysubrc,
        date  TYPE as4date,
        time  TYPE as4time,
      END OF ty_s_system,
      ty_t_system TYPE STANDARD TABLE OF ty_s_system WITH KEY sysid.

    "! Transport request ID
    DATA id TYPE trkorr READ-ONLY .
    "! Transport request description
    DATA description TYPE as4text READ-ONLY .
    "! Transport request status
    DATA status TYPE trstatus READ-ONLY.

    "! Constructs an instance for the given request ID
    METHODS constructor
      IMPORTING
                !id TYPE trkorr
      RAISING   zcx_timem.

    METHODS get_imported_systems
      RETURNING
        VALUE(result) TYPE ty_t_system.

    METHODS get_task_for_object
      IMPORTING
                object_type   TYPE versobjtyp
                object_name   TYPE versobjnam
      RETURNING VALUE(result) TYPE e070.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_date_time,
        date TYPE datum,
        time TYPE uzeit,
      END OF ty_date_time.

    METHODS populate_details
      IMPORTING
        !id TYPE trkorr
      RAISING
        zcx_timem.

    METHODS get_system_latest_datetime
      IMPORTING
                system        TYPE ctslg_system
      RETURNING VALUE(result) TYPE ty_date_time.

    METHODS get_latest_task_for_object
      IMPORTING
                object_type   TYPE versobjtyp
                object_name   TYPE versobjnam
      RETURNING VALUE(result) TYPE e070.

    METHODS get_task_if_only_one
      RETURNING VALUE(result) TYPE e070.
ENDCLASS.



CLASS zcl_timem_request IMPLEMENTATION.


  METHOD constructor.
    me->id = id.
    populate_details( id ).
  ENDMETHOD.


  METHOD get_imported_systems.
    DATA cofile TYPE ctslg_cofile.
    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING
        iv_trkorr = id
      IMPORTING
        es_cofile = cofile.

    LOOP AT cofile-systems INTO DATA(system).
      DATA(dt) = get_system_latest_datetime( system ).
      result = VALUE #(
        BASE result
        ( sysid = system-systemid
          subrc = system-rc
          date = dt-date
          time = dt-time ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD populate_details.
    SELECT as4text, trstatus INTO (@description, @status)
    UP TO 1 ROWS
    FROM e070
    LEFT JOIN e07t ON e07t~trkorr = e070~trkorr WHERE e070~trkorr = @id
    ORDER BY as4text, trstatus.
      EXIT.
    ENDSELECT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_timem.
    ENDIF.
  ENDMETHOD.


  METHOD get_system_latest_datetime.
    result = REDUCE #(
      INIT dt TYPE ty_date_time
      FOR step IN system-steps
      FOR action IN step-actions
      NEXT dt = COND #(
      WHEN action-date > dt-date AND action-time > dt-time
      THEN VALUE #( date = |{ action-date DATE = RAW }| time = |{ action-time TIME = RAW }| )
      ELSE dt ) ).
  ENDMETHOD.

  METHOD get_task_for_object.
    " TODO
    " This approach was taken because the relationship between VRSD and E071
    " is not direct so sometimes method get_latest_task_for_object fails to
    " find anything in E071. Assuming that the most common scenario is for a
    " TR to have only one task, we first try to get a single task and only if
    " there is more than one do we use E071. This doesn't solve 100% of the
    " cases but should take care of many. I still hope to find a definitive
    " solution someday though.
    result = get_task_if_only_one( ).

    IF result IS INITIAL.
      " If here then there is more than one task for this request
      result = get_latest_task_for_object(
                 object_type = object_type
                 object_name = object_name ).
    ENDIF.

    IF result IS INITIAL AND object_type = 'REPS'.
      " This is a unfortunate hack. I hope to find a way to avoid this.
      " For some reason in VRSD the object type for programs is REPS
      " (which stands for report source) for a given TR, but in E071 the
      " object type registered is PROG. This is a workaround to
      " be sure the request details are found in those cases.
      result = get_task_for_object(
                 object_type = 'PROG'
                 object_name = object_name ).
    ENDIF.

    " For TRs with more than one task for which the relationship between VRSD
    " and E071 is not direct this can still fail. Example:
    " - VRSD: REPS + LZFUNCTIONGROUPNAMETOP
    " - E071: FUGR + ZFUNCTIONGROUP
  ENDMETHOD.


  METHOD get_task_if_only_one.
    DATA e070_list TYPE STANDARD TABLE OF e070.
    SELECT trkorr as4user as4date as4time
      INTO CORRESPONDING FIELDS OF TABLE e070_list
      FROM e070
      WHERE strkorr = me->id.
    IF lines( e070_list ) = 1.
      result = e070_list[ 1 ].
    ENDIF.
  ENDMETHOD.


  METHOD get_latest_task_for_object.
    SELECT e070~trkorr as4user as4date as4time
      INTO (result-trkorr, result-as4user, result-as4date, result-as4time)
      FROM e070
      INNER JOIN e071 ON e071~trkorr = e070~trkorr
      UP TO 1 ROWS
      WHERE strkorr = me->id
        AND object = object_type
        AND obj_name = object_name
      ORDER BY as4date DESCENDING as4time DESCENDING.
      EXIT.
    ENDSELECT.
  ENDMETHOD.
ENDCLASS.
