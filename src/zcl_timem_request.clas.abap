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

    METHODS get_latest_task_for_object
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
ENDCLASS.



CLASS zcl_timem_request IMPLEMENTATION.


  METHOD constructor.
    me->id = id.
    populate_details( id ).
  ENDMETHOD.


  METHOD get_imported_systems.
    DATA cofile                 TYPE ctslg_cofile.
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
