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

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS populate_details
      IMPORTING
        !id TYPE trkorr
      RAISING
        zcx_timem.
ENDCLASS.



CLASS ZCL_TIMEM_REQUEST IMPLEMENTATION.


  METHOD constructor.
    me->id = id.
    populate_details( id ).
  ENDMETHOD.


  METHOD get_imported_systems.
    DATA cofile                 TYPE ctslg_cofile.
    DATA datetime TYPE n LENGTH 14.

    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING
        iv_trkorr = id
      IMPORTING
        es_cofile = cofile.

    LOOP AT cofile-systems INTO DATA(system).
      datetime = REDUCE #(
        INIT: dt = '00000000000000'
        FOR step IN system-steps
        FOR action IN step-actions
        NEXT dt = COND #(
        WHEN action-date > dt(8) AND action-time > dt+8
        THEN |{ action-date DATE = RAW }{ action-time TIME = RAW }| ELSE dt ) ).
      result = VALUE #(
        BASE result
        ( sysid = system-systemid
          subrc = system-rc
          date = datetime(8)
          time = datetime+8(6) ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD populate_details.
    SELECT as4text trstatus INTO (description, status)
    UP TO 1 ROWS
    FROM e070
    INNER JOIN e07t ON e07t~trkorr = e070~trkorr
    WHERE e070~trkorr = id
      AND langu  = 'E'
    ORDER BY as4text trstatus.
      EXIT.
    ENDSELECT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_timem.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
