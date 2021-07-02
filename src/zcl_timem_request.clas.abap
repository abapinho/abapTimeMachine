"! Represents an SAP transport request
CLASS zcl_timem_request DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Transport request ID
    DATA id TYPE trkorr READ-ONLY .
    "! Transport request description
    DATA description TYPE as4text READ-ONLY .
    "! Transport request status
    DATA status TYPE trstatus READ-ONLY.

    "! Constructs an instance for the given request ID
    METHODS constructor
      IMPORTING
        !id TYPE trkorr .

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS populate_details
      IMPORTING
        !id TYPE trkorr.
ENDCLASS.



CLASS ZCL_TIMEM_REQUEST IMPLEMENTATION.


  METHOD constructor.
    me->id = id.
    populate_details( id ).
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
  ENDMETHOD.
ENDCLASS.
