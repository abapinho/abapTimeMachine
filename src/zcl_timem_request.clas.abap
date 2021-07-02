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
        !i_request TYPE trkorr .

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS populate_details
      IMPORTING
        !i_request TYPE trkorr.
ENDCLASS.



CLASS ZCL_TIMEM_REQUEST IMPLEMENTATION.


  METHOD constructor.
    me->id = i_request.
    populate_details( i_request ).
  ENDMETHOD.


  METHOD populate_details.
    SELECT SINGLE as4text trstatus INTO (description, status)
    FROM e070
    INNER JOIN e07t ON e07t~trkorr = e070~trkorr
    WHERE e070~trkorr = i_request
      AND langu  = 'E'.
  ENDMETHOD.
ENDCLASS.
