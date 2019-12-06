CLASS zcl_blame_request DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA id TYPE trkorr READ-ONLY.
    DATA description TYPE as4text READ-ONLY.

    METHODS constructor
      IMPORTING
        !i_request TYPE trkorr.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_description
      IMPORTING
                !i_request           TYPE trkorr
      RETURNING VALUE(r_description) TYPE as4text.
ENDCLASS.



CLASS ZCL_BLAME_REQUEST IMPLEMENTATION.


  METHOD constructor.
    me->id = i_request.
    me->description = get_description( i_request ).
  ENDMETHOD.


  METHOD get_description.
    SELECT SINGLE as4text INTO r_description
    FROM e07t
    WHERE trkorr = i_request
      AND langu  = 'E'.
  ENDMETHOD.
ENDCLASS.
