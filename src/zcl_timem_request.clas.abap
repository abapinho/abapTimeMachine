"! Represents an SAP transport request
class ZCL_TIMEM_REQUEST definition
  public
  final
  create public .

public section.

    "! Transport request ID
  data ID type TRKORR read-only .
    "! Transport request description
  data DESCRIPTION type AS4TEXT read-only .

    "! Constructs an instance for the given request ID
  methods CONSTRUCTOR
    importing
      !I_REQUEST type TRKORR .

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_description
      IMPORTING
                !i_request           TYPE trkorr
      RETURNING VALUE(result) TYPE as4text.
ENDCLASS.



CLASS ZCL_TIMEM_REQUEST IMPLEMENTATION.


  METHOD constructor.
    me->id = i_request.
    me->description = get_description( i_request ).
  ENDMETHOD.


  METHOD get_description.
    SELECT SINGLE as4text INTO result
    FROM e07t
    WHERE trkorr = i_request
      AND langu  = 'E'.
  ENDMETHOD.
ENDCLASS.
