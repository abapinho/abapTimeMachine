"! Renders the main HTML page from a Transformation which receives the deep
"! structure with all the required data
CLASS zcl_blame_asset_html_tmachine DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_blame_asset_factory .

  PUBLIC SECTION.

    INTERFACES zif_blame_asset .

    "! Constructor for the main HTML asset
    "! @parameter is_parts | Deep structure containing all the information that
    "! is to be rendered as HTML.
    METHODS constructor
      IMPORTING
        is_parts TYPE zblame_parts.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA gs_parts TYPE zblame_parts.
ENDCLASS.



CLASS ZCL_BLAME_ASSET_HTML_TMACHINE IMPLEMENTATION.


  METHOD constructor.
    gs_parts = is_parts.
  ENDMETHOD.


  METHOD zif_blame_asset~get_content.
    CALL TRANSFORMATION zblame_html_timemachine
    SOURCE parts = gs_parts
    RESULT XML r_content
    OPTIONS xml_header = 'NO'.
  ENDMETHOD.


  METHOD zif_blame_asset~get_subtype.
    r_subtype = 'html'.
  ENDMETHOD.


  METHOD zif_blame_asset~get_url.
    r_url = 'timemachine.html'.
  ENDMETHOD.
ENDCLASS.
