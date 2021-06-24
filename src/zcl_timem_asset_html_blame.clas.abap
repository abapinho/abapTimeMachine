"! Renders the main HTML page from a Transformation which receives the deep
"! structure with all the required data
class ZCL_TIMEM_ASSET_HTML_BLAME definition
  public
  final
  create private

  global friends ZCL_TIMEM_ASSET_FACTORY .

public section.

  interfaces ZIF_TIMEM_ASSET .

    "! Constructor for the main HTML asset
    "! @parameter is_parts | Deep structure containing all the information that
    "! is to be rendered as HTML.
  methods CONSTRUCTOR
    importing
      !IS_PARTS type ZTIMEM_PARTS .
  PROTECTED SECTION.
private section.

  data GS_PARTS type ZTIMEM_PARTS .
ENDCLASS.



CLASS ZCL_TIMEM_ASSET_HTML_BLAME IMPLEMENTATION.


  METHOD constructor.
    gs_parts = is_parts.
  ENDMETHOD.


  METHOD zif_timem_asset~get_content.
    CALL TRANSFORMATION ztimem_html_blame
    SOURCE parts = gs_parts
    RESULT XML result
    OPTIONS xml_header = 'NO'.
  ENDMETHOD.


  METHOD zif_timem_asset~get_subtype.
    result = 'html'.
  ENDMETHOD.


  METHOD zif_timem_asset~get_url.
    result = 'blame.html'.
  ENDMETHOD.
ENDCLASS.
