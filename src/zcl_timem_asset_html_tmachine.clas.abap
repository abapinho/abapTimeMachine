"! Renders the main HTML page from a Transformation which receives the deep
"! structure with all the required data
class ZCL_TIMEM_ASSET_HTML_TMACHINE definition
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
      !DATA type ZTIMEM_DATA .
  PROTECTED SECTION.
private section.

  data DATA type ZTIMEM_DATA .
ENDCLASS.



CLASS ZCL_TIMEM_ASSET_HTML_TMACHINE IMPLEMENTATION.


  METHOD constructor.
    me->data = data.
  ENDMETHOD.


  METHOD zif_timem_asset~get_content.
    CALL TRANSFORMATION ztimem_html_timemachine
    SOURCE data = data
    RESULT XML result
    OPTIONS xml_header = 'NO'.
  ENDMETHOD.


  METHOD zif_timem_asset~get_subtype.
    result = 'html'.
  ENDMETHOD.


  METHOD zif_timem_asset~get_url.
    result = 'timemachine.html'.
  ENDMETHOD.
ENDCLASS.
