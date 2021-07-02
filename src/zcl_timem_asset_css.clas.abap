"! Renders a CSS asset which is generated from a Transformation based on
"! the provided theme name
class ZCL_TIMEM_ASSET_CSS definition
  public
  final
  create private

  global friends ZCL_TIMEM_ASSET_FACTORY .

public section.

  interfaces ZIF_TIMEM_ASSET .

    "! Constructor for a CSS asset
    "! @parameter theme | Theme name which will determine which transformation to use
    "! to generate the CSS asset
  methods CONSTRUCTOR
    importing
      !theme type ZTIMEM_THEME .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_theme_transformation TYPE char30.
ENDCLASS.



CLASS ZCL_TIMEM_ASSET_CSS IMPLEMENTATION.


  METHOD constructor.
    g_theme_transformation = |ZTIMEM_CSS_THEME_{ theme }|.
  ENDMETHOD.


  METHOD zif_timem_asset~get_content.
    DATA theme_css TYPE string.
    DATA(css) = |<css/>|.

    CALL TRANSFORMATION ztimem_css_main
    SOURCE XML css
    RESULT XML result.

    CALL TRANSFORMATION (g_theme_transformation)
    SOURCE XML css
    RESULT XML theme_css.

    result = |{ result }{ theme_css }|.
  ENDMETHOD.


  METHOD zif_timem_asset~get_subtype.
    result = 'css'.
  ENDMETHOD.


  METHOD zif_timem_asset~get_url.
    result = 'abaptimemachine.css'.
  ENDMETHOD.
ENDCLASS.
