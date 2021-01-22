"! Renders a CSS asset which is generated from a Transformation based on
"! the provided theme name
CLASS zcl_blame_asset_css DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_blame_asset_factory .

  PUBLIC SECTION.

    INTERFACES zif_blame_asset .

    "! Constructor for a CSS asset
    "! @parameter i_theme | Theme name which will determine which transformation to use
    "! to generate the CSS asset
    METHODS constructor
      IMPORTING
        !i_theme TYPE zblame_theme .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_theme_transformation TYPE char30.
ENDCLASS.



CLASS ZCL_BLAME_ASSET_CSS IMPLEMENTATION.


  METHOD constructor.
    g_theme_transformation = |ZBLAME_CSS_THEME_{ i_theme }|.
  ENDMETHOD.


  METHOD zif_blame_asset~get_content.
    DATA theme_css TYPE string.
    DATA(css) = |<css/>|.

    CALL TRANSFORMATION zblame_css_main
    SOURCE XML css
    RESULT XML r_content.

    CALL TRANSFORMATION (g_theme_transformation)
    SOURCE XML css
    RESULT XML theme_css.

    r_content = |{ r_content }{ theme_css }|.
  ENDMETHOD.


  METHOD zif_blame_asset~get_subtype.
    r_subtype = 'css'.
  ENDMETHOD.


  METHOD zif_blame_asset~get_url.
    r_url = 'abapblame.css'.
  ENDMETHOD.
ENDCLASS.
