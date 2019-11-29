CLASS zcl_blame_asset_css DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_blame_asset .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_blame_asset_css IMPLEMENTATION.
  METHOD zif_blame_asset~get_url.
    r_url = 'abapblame.css'.
  ENDMETHOD.


  METHOD zif_blame_asset~get_subtype.
    r_subtype = 'css'.
  ENDMETHOD.


  METHOD zif_blame_asset~get_content.
    DATA(css) = |<css/>|.

    CALL TRANSFORMATION zblame_css
    SOURCE XML css
    RESULT XML r_content.
  ENDMETHOD.
ENDCLASS.
