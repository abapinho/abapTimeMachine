CLASS zcl_blame_asset_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS create_instance
      IMPORTING
                i_asset_type    TYPE string
                is_parts        TYPE zblame_parts
      RETURNING VALUE(ro_asset) TYPE REF TO zif_blame_asset.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_blame_asset_factory IMPLEMENTATION.
  METHOD create_instance.
    DATA(o_options) = zcl_blame_options=>get_instance( ).
    ro_asset = SWITCH #(
      i_asset_type
      WHEN 'CSS' THEN NEW zcl_blame_asset_css( o_options->theme )
      WHEN 'HTML' THEN
        SWITCH #(
          o_options->mode
          WHEN zif_blame_consts=>mode-blame THEN NEW zcl_blame_asset_html_blame( is_parts )
          WHEN zif_blame_consts=>mode-time_machine THEN NEW zcl_blame_asset_html_tmachine( is_parts ) ) ).
  ENDMETHOD.
ENDCLASS.
