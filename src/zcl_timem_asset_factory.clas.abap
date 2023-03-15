CLASS zcl_timem_asset_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS create_instance
      IMPORTING
        !asset_type   TYPE string
        !data         TYPE ztimem_data
        mode          TYPE ztimem_mode
        theme         TYPE ztimem_theme
      RETURNING
        VALUE(result) TYPE REF TO zif_timem_asset .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_timem_asset_factory IMPLEMENTATION.


  METHOD create_instance.
    result = SWITCH #(
      asset_type
      WHEN zcl_timem_consts=>asset_type-css THEN NEW zcl_timem_asset_css( theme )
      WHEN zcl_timem_consts=>asset_type-html THEN
        SWITCH #(
          mode
          WHEN zcl_timem_consts=>mode-blame THEN NEW zcl_timem_asset_html_blame( data )
          WHEN zcl_timem_consts=>mode-time_machine THEN NEW zcl_timem_asset_html_tmachine( data ) ) ).
  ENDMETHOD.
ENDCLASS.
