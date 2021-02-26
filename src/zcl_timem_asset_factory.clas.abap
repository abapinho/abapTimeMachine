class ZCL_TIMEM_ASSET_FACTORY definition
  public
  final
  create public .

public section.

  methods CREATE_INSTANCE
    importing
      !I_ASSET_TYPE type STRING
      !IS_PARTS type ZTIMEM_PARTS
    returning
      value(RO_ASSET) type ref to ZIF_TIMEM_ASSET .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TIMEM_ASSET_FACTORY IMPLEMENTATION.


  METHOD create_instance.
    DATA(o_options) = zcl_timem_options=>get_instance( ).
    ro_asset = SWITCH #(
      i_asset_type
      WHEN 'CSS' THEN NEW zcl_TIMEM_asset_css( o_options->theme )
      WHEN 'HTML' THEN
        SWITCH #(
          o_options->mode
          WHEN zif_timem_consts=>mode-blame THEN NEW zcl_timem_asset_html_blame( is_parts )
          WHEN zif_timem_consts=>mode-time_machine THEN NEW zcl_timem_asset_html_tmachine( is_parts ) ) ).
  ENDMETHOD.
ENDCLASS.
