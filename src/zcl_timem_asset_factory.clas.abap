class ZCL_TIMEM_ASSET_FACTORY definition
  public
  final
  create public .

public section.

  methods CREATE_INSTANCE
    importing
      !ASSET_TYPE type STRING
      !DATA type ZTIMEM_DATA
    returning
      value(RESULT) type ref to ZIF_TIMEM_ASSET .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TIMEM_ASSET_FACTORY IMPLEMENTATION.


  METHOD create_instance.
    DATA(options) = zcl_timem_options=>get_instance( ).
    result = SWITCH #(
      asset_type
      WHEN 'CSS' THEN NEW zcl_timem_asset_css( options->theme )
      WHEN 'HTML' THEN
        SWITCH #(
          options->mode
          WHEN zif_timem_consts=>mode-blame THEN NEW zcl_timem_asset_html_blame( data )
          WHEN zif_timem_consts=>mode-time_machine THEN NEW zcl_timem_asset_html_tmachine( data ) ) ).
  ENDMETHOD.
ENDCLASS.
