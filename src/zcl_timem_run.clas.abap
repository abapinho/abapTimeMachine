"! Main entry point. Instantiated by the program to run the whole show:
"! 1. Load all the information
"! 2. Display results in HTML page
class ZCL_TIMEM_RUN definition
  public
  final
  create public .

public section.

    "! Execute everything
  methods GO
    importing
      !I_OBJECT_TYPE type ZTIMEM_OBJECT_TYPE
      !I_OBJECT_NAME type SOBJ_NAME
    raising
      ZCX_TIMEM .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS on_loading_version_source
          FOR EVENT loading_source OF zcl_timem_version
      IMPORTING
          !type
          !name
          !version_number.
ENDCLASS.



CLASS ZCL_TIMEM_RUN IMPLEMENTATION.


  METHOD go.
    SET HANDLER me->on_loading_version_source FOR ALL INSTANCES.
    DATA(o_parts) = NEW zcl_timem_parts( i_object_type = i_object_type
                                         i_object_name = i_object_name ).
    NEW zcl_timem_gui( o_parts )->display( ).
  ENDMETHOD.


  METHOD on_loading_version_source.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text       = |Loading { type } { name } { version_number }|.
  ENDMETHOD.
ENDCLASS.
