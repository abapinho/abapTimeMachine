interface ZIF_TIMEM_USEREXIT
  public .


  methods BEFORE_RENDERING
    importing
      !OPTIONS type ref to ZCL_TIMEM_OPTIONS
    changing
      !DATA type ZTIMEM_DATA .
  methods ON_SAPEVENT
    importing
      !OPTIONS type ref to ZCL_TIMEM_OPTIONS
      !ACTION type C
      !GETDATA type C .
  methods MODIFY_PART_LIST
    importing
      !OPTIONS type ref to ZCL_TIMEM_OPTIONS
    changing
      !PART_LIST type ZTIMEM_PART_T .
  methods MODIFY_ASSET_CONTENT
    importing
      !OPTIONS type ref to ZCL_TIMEM_OPTIONS
      !SUBTYPE type C
    changing
      !CONTENT type STRING .
endinterface.
