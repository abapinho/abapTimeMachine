interface ZIF_TIMEM_CONSTS
  public .


  constants VERSION type CHAR10 value '0.25b' ##NO_TEXT.
  constants:
    BEGIN OF mode,
               time_machine TYPE zcl_timem_options=>ty_mode VALUE 'T',
               blame        TYPE zcl_timem_options=>ty_mode VALUE 'B',
             END OF mode .
  constants:
    BEGIN OF asset_type,
               css  TYPE string VALUE 'CSS',
               html TYPE string VALUE 'HTML',
             END OF asset_type .
endinterface.
