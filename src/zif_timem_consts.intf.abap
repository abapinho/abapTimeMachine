INTERFACE zif_timem_consts
  PUBLIC .

  CONSTANTS version TYPE char10 VALUE '0.30b' ##NO_TEXT.

  CONSTANTS:
    BEGIN OF mode,
      time_machine TYPE zcl_timem_options=>ty_mode VALUE 'T',
      blame        TYPE zcl_timem_options=>ty_mode VALUE 'B',
    END OF mode .

  CONSTANTS:
    BEGIN OF asset_type,
      css  TYPE string VALUE 'CSS',
      html TYPE string VALUE 'HTML',
    END OF asset_type .

ENDINTERFACE.
