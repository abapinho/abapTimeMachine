INTERFACE zif_blame_consts
  PUBLIC .


  CONSTANTS version TYPE char10 VALUE '0.23b' ##NO_TEXT.
  CONSTANTS: BEGIN OF mode,
               blame        TYPE zblame_mode VALUE 'M',
               time_machine TYPE zblame_mode VALUE 'T',
             END OF mode.
  CONSTANTS: BEGIN OF asset_type,
               css  TYPE string VALUE 'CSS',
               html TYPE string VALUE 'HTML',
             END OF asset_type.
ENDINTERFACE.
