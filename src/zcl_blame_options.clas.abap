CLASS zcl_blame_options DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA ignore_case TYPE boolean READ-ONLY .
    DATA ignore_indentation TYPE boolean READ-ONLY .
    DATA theme TYPE zblame_theme READ-ONLY.

    METHODS constructor
      IMPORTING
        !i_ignore_case        TYPE boolean DEFAULT abap_false
        !i_ignore_indentation TYPE boolean DEFAULT abap_false
        !i_theme              TYPE zblame_theme DEFAULT zcl_blame_output=>c_theme-light.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BLAME_OPTIONS IMPLEMENTATION.


  METHOD constructor.
    me->ignore_case = i_ignore_case.
    me->ignore_indentation = i_ignore_indentation.
    me->theme = i_theme.
  ENDMETHOD.
ENDCLASS.
