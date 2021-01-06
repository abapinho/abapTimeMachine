CLASS zcl_blame_options DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Diff operation should ignore case
    DATA ignore_case TYPE boolean READ-ONLY .

    "! Diff operation should ignore indentation
    DATA ignore_indentation TYPE boolean READ-ONLY .

    "! CSS theme name
    DATA theme TYPE zblame_theme READ-ONLY.

    METHODS constructor
      IMPORTING
        !i_ignore_case        TYPE boolean DEFAULT abap_false
        !i_ignore_indentation TYPE boolean DEFAULT abap_false
        !i_theme              TYPE zblame_theme DEFAULT zcl_blame_gui_viewer=>c_theme-light.
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
