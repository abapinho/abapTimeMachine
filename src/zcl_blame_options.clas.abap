CLASS zcl_blame_options DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    "! Diff operation should ignore case
    DATA ignore_case TYPE boolean READ-ONLY .

    "! Diff operation should ignore indentation
    DATA ignore_indentation TYPE boolean READ-ONLY .

    "! CSS theme name
    DATA theme TYPE zblame_theme READ-ONLY.

    " Date
    DATA date TYPE datum READ-ONLY.

    " Time
    DATA time TYPE uzeit READ-ONLY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_blame_options.

    METHODS set
      IMPORTING
        !i_ignore_case        TYPE boolean OPTIONAL
        !i_ignore_indentation TYPE boolean OPTIONAL
        !i_date               TYPE datum OPTIONAL
        !i_time               TYPE uzeit OPTIONAL
        !i_theme              TYPE zblame_theme OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: go_instance TYPE REF TO zcl_blame_options.
ENDCLASS.



CLASS ZCL_BLAME_OPTIONS IMPLEMENTATION.


  METHOD class_constructor.
    go_instance = NEW #( ).
    go_instance->set(
        i_ignore_case        = abap_false
        i_ignore_indentation = abap_false
        i_date               = sy-datum
        i_time               = '235959'
        i_theme              = zcl_blame_gui_viewer=>c_theme-light ).
  ENDMETHOD.


  METHOD get_instance.
    ro_instance = go_instance.
  ENDMETHOD.


  METHOD set.
    IF i_ignore_case IS SUPPLIED.
      me->ignore_case = i_ignore_case.
    ENDIF.

    IF i_ignore_indentation IS SUPPLIED.
      me->ignore_indentation = i_ignore_indentation.
    ENDIF.

    IF i_theme IS SUPPLIED.
      me->theme = i_theme.
    ENDIF.

    IF i_date IS SUPPLIED.
      me->date = i_date.
    ENDIF.

    IF i_time IS SUPPLIED.
      me->time = i_time.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
