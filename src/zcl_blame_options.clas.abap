CLASS zcl_blame_options DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    "! Mode (Blame or Time Machine)
    DATA mode TYPE zblame_mode READ-ONLY.

    "! Diff operation should ignore case
    DATA ignore_case TYPE boolean READ-ONLY .

    "! Diff operation should ignore indentation
    DATA ignore_indentation TYPE boolean READ-ONLY .

    "! CSS theme name
    DATA theme TYPE zblame_theme READ-ONLY.

    "! Timestamp
    DATA timestamp TYPE timestamp READ-ONLY.

    "! Date
    DATA date TYPE datum READ-ONLY.

    "! Time
    DATA time TYPE uzeit READ-ONLY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_blame_options.

    METHODS set
      IMPORTING
        !i_mode               TYPE zblame_mode OPTIONAL
        !i_ignore_case        TYPE boolean OPTIONAL
        !i_ignore_indentation TYPE boolean OPTIONAL
        !i_timestamp          TYPE timestamp OPTIONAL
        !i_theme              TYPE zblame_theme OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: go_instance TYPE REF TO zcl_blame_options.
ENDCLASS.



CLASS zcl_blame_options IMPLEMENTATION.


  METHOD class_constructor.
    go_instance = NEW #( ).
    go_instance->set(
        i_mode               = zif_blame_consts=>mode-blame
        i_ignore_case        = abap_false
        i_ignore_indentation = abap_false
        i_timestamp          = CONV #( |{ sy-datum }235959| )
        i_theme              = zcl_blame_gui_viewer=>c_theme-light ).
  ENDMETHOD.


  METHOD get_instance.
    ro_instance = go_instance.
  ENDMETHOD.


  METHOD set.
    IF i_mode IS SUPPLIED.
      me->mode = i_mode.
    ENDIF.

    IF i_ignore_case IS SUPPLIED.
      me->ignore_case = i_ignore_case.
    ENDIF.

    IF i_ignore_indentation IS SUPPLIED.
      me->ignore_indentation = i_ignore_indentation.
    ENDIF.

    IF i_theme IS SUPPLIED.
      me->theme = i_theme.
    ENDIF.

    IF i_timestamp IS SUPPLIED.
      me->timestamp = i_timestamp.
      CONVERT TIME STAMP i_timestamp TIME ZONE sy-zonlo INTO DATE me->date TIME me->time.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
