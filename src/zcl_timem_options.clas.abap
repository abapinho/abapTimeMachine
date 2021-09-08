CLASS zcl_timem_options DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES ty_mode TYPE char1 .

    "! Mode (Blame or Time Machine)
    DATA mode TYPE ty_mode READ-ONLY .

    "! Diff operation should ignore case
    DATA ignore_case TYPE boolean READ-ONLY .

    "! Diff operation should ignore indentation
    DATA ignore_indentation TYPE boolean READ-ONLY .

    "! CSS theme name
    DATA theme TYPE ztimem_theme READ-ONLY .

    "! Timestamp
    DATA timestamp TYPE timestamp READ-ONLY .

    "! Date
    DATA date TYPE datum READ-ONLY .

    "! Time
    DATA time TYPE uzeit READ-ONLY .

    " Ignore unreleased requests
    DATA ignore_unreleased TYPE boolean READ-ONLY.

    CLASS-METHODS class_constructor .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(result) TYPE REF TO zcl_timem_options .

    METHODS set      "#EC OPTL_PARAM
      IMPORTING
        !mode               TYPE ty_mode OPTIONAL
        !ignore_case        TYPE boolean OPTIONAL
        !ignore_indentation TYPE boolean OPTIONAL
        !timestamp          TYPE timestamp OPTIONAL
        !theme              TYPE ztimem_theme OPTIONAL
        !ignore_unreleased  TYPE boolean OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: instance TYPE REF TO zcl_timem_options.
ENDCLASS.



CLASS ZCL_TIMEM_OPTIONS IMPLEMENTATION.


  METHOD class_constructor.
    instance = NEW #( ).
    instance->set(
      mode               = zcl_timem_consts=>mode-blame
      ignore_case        = abap_false
      ignore_indentation = abap_false
      timestamp          = CONV #( |{ sy-datum }235959| )
      theme              = zcl_timem_gui_viewer=>c_theme-light ).
  ENDMETHOD.


  METHOD get_instance.
    result = instance.
  ENDMETHOD.


  METHOD set.
    IF mode IS SUPPLIED.
      me->mode = mode.
    ENDIF.

    IF ignore_case IS SUPPLIED.
      me->ignore_case = ignore_case.
    ENDIF.

    IF ignore_indentation IS SUPPLIED.
      me->ignore_indentation = ignore_indentation.
    ENDIF.

    IF ignore_unreleased IS SUPPLIED.
      me->ignore_unreleased = ignore_unreleased.
    ENDIF.

    IF theme IS SUPPLIED.
      me->theme = theme.
    ENDIF.

    IF timestamp IS SUPPLIED.
      me->timestamp = timestamp.
      CONVERT TIME STAMP timestamp TIME ZONE space INTO DATE me->date TIME me->time.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
