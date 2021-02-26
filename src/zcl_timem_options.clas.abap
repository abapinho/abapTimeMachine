class ZCL_TIMEM_OPTIONS definition
  public
  final
  create private .

public section.

  types TY_MODE type CHAR1 .

    "! Mode (Blame or Time Machine)
  data MODE type TY_MODE read-only .
    "! Diff operation should ignore case
  data IGNORE_CASE type BOOLEAN read-only .
    "! Diff operation should ignore indentation
  data IGNORE_INDENTATION type BOOLEAN read-only .
    "! CSS theme name
  data THEME type ZTIMEM_THEME read-only .
    "! Timestamp
  data TIMESTAMP type TIMESTAMP read-only .
    "! Date
  data DATE type DATUM read-only .
    "! Time
  data TIME type UZEIT read-only .

  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_TIMEM_OPTIONS .
  methods SET
    importing
      !I_MODE type TY_MODE optional
      !I_IGNORE_CASE type BOOLEAN optional
      !I_IGNORE_INDENTATION type BOOLEAN optional
      !I_TIMESTAMP type TIMESTAMP optional
      !I_THEME type ZTIMEM_THEME optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: go_instance TYPE REF TO zcl_timem_options.
ENDCLASS.



CLASS ZCL_TIMEM_OPTIONS IMPLEMENTATION.


  METHOD class_constructor.
    go_instance = NEW #( ).
    go_instance->set(
        i_mode               = zif_timem_consts=>mode-blame
        i_ignore_case        = abap_false
        i_ignore_indentation = abap_false
        i_timestamp          = CONV #( |{ sy-datum }235959| )
        i_theme              = zcl_timem_gui_viewer=>c_theme-light ).
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
