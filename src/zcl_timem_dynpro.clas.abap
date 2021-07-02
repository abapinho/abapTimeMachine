CLASS zcl_timem_dynpro DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA dynnr TYPE sydynnr READ-ONLY .

    METHODS constructor
      IMPORTING
        !dynnr TYPE sydynnr .
    METHODS remove_toolbar .
    METHODS hide_buttons.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TIMEM_DYNPRO IMPLEMENTATION.


  METHOD constructor.
    me->dynnr = dynnr.
  ENDMETHOD.


  METHOD hide_buttons.
    DATA: ucomms TYPE TABLE OF sy-ucomm.

*  PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

    APPEND 'CRET' TO ucomms.  "Button Execute
    APPEND 'GET' TO ucomms.  "Button Save

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = ucomms.

  ENDMETHOD.


  METHOD remove_toolbar.
    " Borrowed from abapGit

    DATA: header               TYPE rpy_dyhead,
          containers           TYPE dycatt_tab,
          fields_to_containers TYPE dyfatc_tab,
          flow_logic           TYPE swydyflow.

    CALL FUNCTION 'RPY_DYNPRO_READ'
      EXPORTING
        progname             = sy-cprog
        dynnr                = me->dynnr
      IMPORTING
        header               = header
      TABLES
        containers           = containers
        fields_to_containers = fields_to_containers
        flow_logic           = flow_logic
      EXCEPTIONS
        cancelled            = 1
        not_found            = 2
        permission_error     = 3
        OTHERS               = 4.
    IF sy-subrc IS NOT INITIAL.
      RETURN. " Ignore errors, just exit
    ENDIF.

    IF header-no_toolbar = abap_true.
      RETURN. " No change required
    ENDIF.

    header-no_toolbar = abap_true.

    CALL FUNCTION 'RPY_DYNPRO_INSERT'
      EXPORTING
        header                 = header
        suppress_exist_checks  = abap_true
      TABLES
        containers             = containers
        fields_to_containers   = fields_to_containers
        flow_logic             = flow_logic
      EXCEPTIONS
        cancelled              = 1
        already_exists         = 2
        program_not_exists     = 3
        not_executed           = 4
        missing_required_field = 5
        illegal_field_value    = 6
        field_not_allowed      = 7
        not_generated          = 8
        illegal_field_position = 9
        OTHERS                 = 10.
    IF sy-subrc <> 2 AND sy-subrc <> 0.
      RETURN. " Ignore errors, just exit
    ENDIF.
  ENDMETHOD.
ENDCLASS.
