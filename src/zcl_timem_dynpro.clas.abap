CLASS zcl_timem_dynpro DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA dynnr TYPE sydynnr READ-ONLY .

    METHODS constructor
      IMPORTING
        !i_dynnr TYPE sydynnr .
    METHODS remove_toolbar .
    METHODS hide_buttons.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TIMEM_DYNPRO IMPLEMENTATION.


  METHOD constructor.
  ENDMETHOD.


  METHOD hide_buttons.
    DATA: lt_ucomm TYPE TABLE OF sy-ucomm.

*  PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

    APPEND 'CRET' TO lt_ucomm.  "Button Execute
    APPEND 'GET' TO lt_ucomm.  "Button Save

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = lt_ucomm.

  ENDMETHOD.


  METHOD remove_toolbar.
    " Borrowed from abapGit

    DATA: s_header               TYPE rpy_dyhead,
          t_containers           TYPE dycatt_tab,
          t_fields_to_containers TYPE dyfatc_tab,
          t_flow_logic           TYPE swydyflow.

    CALL FUNCTION 'RPY_DYNPRO_READ'
      EXPORTING
        progname             = sy-cprog
        dynnr                = me->dynnr
      IMPORTING
        header               = s_header
      TABLES
        containers           = t_containers
        fields_to_containers = t_fields_to_containers
        flow_logic           = t_flow_logic
      EXCEPTIONS
        cancelled            = 1
        not_found            = 2
        permission_error     = 3
        OTHERS               = 4.
    IF sy-subrc IS NOT INITIAL.
      RETURN. " Ignore errors, just exit
    ENDIF.

    IF s_header-no_toolbar = abap_true.
      RETURN. " No change required
    ENDIF.

    s_header-no_toolbar = abap_true.

    CALL FUNCTION 'RPY_DYNPRO_INSERT'
      EXPORTING
        header                 = s_header
        suppress_exist_checks  = abap_true
      TABLES
        containers             = t_containers
        fields_to_containers   = t_fields_to_containers
        flow_logic             = t_flow_logic
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
