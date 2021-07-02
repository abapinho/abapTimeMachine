"! Takes an object type and name, calculates the blame information for the source
"! code of all its parts and displays it as HTML.
REPORT ztimemachine.

TABLES vrsd.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-sel.
PARAMETERS p_otype TYPE ztimem_object_type AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY DEFAULT 'PROG'.
PARAMETERS p_name TYPE sobj_name OBLIGATORY.
SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN BEGIN OF BLOCK filters WITH FRAME TITLE TEXT-flt.
PARAMETERS p_date TYPE datum OBLIGATORY DEFAULT sy-datum.
PARAMETERS p_time TYPE uzeit OBLIGATORY DEFAULT '235959'.
SELECTION-SCREEN END OF BLOCK filters.

SELECTION-SCREEN BEGIN OF BLOCK mode WITH FRAME TITLE TEXT-mde.
PARAMETERS p_mtimem RADIOBUTTON GROUP mode USER-COMMAND mode.
PARAMETERS p_mblame RADIOBUTTON GROUP mode.
SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE TEXT-opt.
PARAMETERS p_icase AS CHECKBOX MODIF ID bla.
PARAMETERS p_iinde AS CHECKBOX MODIF ID bla.
PARAMETERS p_iunre AS CHECKBOX MODIF ID bla.
SELECTION-SCREEN END OF BLOCK options.
SELECTION-SCREEN END OF BLOCK mode.

SELECTION-SCREEN COMMENT /1(83) link.

SELECTION-SCREEN BEGIN OF SCREEN 1001.
* dummy for triggering screen on Java SAP GUI
SELECTION-SCREEN END OF SCREEN 1001.

*SELECTION-SCREEN BEGIN OF BLOCK output WITH FRAME TITLE TEXT-out.
*PARAMETERS p_theme TYPE zblame_theme AS LISTBOX VISIBLE LENGTH 15 DEFAULT 'LIGHT'.
*SELECTION-SCREEN END OF BLOCK output.

INITIALIZATION.
  IF sy-tcode CS 'BLAME'.
    p_mtimem = abap_false.
    p_mblame = abap_true.
  ENDIF.
  link = 'More details at https://github.com/abapinho/abapTimeMachine'.
  NEW zcl_timem_dynpro( '1001' )->remove_toolbar( ).

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'BLA' AND p_mblame IS INITIAL.
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  NEW zcl_timem_dynpro( '1001' )->hide_buttons( ).

START-OF-SELECTION.
  " Convert radio button to mode
  DATA(mode) = SWITCH zcl_timem_options=>ty_mode(
    p_mblame
    WHEN abap_true THEN zif_timem_consts=>mode-blame
    ELSE zif_timem_consts=>mode-time_machine ).

  TRY.
      zcl_timem_options=>get_instance( )->set( i_mode               = mode
                                               i_ignore_case        = p_icase
                                               i_ignore_indentation = p_iinde
                                               i_timestamp = CONV #( |{ p_date }{ p_time }| )
                                               i_ignore_unreleased  = p_iunre ).

      NEW zcl_timem_run( )->go( i_object_type = p_otype
                                i_object_name = p_name ).

      CALL SELECTION-SCREEN 1001.
      LEAVE SCREEN.

    CATCH zcx_timem INTO DATA(o_exp).
      MESSAGE o_exp TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
