*&---------------------------------------------------------------------*
*& Report ZABAPBLAME
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zblame.

DATA: s_tadir TYPE tadir.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-sel.
PARAMETERS: p_object TYPE zblame_object_type AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY DEFAULT 'PROG'.
PARAMETERS: p_name TYPE versobjnam DEFAULT 'ZBLAME_SAMPLE'.
SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN BEGIN OF BLOCK out WITH FRAME TITLE TEXT-out.
PARAMETERS: p_html RADIOBUTTON GROUP out.
PARAMETERS: p_salv RADIOBUTTON GROUP out.
SELECTION-SCREEN END OF BLOCK out.

DATA go_out TYPE REF TO zif_blame_renderable.

START-OF-SELECTION.
  DATA output_type TYPE zcl_blame_gui_factory=>ty_output_type.

  CASE abap_true.
    WHEN p_salv.
      output_type = zcl_blame_gui_factory=>gc_output_type-salv.
    WHEN p_html.
      output_type = zcl_blame_gui_factory=>gc_output_type-html.
  ENDCASE.

  NEW zcl_blame_run( )->go( i_object_type = 'REPS'
                            i_object_name = p_name
                            i_output_type = output_type ).
