*&---------------------------------------------------------------------*
*& Report ZABAPBLAME
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zblame.

DATA: s_tadir TYPE tadir.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-sel.
PARAMETERS: p_otype TYPE zblame_object_type AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY DEFAULT 'PROG'.
PARAMETERS: p_name TYPE sobj_name DEFAULT 'ZBLAME_SAMPLE'.
SELECTION-SCREEN END OF BLOCK sel.

START-OF-SELECTION.
  NEW zcl_blame_run( )->go( i_object_type = p_otype
                            i_object_name = p_name ).
