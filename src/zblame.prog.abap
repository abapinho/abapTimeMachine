*&---------------------------------------------------------------------*
*& Report ZABAPBLAME
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zblame.

DATA: s_tadir TYPE tadir.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-sel.
PARAMETERS p_otype TYPE zblame_object_type AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY DEFAULT 'PROG'.
PARAMETERS p_name TYPE sobj_name OBLIGATORY.
SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE TEXT-opt.
PARAMETERS p_icase AS CHECKBOX.
PARAMETERS p_iinde AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK options.

*SELECTION-SCREEN BEGIN OF BLOCK output WITH FRAME TITLE TEXT-out.
*PARAMETERS p_theme TYPE zblame_theme AS LISTBOX VISIBLE LENGTH 15 DEFAULT 'LIGHT'.
*SELECTION-SCREEN END OF BLOCK output.

START-OF-SELECTION.
  TRY.
      DATA(o_options) = NEW zcl_blame_options( i_ignore_case        = p_icase
                                               i_ignore_indentation = p_iinde ).

      NEW zcl_blame_run( )->go( i_object_type = p_otype
                                i_object_name = p_name
                                io_options    = o_options ).
    CATCH zcx_blame INTO DATA(o_exp).
      MESSAGE o_exp TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
