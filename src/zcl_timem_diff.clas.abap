CLASS zcl_timem_diff DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Takes two source lists - old and new - and returns a new source list
    "! which merges both, adding for each line an indicator of how it changed
    "! between both versions: (I)nsert/(D)elete/(U)pdate.
    "! @parameter lines_old | Old source list
    "! @parameter lines_new | New source list
    METHODS compute
      IMPORTING
        !lines_old       TYPE ztimem_line_t
        !lines_new       TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE ztimem_line_t .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_diff,
        insert TYPE c LENGTH 1 VALUE 'I',
        delete TYPE c LENGTH 1 VALUE 'D',
        update TYPE c LENGTH 1 VALUE 'U',
      END OF c_diff .

    METHODS compute_delta
      IMPORTING
        !lines_old       TYPE ztimem_line_t
        !lines_new       TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE vxabapt255_tab .

    METHODS get_source
      IMPORTING
                !it_line      TYPE ztimem_line_t
      RETURNING VALUE(result) TYPE abaptxt255_tab.

    METHODS process_line
      IMPORTING
                !i_line       TYPE text1000
      RETURNING VALUE(result) TYPE text1000.
ENDCLASS.



CLASS ZCL_TIMEM_DIFF IMPLEMENTATION.


  METHOD compute.
    DATA old_index TYPE i VALUE 1.
    DATA new_index TYPE i VALUE 1.
    DATA s_line    LIKE LINE OF result.

    IF lines_old IS INITIAL.
      result = lines_new.
      RETURN.
    ENDIF.

    DATA(t_delta) = compute_delta( lines_old = lines_old lines_new = lines_new ).

    DO.
      READ TABLE t_delta INTO DATA(s_delta) WITH KEY number = old_index.
      IF sy-subrc = 0.
        DELETE t_delta INDEX sy-tabix.

        CASE s_delta-vrsflag.
          WHEN c_diff-delete.
            old_index = old_index + 1.

          WHEN c_diff-insert.
            READ TABLE lines_new INTO s_line INDEX new_index.
            ASSERT sy-subrc = 0.
            s_line-source = s_delta-line.
            INSERT s_line INTO TABLE result.
            new_index = new_index + 1.

          WHEN c_diff-update.
            READ TABLE lines_new INTO s_line INDEX new_index.
            ASSERT sy-subrc = 0.
            INSERT s_line INTO TABLE result.
            new_index = new_index + 1.
            old_index = old_index + 1.

          WHEN OTHERS.
            ASSERT 0 = 1.

        ENDCASE.
      ELSE.
        READ TABLE lines_old INTO s_line INDEX old_index.
        ASSERT sy-subrc = 0.
        INSERT s_line INTO TABLE result.
        new_index = new_index + 1.
        old_index = old_index + 1.

      ENDIF.

      IF new_index > lines( lines_new ) AND old_index > lines( lines_old ).
        " Current loop
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD compute_delta.
    DATA t_trdirtab_old TYPE TABLE OF trdir.
    DATA t_trdirtab_new TYPE TABLE OF trdir.
    DATA t_trdir_delta  TYPE TABLE OF xtrdir.

    DATA(t_source_old) = get_source( lines_old ).
    DATA(t_source_new) = get_source( lines_new ).

    CALL FUNCTION 'SVRS_COMPUTE_DELTA_REPS'
      TABLES
        texttab_old  = t_source_old
        texttab_new  = t_source_new
        trdirtab_old = t_trdirtab_old
        trdirtab_new = t_trdirtab_new
        trdir_delta  = t_trdir_delta
        text_delta   = result.
  ENDMETHOD.


  METHOD get_source.
    result = VALUE abaptxt255_tab(
      FOR s_line IN it_line
      ( line = process_line( s_line-source ) ) ).
  ENDMETHOD.


  METHOD process_line.
    result = i_line.
    IF zcl_timem_options=>get_instance( )->ignore_case = abap_true.
      result = to_upper( result ).
    ENDIF.
    IF zcl_timem_options=>get_instance( )->ignore_indentation = abap_true.
      SHIFT result LEFT DELETING LEADING space.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
