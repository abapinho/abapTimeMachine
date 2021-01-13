CLASS zcl_blame_diff DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Takes two source lists - old and new - and returns a new source list
    "! which merges both, adding for each line an indicator of how it changed
    "! between both versions: (I)nsert/(D)elete/(U)pdate.
    "! @parameter it_old | Old source list
    "! @parameter it_new | New source list
    METHODS compute
      IMPORTING
        !it_old         TYPE zblame_line_t
        !it_new         TYPE zblame_line_t
      RETURNING
        VALUE(rt_blame) TYPE zblame_line_t .

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
        !it_old         TYPE zblame_line_t
        !it_new         TYPE zblame_line_t
      RETURNING
        VALUE(rt_delta) TYPE vxabapt255_tab .

    METHODS get_source
      IMPORTING
                !it_blame        TYPE zblame_line_t
      RETURNING VALUE(rt_source) TYPE abaptxt255_tab.

    METHODS process_line
      IMPORTING
                !i_line       TYPE text255
      RETURNING VALUE(r_line) TYPE text255.
ENDCLASS.



CLASS ZCL_BLAME_DIFF IMPLEMENTATION.


  METHOD compute.
    DATA: old_index TYPE i VALUE 1,
          new_index TYPE i VALUE 1,
          s_blame   LIKE LINE OF rt_blame.

    IF it_old IS INITIAL.
      rt_blame = it_new.
      RETURN.
    ENDIF.

    DATA(t_delta) = compute_delta( it_old = it_old it_new = it_new ).

    DO.
      READ TABLE t_delta INTO DATA(s_delta) WITH KEY number = old_index.
      IF sy-subrc = 0.
        DELETE t_delta INDEX sy-tabix.

        CASE s_delta-vrsflag.
          WHEN c_diff-delete.
            old_index = old_index + 1.

          WHEN c_diff-insert.
            READ TABLE it_new INTO s_blame INDEX new_index.
            ASSERT sy-subrc = 0.
            s_blame-source = s_delta-line.
            INSERT s_blame INTO TABLE rt_blame.
            new_index = new_index + 1.

          WHEN c_diff-update.
            READ TABLE it_new INTO s_blame INDEX new_index.
            ASSERT sy-subrc = 0.
            INSERT s_blame INTO TABLE rt_blame.
            new_index = new_index + 1.
            old_index = old_index + 1.

          WHEN OTHERS.
            ASSERT 0 = 1.

        ENDCASE.
      ELSE.
        READ TABLE it_old INTO s_blame INDEX old_index.
        ASSERT sy-subrc = 0.
        INSERT s_blame INTO TABLE rt_blame.
        new_index = new_index + 1.
        old_index = old_index + 1.

      ENDIF.

      IF new_index > lines( it_new ) AND old_index > lines( it_old ).
        EXIT. " current loop
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD compute_delta.
    DATA: t_trdirtab_old TYPE TABLE OF trdir,
          t_trdirtab_new TYPE TABLE OF trdir,
          t_trdir_delta  TYPE TABLE OF xtrdir.

    DATA(t_source_old) = get_source( it_old ).
    DATA(t_source_new) = get_source( it_new ).

    CALL FUNCTION 'SVRS_COMPUTE_DELTA_REPS'
      TABLES
        texttab_old  = t_source_old
        texttab_new  = t_source_new
        trdirtab_old = t_trdirtab_old
        trdirtab_new = t_trdirtab_new
        trdir_delta  = t_trdir_delta
        text_delta   = rt_delta.
  ENDMETHOD.


  METHOD get_source.
    rt_source = VALUE abaptxt255_tab(
      FOR s_blame IN it_blame
      ( line = process_line( s_blame-source ) ) ).
  ENDMETHOD.


  METHOD process_line.
    r_line = i_line.
    IF zcl_blame_options=>get_instance( )->ignore_case = abap_true.
      r_line = to_upper( r_line ).
    ENDIF.
    IF zcl_blame_options=>get_instance( )->ignore_indentation = abap_true.
      SHIFT r_line LEFT DELETING LEADING space.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
