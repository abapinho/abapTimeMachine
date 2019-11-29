CLASS zcl_blame_part DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA object_name TYPE sobj_name READ-ONLY.

    METHODS constructor
      IMPORTING
        !i_object_name TYPE sobj_name.

    METHODS compute_blame
      RETURNING VALUE(rt_blame) TYPE zblame_line_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA go_versions TYPE REF TO zcl_blame_versions.

    METHODS compute_blame_aux
      IMPORTING
                !io_version     TYPE REF TO zcl_blame_version
      RETURNING VALUE(rt_blame) TYPE zblame_line_t.
ENDCLASS.



CLASS ZCL_BLAME_PART IMPLEMENTATION.


  METHOD compute_blame.
    rt_blame = compute_blame_aux( go_versions->get_version( 0 ) ).
  ENDMETHOD.


  METHOD compute_blame_aux.
    IF io_version->version_number = 1.
      rt_blame = io_version->get_source_with_blame( ).
    ELSE.
      DATA(o_previous_version) = go_versions->get_previous_version( io_version->version_number ).
      rt_blame = NEW zcl_blame_diff( )->compute( it_old   = compute_blame_aux( o_previous_version )
                                                 it_new   =  io_version->get_source_with_blame( ) ).
    ENDIF.
  ENDMETHOD.


  METHOD  constructor.
    me->object_name = i_object_name.
    go_versions = NEW #( me ).
  ENDMETHOD.
ENDCLASS.
