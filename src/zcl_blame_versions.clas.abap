CLASS zcl_blame_versions DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !io_part TYPE REF TO zcl_blame_part.

    METHODS get_version
      IMPORTING
                !i_version_number TYPE versno
      RETURNING VALUE(ro_version) TYPE REF TO zcl_blame_version.

    methods get_previous_version
      IMPORTING
                !i_version_number TYPE versno
      RETURNING VALUE(ro_version) TYPE REF TO zcl_blame_version.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_t_versno TYPE SORTED TABLE OF versno WITH UNIQUE KEY table_line.

    DATA go_part TYPE REF TO zcl_blame_part.
    DATA gt_versno TYPE ty_t_versno.

    METHODS load_numbers
      RETURNING VALUE(rt_versno) TYPE ty_t_versno.

    METHODS get_previous_version_number
      IMPORTING
        !value(i_current) TYPE versno
      RETURNING VALUE(r_previous) TYPE versno.
ENDCLASS.



CLASS zcl_blame_versions IMPLEMENTATION.
  METHOD constructor.
    go_part = io_part.
    load_numbers( ).
  ENDMETHOD.


  METHOD load_numbers.
    SELECT versno INTO TABLE gt_versno
      FROM vrsd
      WHERE objtype = go_part->object_type
        AND objname = go_part->object_name.
  ENDMETHOD.


  METHOD get_previous_version_number.
    IF i_current = 1.
      ASSERT 1 = 0. " This is the oldest version!
    ENDIF.

*   Current version = 0. So we change it to 9999 to get the latest one
    IF i_current = 0.
      i_current = 9999.
    ENDIF.

    DATA(t_versno) = gt_versno.
    DELETE t_versno WHERE table_line >= i_current.
    r_previous = t_versno[ lines( t_versno ) ].
  ENDMETHOD.


  METHOD get_version.
    ro_version = NEW zcl_blame_version( io_part          = go_part
                                        i_version_number = i_version_number ).
  ENDMETHOD.


  method get_previous_version.
    ro_version = get_version( get_previous_version_number( i_version_number ) ).
  ENDMETHOD.
ENDCLASS.
