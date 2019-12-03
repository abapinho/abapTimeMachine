CLASS zcl_blame_part DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA name TYPE string READ-ONLY.
    DATA vrsd_type TYPE versobjtyp READ-ONLY.
    DATA vrsd_name TYPE versobjnam READ-ONLY.
    DATA t_blame TYPE zblame_line_t READ-ONLY.

    METHODS constructor
      IMPORTING
                !i_name      TYPE string
                !i_vrsd_type TYPE versobjtyp
                !i_vrsd_name TYPE versobjnam
      RAISING   zcx_blame.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_t_versno TYPE SORTED TABLE OF versno WITH UNIQUE KEY table_line.
    TYPES ty_t_version TYPE STANDARD TABLE OF REF TO zcl_blame_version WITH KEY table_line.

    METHODS get_numbers
      RETURNING VALUE(rt_versno) TYPE ty_t_versno.

    METHODS get_versions
      RETURNING VALUE(rt_version) TYPE ty_t_version
      RAISING   zcx_blame.

    METHODS get_version
      IMPORTING
                !i_version_number TYPE versno
      RETURNING VALUE(ro_version) TYPE REF TO zcl_blame_version
      RAISING   zcx_blame.

    METHODS compute_blame
      RETURNING VALUE(rt_blame) TYPE zblame_line_t
      RAISING   zcx_blame.
ENDCLASS.



CLASS zcl_blame_part IMPLEMENTATION.
  METHOD  constructor.
    me->name = i_name.
    me->vrsd_type = i_vrsd_type.
    me->vrsd_name = i_vrsd_name.
    me->t_blame = compute_blame( ).
  ENDMETHOD.


  METHOD compute_blame.
    DATA(t_version) = get_versions( ).
    DATA(o_diff) = NEW zcl_blame_diff( ).
    LOOP AT t_version INTO DATA(o_version).
      rt_blame = o_diff->compute( it_old = rt_blame
                                  it_new =  o_version->get_source_with_blame( ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_numbers.
    DATA versno0 LIKE LINE OF rt_versno.
    SELECT versno INTO TABLE rt_versno
      FROM vrsd
      WHERE objtype = me->vrsd_type
        AND objname = me->vrsd_name.

*   We consider the current version to be 99999 instead of 0
    versno0 = 0.
    DELETE rt_versno WHERE table_line = versno0.
    IF sy-subrc = 0.
      INSERT zcl_blame_version=>c_latest_version INTO TABLE rt_versno.
    ENDIF.
  ENDMETHOD.


  METHOD get_versions.
    rt_version = VALUE #( FOR versno IN get_numbers( )
                          ( get_version( versno ) ) ).
  ENDMETHOD.


  METHOD get_version.
    ro_version = NEW zcl_blame_version( io_part          = me
                                        i_version_number = i_version_number ).
  ENDMETHOD.
ENDCLASS.
