"! Represents a part of an object, including all the versions of that part that
"! exist in the system.
CLASS zcl_blame_part DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! Object name
    DATA name TYPE string READ-ONLY.

    "! Part type
    DATA vrsd_type TYPE versobjtyp READ-ONLY.

    "! Part name
    DATA vrsd_name TYPE versobjnam READ-ONLY.

    "! Constructs a new part
    "! @parameter i_name | Object name
    "! @parameter i_vrsd_type | Part type
    "! @parameter i_vrsd_name | Part object
    METHODS constructor
      IMPORTING
                !i_name      TYPE string
                !i_vrsd_type TYPE versobjtyp
                !i_vrsd_name TYPE versobjnam
      RAISING   zcx_blame.

    "! Calculates and returns a list of the source already filled with blame
    "! details.
    "! @parameter io_options | Instance of options
    METHODS compute_blame
      IMPORTING
                !io_options     TYPE REF TO zcl_blame_options
      RETURNING VALUE(rt_blame) TYPE zblame_line_t
      RAISING   zcx_blame.

    "! Returns list of authors involved in the different existing versions.
    METHODS get_authors
      RETURNING VALUE(rt_author) TYPE zblame_author_info_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_t_versno TYPE SORTED TABLE OF versno WITH UNIQUE KEY table_line.
    TYPES ty_t_version TYPE STANDARD TABLE OF REF TO zcl_blame_version WITH KEY table_line.

    DATA gt_version TYPE ty_t_version.
    data go_vrsd type ref to zcl_blame_vrsd.

    METHODS load_versions
      RAISING   zcx_blame.

    METHODS get_version
      IMPORTING
                !i_version_number TYPE versno
      RETURNING VALUE(ro_version) TYPE REF TO zcl_blame_version
      RAISING   zcx_blame.
ENDCLASS.



CLASS zcl_blame_part IMPLEMENTATION.


  METHOD compute_blame.
    DATA(o_diff) = NEW zcl_blame_diff( io_options ).
    LOOP AT gt_version INTO DATA(o_version).
      rt_blame = o_diff->compute( it_old = rt_blame
                                  it_new =  o_version->get_source_with_blame( ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD  constructor.
    me->name = i_name.
    me->vrsd_type = i_vrsd_type.
    me->vrsd_name = i_vrsd_name.
    me->go_vrsd = new #( i_type = i_vrsd_type
                         i_name = i_vrsd_name ).
    load_versions( ).
  ENDMETHOD.


  METHOD get_authors.
    rt_author = VALUE #( FOR o_version IN gt_version
                         ( author = o_version->author name = o_version->author_name ) ).
  ENDMETHOD.


  METHOD get_version.
    ro_version = NEW zcl_blame_version( go_vrsd->t_vrsd[ versno = i_version_number ] ).
  ENDMETHOD.


  METHOD load_versions.
    me->gt_version = VALUE #( FOR s_vrsd IN go_vrsd->t_vrsd
                              ( get_version( s_vrsd-versno ) ) ).
  ENDMETHOD.
ENDCLASS.
