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
    METHODS compute_blame
      RETURNING VALUE(rt_blame)   TYPE zblame_line_t
      RAISING   zcx_blame.

    "! Returns list of authors involved in the different existing versions.
    METHODS get_authors
      RETURNING VALUE(rt_author) TYPE zblame_author_info_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_t_version TYPE STANDARD TABLE OF REF TO zcl_blame_version WITH KEY table_line.

    DATA gt_version TYPE ty_t_version.

    METHODS load_versions
      IMPORTING
                !i_vrsd_type TYPE versobjtyp
                !i_vrsd_name TYPE versobjnam
      RAISING   zcx_blame.

    METHODS get_versions_until_threshold
      RETURNING VALUE(rt_version) TYPE ty_t_version.
ENDCLASS.



CLASS ZCL_BLAME_PART IMPLEMENTATION.


  METHOD compute_blame.
    DATA(o_diff) = NEW zcl_blame_diff( ).
    LOOP AT get_versions_until_threshold( ) INTO DATA(o_version).
      rt_blame = o_diff->compute( it_old = rt_blame
                                  it_new =  o_version->get_source_with_blame( ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD  constructor.
    me->name = i_name.
    me->vrsd_type = i_vrsd_type.
    me->vrsd_name = i_vrsd_name.
    load_versions( i_vrsd_type = i_vrsd_type
                   i_vrsd_name = i_vrsd_name ).
  ENDMETHOD.


  METHOD get_authors.
    rt_author = VALUE #( FOR o_version IN get_versions_until_threshold( )
                         ( author = o_version->author name = o_version->author_name ) ).
  ENDMETHOD.


  METHOD get_versions_until_threshold.
    rt_version = VALUE #(
      FOR o_version IN gt_version
        WHERE (
          table_line->date < zcl_blame_options=>get_instance( )->date OR
          ( table_line->date = zcl_blame_options=>get_instance( )->date AND
            table_line->time <= zcl_blame_options=>get_instance( )->time ) )
        ( o_version ) ).
  ENDMETHOD.


  METHOD load_versions.
    DATA(o_vrsd) = NEW zcl_blame_vrsd( i_type = i_vrsd_type
                                       i_name = i_vrsd_name ).

    me->gt_version = VALUE #( FOR s_vrsd IN o_vrsd->t_vrsd
                              ( NEW zcl_blame_version( s_vrsd ) ) ).
  ENDMETHOD.
ENDCLASS.
