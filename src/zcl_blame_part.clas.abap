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

    "! Returns list of authors involved in the different existing versions.
    METHODS get_authors
      RETURNING VALUE(rt_author) TYPE zblame_author_info_t.

    METHODS get_timestamps
      RETURNING VALUE(rt_timestamp) TYPE zblame_timestamp_t.

    METHODS get_source
      RETURNING VALUE(rt_line) TYPE zblame_line_t
      RAISING   zcx_blame.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_t_version TYPE STANDARD TABLE OF REF TO zcl_blame_version WITH KEY table_line.

    DATA gt_version TYPE ty_t_version.

    METHODS load_versions
      IMPORTING
                !i_vrsd_type TYPE versobjtyp
                !i_vrsd_name TYPE versobjnam
      RAISING   zcx_blame.

    METHODS get_version_at_threshold
      RETURNING VALUE(ro_version) TYPE REF TO zcl_blame_version.

    METHODS get_versions_until_threshold
      RETURNING VALUE(rt_version) TYPE ty_t_version.

    "! Calculates and returns a list of the diffed source already filled with blame
    "! details.
    METHODS get_diffed_source_with_blame
      RETURNING VALUE(rt_line) TYPE zblame_line_t
      RAISING   zcx_blame.

    METHODS get_source_at_threshold
      RETURNING VALUE(rt_line) TYPE zblame_line_t
      RAISING   zcx_blame.
ENDCLASS.



CLASS zcl_blame_part IMPLEMENTATION.


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


  METHOD get_diffed_source_with_blame.
    DATA(o_diff) = NEW zcl_blame_diff( ).
    LOOP AT get_versions_until_threshold( ) INTO DATA(o_version).
      rt_line = o_diff->compute( it_old = rt_line
                                  it_new =  o_version->get_source( ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_source_at_threshold.
    DATA(o_version) = get_version_at_threshold( ).
    IF o_version IS BOUND.
      rt_line = o_version->get_source( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_timestamps.
    DATA ts LIKE LINE OF rt_timestamp.
    LOOP AT gt_version INTO DATA(o_version).
      ts = |{ o_version->date }{ o_version->time }|.
      COLLECT ts INTO rt_timestamp.
    ENDLOOP.
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


  METHOD get_version_at_threshold.
    " The last one should be the one we want
    LOOP AT gt_version INTO ro_version WHERE
          table_line->date < zcl_blame_options=>get_instance( )->date OR
          ( table_line->date = zcl_blame_options=>get_instance( )->date AND
            table_line->time <= zcl_blame_options=>get_instance( )->time ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_source.
    rt_line = SWITCH #(
      zcl_blame_options=>get_instance( )->mode
      WHEN zif_blame_consts=>mode-blame THEN get_diffed_source_with_blame( )
      WHEN zif_blame_consts=>mode-time_machine THEN get_source_at_threshold( ) ).
  ENDMETHOD.


  METHOD load_versions.
    DATA(o_vrsd) = NEW zcl_blame_vrsd( i_type = i_vrsd_type
                                       i_name = i_vrsd_name ).

    me->gt_version = VALUE #( FOR s_vrsd IN o_vrsd->t_vrsd
                              ( NEW zcl_blame_version( s_vrsd ) ) ).
  ENDMETHOD.
ENDCLASS.
