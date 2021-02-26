"! Represents a part of an object, including all the versions of that part that
"! exist in the system.
class ZCL_TIMEM_PART definition
  public
  final
  create public .

public section.

    "! Object name
  data NAME type STRING read-only .
    "! Part type
  data VRSD_TYPE type VERSOBJTYP read-only .
    "! Part name
  data VRSD_NAME type VERSOBJNAM read-only .

    "! Constructs a new part
    "! @parameter i_name | Object name
    "! @parameter i_vrsd_type | Part type
    "! @parameter i_vrsd_name | Part object
  methods CONSTRUCTOR
    importing
      !I_NAME type STRING
      !I_VRSD_TYPE type VERSOBJTYP
      !I_VRSD_NAME type VERSOBJNAM
    raising
      ZCX_TIMEM .
    "! Returns list of authors involved in the different existing versions.
  methods GET_AUTHORS
    returning
      value(RT_AUTHOR) type ZTIMEM_AUTHOR_INFO_T .
  methods GET_TIMESTAMPS
    returning
      value(RT_TIMESTAMP) type ZTIMEM_TIMESTAMP_T .
  methods GET_SOURCE
    returning
      value(RT_LINE) type ZTIMEM_LINE_T
    raising
      ZCX_TIMEM .
  PROTECTED SECTION.
private section.

  types:
    ty_t_version TYPE STANDARD TABLE OF REF TO zcl_timem_version WITH KEY table_line .

  data GT_VERSION type TY_T_VERSION .

  methods LOAD_VERSIONS
    importing
      !I_VRSD_TYPE type VERSOBJTYP
      !I_VRSD_NAME type VERSOBJNAM
    raising
      ZCX_TIMEM .
  methods GET_VERSION_AT_THRESHOLD
    returning
      value(RO_VERSION) type ref to ZCL_TIMEM_VERSION .
  methods GET_VERSIONS_UNTIL_THRESHOLD
    returning
      value(RT_VERSION) type TY_T_VERSION .
    "! Calculates and returns a list of the diffed source already filled with blame
    "! details.
  methods GET_DIFFED_SOURCE_WITH_BLAME
    returning
      value(RT_LINE) type ZTIMEM_LINE_T
    raising
      ZCX_TIMEM .
  methods GET_SOURCE_AT_THRESHOLD
    returning
      value(RT_LINE) type ZTIMEM_LINE_T
    raising
      ZCX_TIMEM .
ENDCLASS.



CLASS ZCL_TIMEM_PART IMPLEMENTATION.


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
    DATA(o_diff) = NEW zcl_timem_diff( ).
    LOOP AT get_versions_until_threshold( ) INTO DATA(o_version).
      rt_line = o_diff->compute( it_old = rt_line
                                  it_new =  o_version->get_source( ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_source.
    rt_line = SWITCH #(
      zcl_timem_options=>get_instance( )->mode
      WHEN zif_timem_consts=>mode-blame THEN get_diffed_source_with_blame( )
      WHEN zif_timem_consts=>mode-time_machine THEN get_source_at_threshold( ) ).
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
          table_line->date < zcl_timem_options=>get_instance( )->date OR
          ( table_line->date = zcl_timem_options=>get_instance( )->date AND
            table_line->time <= zcl_timem_options=>get_instance( )->time ) )
        ( o_version ) ).
  ENDMETHOD.


  METHOD get_version_at_threshold.
    " The last one should be the one we want
    LOOP AT gt_version INTO ro_version WHERE
          table_line->date < zcl_timem_options=>get_instance( )->date OR
          ( table_line->date = zcl_timem_options=>get_instance( )->date AND
            table_line->time <= zcl_timem_options=>get_instance( )->time ).
    ENDLOOP.
  ENDMETHOD.


  METHOD load_versions.
    DATA(o_vrsd) = NEW zcl_timem_vrsd( i_type = i_vrsd_type
                                       i_name = i_vrsd_name ).

    me->gt_version = VALUE #( FOR s_vrsd IN o_vrsd->t_vrsd
                              ( NEW zcl_timem_version( s_vrsd ) ) ).
  ENDMETHOD.
ENDCLASS.
