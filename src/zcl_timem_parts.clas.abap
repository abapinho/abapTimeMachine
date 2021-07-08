"! List of parts of an object.
class ZCL_TIMEM_PARTS definition
  public
  final
  create public .

public section.

    "! Constructor for an object parts
    "! @parameter i_object_type | Object type
    "! @parameter i_object_name | Object name
  methods CONSTRUCTOR
    importing
      !OBJECT_TYPE type ZTIMEM_OBJECT_TYPE
      !OBJECT_NAME type SOBJ_NAME
    raising
      ZCX_TIMEM .
    "! Returns a deep structure containing all the details for all the parts.
  methods GET_DATA
    returning
      value(RESULT) type ZTIMEM_DATA
    raising
      ZCX_TIMEM .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA object_type TYPE ztimem_object_type .
    DATA object_name TYPE sobj_name .
    DATA parts TYPE zif_timem_object=>ty_t_part_ref .
    DATA userexits TYPE REF TO zcl_timem_userexits.

    "! Load all the data, creating the actual parts
    "! which will load all the versions
    "! @parameter io_counter | To keep track of progress
    METHODS load
      RAISING
        zcx_timem .

    METHODS get_authors
      IMPORTING
        !parts        TYPE zif_timem_object=>ty_t_part_ref
      RETURNING
        VALUE(result) TYPE ztimem_author_info_t
      RAISING   zcx_timem.

    METHODS get_requests
      IMPORTING
                !parts        TYPE zif_timem_object=>ty_t_part_ref
      RETURNING
                VALUE(result) TYPE ztimem_request_info_t
      RAISING   zcx_timem.

    METHODS get_stats
      IMPORTING
        !parts        TYPE zif_timem_object=>ty_t_part_ref
      RETURNING
        VALUE(result) TYPE ztimem_stats
      RAISING   zcx_timem.

    METHODS get_timestamps
      RETURNING
        VALUE(result) TYPE ztimem_timestamp_t .
ENDCLASS.



CLASS ZCL_TIMEM_PARTS IMPLEMENTATION.


  METHOD constructor.
    me->object_type = object_type.
    me->object_name = object_name.
    me->userexits = NEW #( ).
    load( ).
  ENDMETHOD.


  METHOD get_authors.
    DATA authors TYPE ztimem_line_t.

    DATA(lines) = VALUE ztimem_line_t(
      FOR part IN parts
      FOR s_line IN part->get_source( )
      ( s_line ) ).

    LOOP AT lines ASSIGNING FIELD-SYMBOL(<s_line>)
     GROUP BY ( author = <s_line>-author name = <s_line>-author_name )
              ASSIGNING FIELD-SYMBOL(<t_group>).

      REFRESH authors.
      LOOP AT GROUP <t_group> ASSIGNING <s_line> WHERE author = <s_line>-author.
        authors = VALUE #( BASE authors ( <s_line> ) ).
      ENDLOOP.

      DATA(request_count) = REDUCE int2( INIT x = 0
                                         FOR GROUPS request OF author IN authors
                                         GROUP BY author-request
                                         NEXT x = x + 1 ).

      result = VALUE #(
        BASE result
        ( author = <t_group>-author
          name = <t_group>-name
          line_count = lines( authors )
          percentage = lines( authors ) / lines( lines )
          request_count = request_count ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_data.
    DATA(t_part) =
      VALUE ztimem_part_source_t(
        FOR part IN parts
        ( name = part->name
        type = part->vrsd_type
        object_name = part->vrsd_name
        lines = part->get_source( ) ) ).
    DELETE t_part WHERE lines IS INITIAL.

    result = VALUE #( name = object_name
                       type = object_type
                       version = zif_timem_consts=>version
                       authors = get_authors( parts )
                       requests = get_requests( parts )
                       parts = t_part
                       timestamps = get_timestamps( )
                       stats = get_stats( parts )
                       timestamp = zcl_timem_options=>get_instance( )->timestamp
                       ignore_case = zcl_timem_options=>get_instance( )->ignore_case
                       ignore_indentation = zcl_timem_options=>get_instance( )->ignore_indentation ).

    userexits->before_rendering( CHANGING data = result ).
  ENDMETHOD.


  METHOD get_requests.
    DATA requests TYPE ztimem_line_t.
    DATA(lines) = VALUE ztimem_line_t(
      FOR part IN parts
      FOR line IN part->get_source( )
      ( line ) ).

    LOOP AT lines REFERENCE INTO DATA(line_ref)
      GROUP BY ( request = line_ref->request )
        ASCENDING
        ASSIGNING FIELD-SYMBOL(<t_group>).

      REFRESH requests.
      LOOP AT GROUP <t_group> ASSIGNING FIELD-SYMBOL(<s_group>) WHERE request = <t_group>-request.
        requests = VALUE #( BASE requests ( <s_group> ) ).
      ENDLOOP.

      result = VALUE #(
        BASE result
        ( request = <t_group>-request
          description = NEW zcl_timem_request( <t_group>-request )->description
          line_count = lines( requests )
          percentage = lines( requests ) / lines( lines ) ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_stats.
    DATA(lines) = VALUE ztimem_line_t(
      FOR part IN parts
      FOR line IN part->get_source( )
      ( line ) ).
    result = NEW zcl_timem_stats( lines )->stats.
  ENDMETHOD.


  METHOD get_timestamps.
    " Gather timestamps from all parts
    LOOP AT parts INTO DATA(part).
      DATA(t_timestamp_part) = part->get_timestamps( ).
      LOOP AT t_timestamp_part INTO DATA(ts).
        COLLECT ts INTO result.
      ENDLOOP.
    ENDLOOP.
    " We also need the user selected timestamp
    DATA(options_timestamp) = zcl_timem_options=>get_instance( )->timestamp.
    COLLECT options_timestamp INTO result.
    SORT result BY table_line DESCENDING.
  ENDMETHOD.


  METHOD load.
    DATA(object) = NEW zcl_timem_object_factory( )->get_instance( object_type = object_type
                                                                  object_name = object_name ).
    DATA(part_list) = object->get_part_list( ).

    userexits->modify_part_list( CHANGING part_list = part_list ).

    LOOP AT part_list REFERENCE INTO DATA(s_part).
      TRY.
          INSERT NEW #( name = s_part->name
                        vrsd_name = s_part->object_name
                        vrsd_type = s_part->type ) INTO TABLE parts.
        CATCH zcx_timem.
          ASSERT 1 = 1. " Doesn't exist? Carry on
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
