"! List of parts of an object.
CLASS zcl_timem_parts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Constructor for an object parts
    "! @parameter i_object_type | Object type
    "! @parameter i_object_name | Object name
    METHODS constructor
      IMPORTING
        !i_object_type TYPE ztimem_object_type
        !i_object_name TYPE sobj_name
      RAISING
        zcx_timem .

    "! Returns a deep structure containing all the details for all the parts.
    METHODS get_data
      RETURNING
        VALUE(result) TYPE ztimem_parts
      RAISING
        zcx_timem .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA g_type TYPE ztimem_object_type .
    DATA g_name TYPE sobj_name .
    DATA gt_part TYPE zif_timem_object=>ty_t_part_ref .
    DATA userexits TYPE REF TO zcl_timem_userexits.

    "! Load all the data, creating the actual parts
    "! which will load all the versions
    "! @parameter io_counter | To keep track of progress
    METHODS load
      RAISING
        zcx_timem .

    METHODS get_authors
      IMPORTING
        !it_part      TYPE zif_timem_object=>ty_t_part_ref
      RETURNING
        VALUE(result) TYPE ztimem_author_info_t .

    METHODS get_requests
      IMPORTING
        !it_part      TYPE zif_timem_object=>ty_t_part_ref
      RETURNING
        VALUE(result) TYPE ztimem_request_info_t .

    METHODS get_stats
      IMPORTING
        !it_part      TYPE zif_timem_object=>ty_t_part_ref
      RETURNING
        VALUE(result) TYPE ztimem_stats .

    METHODS get_timestamps
      RETURNING
        VALUE(result) TYPE ztimem_timestamp_t .
ENDCLASS.



CLASS ZCL_TIMEM_PARTS IMPLEMENTATION.


  METHOD constructor.
    me->g_type = i_object_type.
    me->g_name = i_object_name.
    me->userexits = NEW #( ).
    load( ).
  ENDMETHOD.


  METHOD get_authors.
    DATA t_author TYPE ztimem_line_t.

    DATA(t_line) = VALUE ztimem_line_t(
      FOR part IN it_part
      FOR s_line IN part->get_source( )
      ( s_line ) ).

    LOOP AT t_line ASSIGNING FIELD-SYMBOL(<s_line>)
     GROUP BY ( author = <s_line>-author name = <s_line>-author_name )
              ASSIGNING FIELD-SYMBOL(<t_group>).

      REFRESH t_author.
      LOOP AT GROUP <t_group> ASSIGNING <s_line> WHERE author = <s_line>-author.
        t_author = VALUE #( BASE t_author ( <s_line> ) ).
      ENDLOOP.

      DATA(request_count) = REDUCE int2( INIT x = 0
                                         FOR GROUPS request OF s_author IN t_author
                                         GROUP BY s_author-request
                                         NEXT x = x + 1 ).

      result = VALUE #(
        BASE result
        ( author = <t_group>-author
          name = <t_group>-name
          line_count = lines( t_author )
          percentage = lines( t_author ) / lines( t_line )
          request_count = request_count ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_data.
    DATA(t_part) =
      VALUE ztimem_part_source_t(
        FOR o_part IN gt_part
        ( name = o_part->name
        type = o_part->vrsd_type
        object_name = o_part->vrsd_name
        t_line = o_part->get_source( ) ) ).
    DELETE t_part WHERE t_line IS INITIAL.

    result = VALUE #( name = g_name
                       type = g_type
                       version = zif_timem_consts=>version
                       t_author = get_authors( gt_part )
                       t_request = get_requests( gt_part )
                       t_part = t_part
                       t_timestamp = get_timestamps( )
                       s_stats = get_stats( gt_part )
                       timestamp = zcl_timem_options=>get_instance( )->timestamp
                       ignore_case = zcl_timem_options=>get_instance( )->ignore_case
                       ignore_indentation = zcl_timem_options=>get_instance( )->ignore_indentation ).

    userexits->before_rendering( CHANGING parts = result ).
  ENDMETHOD.


  METHOD get_requests.
    DATA t_request TYPE ztimem_line_t.
    DATA(t_line) = VALUE ztimem_line_t(
      FOR part IN it_part
      FOR s_line IN part->get_source( )
      ( s_line ) ).

    LOOP AT t_line REFERENCE INTO DATA(os_line)
      WHERE request IS NOT INITIAL
      GROUP BY ( request = os_line->request )
        ASCENDING
        ASSIGNING FIELD-SYMBOL(<t_group>).

      REFRESH t_request.
      LOOP AT GROUP <t_group> ASSIGNING FIELD-SYMBOL(<s_group>) WHERE request = <t_group>-request.
        t_request = VALUE #( BASE t_request ( <s_group> ) ).
      ENDLOOP.

      DATA(request_count) = REDUCE int2( INIT x = 0
                                         FOR GROUPS request OF s_author IN t_request
                                         GROUP BY s_author-request
                                         NEXT x = x + 1 ).

      result = VALUE #(
        BASE result
        ( request = <t_group>-request
          description = NEW zcl_timem_request( <t_group>-request )->description
          line_count = lines( t_request )
          percentage = lines( t_request ) / lines( t_line ) ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_stats.
    DATA(t_line) = VALUE ztimem_line_t(
      FOR part IN it_part
      FOR s_line IN part->get_source( )
      ( s_line ) ).
    result = NEW zcl_timem_stats( t_line )->stats.
  ENDMETHOD.


  METHOD get_timestamps.
    " Gather timestamps from all parts
    LOOP AT gt_part INTO DATA(o_part).
      DATA(t_timestamp_part) = o_part->get_timestamps( ).
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
    DATA(object) = NEW zcl_timem_object_factory( )->get_instance( i_object_type = me->g_type
                                                                  i_object_name = me->g_name ).
    DATA(part_list) = object->get_part_list( ).

    userexits->modify_part_list( CHANGING part_list = part_list ).

    LOOP AT part_list REFERENCE INTO DATA(s_part).
      TRY.
          INSERT NEW #( name = s_part->name
                        vrsd_name = s_part->object_name
                        vrsd_type = s_part->type ) INTO TABLE me->gt_part.
        CATCH zcx_timem.
          ASSERT 1 = 1. " Doesn't exist? Carry on
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
