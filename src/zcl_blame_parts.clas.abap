"! List of parts of an object.
CLASS zcl_blame_parts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Constructor for an object parts
    "! @parameter i_object_type | Object type
    "! @parameter i_object_name | Object name
    METHODS constructor
      IMPORTING
                !i_object_type TYPE zblame_object_type
                !i_object_name TYPE sobj_name
      RAISING   zcx_blame.

    "! Returns a deep structure containing all the details for all the parts.
    METHODS get_data
      RETURNING
        VALUE(rs_data) TYPE zblame_parts
      RAISING
        zcx_blame .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_type TYPE zblame_object_type .
    DATA g_name TYPE sobj_name .
    DATA gt_part TYPE zif_blame_object=>ty_t_part_ref .

    "! Load all the data, creating the actual parts
    "! which will load all the versions
    "! @parameter io_counter | To keep track of progress
    METHODS load
      RAISING
        zcx_blame .

    METHODS get_authors
      IMPORTING
        !it_part         TYPE zblame_part_t
      RETURNING
        VALUE(rt_author) TYPE zblame_author_info_t .

    METHODS get_requests
      IMPORTING
        !it_part          TYPE zblame_part_t
      RETURNING
        VALUE(rt_request) TYPE zblame_request_info_t .

    METHODS get_stats
      IMPORTING
        !it_part        TYPE zblame_part_t
      RETURNING
        VALUE(rs_stats) TYPE zblame_stats .

    METHODS get_timestamps
      RETURNING VALUE(rt_timestamp) TYPE zblame_timestamp_t.
ENDCLASS.



CLASS zcl_blame_parts IMPLEMENTATION.


  METHOD constructor.
    me->g_type = i_object_type.
    me->g_name = i_object_name.
    load( ).
  ENDMETHOD.


  METHOD get_authors.
    DATA t_author TYPE zblame_line_t.
    DATA(t_line) = VALUE zblame_line_t( FOR s_part IN it_part
                                 FOR s_line IN s_part-t_line
                                 ( s_line ) ).

    LOOP AT t_line ASSIGNING FIELD-SYMBOL(<s_line>)
     GROUP BY ( author = <s_line>-author name = <s_line>-author_name )
              ASSIGNING FIELD-SYMBOL(<t_group>).

      REFRESH t_author.
      LOOP AT GROUP <t_group> ASSIGNING <s_line> WHERE author = <s_line>-author.
        t_author = VALUE zblame_line_t( BASE t_author ( <s_line> ) ).
      ENDLOOP.

      DATA(request_count) = REDUCE int2( INIT x = 0
                                         FOR GROUPS request OF s_author IN t_author
                                         GROUP BY s_author-request
                                         NEXT x = x + 1 ).

      rt_author = VALUE #( BASE rt_author
                           ( author = <t_group>-author
                             name = <t_group>-name
                             line_count = lines( t_author )
                             blame_percentage = lines( t_author ) / lines( t_line )
                             request_count = request_count ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_data.
    DATA(t_part) =
      VALUE zblame_part_t( FOR o_part IN gt_part
                           ( name = o_part->name
                             type = o_part->vrsd_type
                             object_name = o_part->vrsd_name
                             t_line = o_part->get_source( ) ) ).
    DELETE t_part WHERE t_line IS INITIAL.

    rs_data = VALUE #( name = g_name
                       type = g_type
                       abapblame_version = zif_blame_consts=>version
                       t_author = get_authors( t_part )
                       t_request = get_requests( t_part )
                       t_part = t_part
                       t_timestamp = get_timestamps( )
                       s_stats = get_stats( t_part )
                       timestamp = zcl_blame_options=>get_instance( )->timestamp
                       ignore_case = zcl_blame_options=>get_instance( )->ignore_case
                       ignore_indentation = zcl_blame_options=>get_instance( )->ignore_indentation ).
  ENDMETHOD.


  METHOD get_requests.
    DATA t_request TYPE zblame_line_t.
    DATA(t_line) = VALUE zblame_line_t( FOR s_part IN it_part
                                 FOR s_line IN s_part-t_line
                                 ( s_line ) ).

    LOOP AT t_line REFERENCE INTO DATA(os_blame)
      WHERE request IS NOT INITIAL
      GROUP BY ( request = os_blame->request )
        ASCENDING
        ASSIGNING FIELD-SYMBOL(<t_group>).

      REFRESH t_request.
      LOOP AT GROUP <t_group> ASSIGNING FIELD-SYMBOL(<s_group>) WHERE request = <t_group>-request.
        t_request = VALUE zblame_line_t( BASE t_request ( <s_group> ) ).
      ENDLOOP.

      DATA(request_count) = REDUCE int2( INIT x = 0
                                         FOR GROUPS request OF s_author IN t_request
                                         GROUP BY s_author-request
                                         NEXT x = x + 1 ).

      rt_request = VALUE #( BASE rt_request
                           ( request = <t_group>-request
                             description = NEW zcl_blame_request( <t_group>-request )->description
                             line_count = lines( t_request )
                             blame_percentage = lines( t_request ) / lines( t_line ) ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_stats.
    DATA(t_line) = VALUE zblame_line_t( FOR s_part IN it_part
                                 FOR s_line IN s_part-t_line
                                 ( s_line ) ).
    rs_stats = NEW zcl_blame_stats( t_line )->stats.
  ENDMETHOD.


  METHOD get_timestamps.
    " Gather timestamps from all parts
    LOOP AT gt_part INTO DATA(o_part).
      DATA(t_timestamp_part) = o_part->get_timestamps( ).
      LOOP AT t_timestamp_part INTO DATA(ts).
        COLLECT ts INTO rt_timestamp.
      ENDLOOP.
    ENDLOOP.
    " We also need the user selected timestamp
    DATA(options_timestamp) = zcl_blame_options=>get_instance( )->timestamp.
    COLLECT options_timestamp INTO rt_timestamp.
    SORT rt_timestamp BY table_line.
  ENDMETHOD.


  METHOD load.
    DATA(o_object) = NEW zcl_blame_object_factory( )->get_instance( i_object_type = me->g_type
                                                                    i_object_name = me->g_name ).
    me->gt_part = o_object->get_part_list( ).
  ENDMETHOD.
ENDCLASS.
