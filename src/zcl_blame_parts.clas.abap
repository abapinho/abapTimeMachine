CLASS zcl_blame_parts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !i_object_type TYPE zblame_object_type
        !i_object_name TYPE sobj_name
      RAISING
        zcx_blame .

    METHODS get_data
      IMPORTING
                !io_options    TYPE REF TO zcl_blame_options
      RETURNING VALUE(rs_data) TYPE zblame_parts
      RAISING   zcx_blame.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_type TYPE zblame_object_type.
    DATA g_name TYPE sobj_name.
    DATA gt_part TYPE zblame_part_ref_t.

    METHODS load_parts
      RAISING zcx_blame.

    METHODS get_authors
      IMPORTING
                !it_part         TYPE zblame_part_t
      RETURNING VALUE(rt_author) TYPE zblame_author_info_t.

    METHODS get_requests
      IMPORTING
                !it_part          TYPE zblame_part_t
      RETURNING VALUE(rt_request) TYPE zblame_request_info_t.

    methods get_stats
      IMPORTING
        !it_part type zblame_part_t
        RETURNING VALUE(rs_stats) type zblame_stats.
ENDCLASS.



CLASS zcl_blame_parts IMPLEMENTATION.


  METHOD constructor.
    me->g_type = i_object_type.
    me->g_name = i_object_name.
    load_parts( ).
  ENDMETHOD.


  METHOD get_authors.
    DATA t_blame_author TYPE zblame_line_t.
    DATA(t_blame_all) = VALUE zblame_line_t( FOR s_part IN it_part
                                 FOR s_blame IN s_part-t_blame
                                 ( s_blame ) ).

    LOOP AT t_blame_all REFERENCE INTO DATA(os_blame)
     GROUP BY ( author = os_blame->author name = os_blame->author_name )
              REFERENCE INTO DATA(os_group).

      REFRESH t_blame_author.
      LOOP AT GROUP os_group REFERENCE INTO os_blame WHERE author = os_group->author.
        t_blame_author = VALUE zblame_line_t( BASE t_blame_author ( os_blame->* ) ).
      ENDLOOP.

      DATA(request_count) = REDUCE int2( INIT x = 0
                                         FOR GROUPS request OF s_blame_author IN t_blame_author
                                         GROUP BY s_blame_author-request
                                         NEXT x = x + 1 ).

      rt_author = VALUE #( BASE rt_author
                           ( author = os_group->author
                             name = os_group->name
                             line_count = lines( t_blame_author )
                             blame_percentage = lines( t_blame_author ) / lines( t_blame_all )
                             request_count = request_count ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_data.
    DATA(t_part) = VALUE zblame_part_t( FOR o_part IN gt_part
                                        ( name = o_part->name
                                          type = o_part->vrsd_type
                                          object_name = o_part->vrsd_name
                                          t_blame = o_part->compute_blame( io_options ) ) ).

    rs_data = VALUE #( name = g_name
                       type = g_type
                       abapblame_version = zif_blame_consts=>version
                       t_author = get_authors( t_part )
                       t_request = get_requests( t_part )
                       t_part = t_part
                       s_stats = get_stats( t_part ) ).
  ENDMETHOD.


  METHOD get_requests.
    DATA t_blame_request TYPE zblame_line_t.
    DATA(t_blame_all) = VALUE zblame_line_t( FOR s_part IN it_part
                                 FOR s_blame IN s_part-t_blame
                                 ( s_blame ) ).

    LOOP AT t_blame_all REFERENCE INTO DATA(os_blame)
     GROUP BY ( request = os_blame->request date = os_blame->date time = os_blame->time author = os_blame->author name = os_blame->author_name )
              ASCENDING
              REFERENCE INTO DATA(os_group).

      REFRESH t_blame_request.
      LOOP AT GROUP os_group REFERENCE INTO os_blame WHERE request = os_group->request.
        t_blame_request = VALUE zblame_line_t( BASE t_blame_request ( os_blame->* ) ).
      ENDLOOP.

      DATA(request_count) = REDUCE int2( INIT x = 0
                                         FOR GROUPS request OF s_blame_author IN t_blame_request
                                         GROUP BY s_blame_author-request
                                         NEXT x = x + 1 ).

      rt_request = VALUE #( BASE rt_request
                           ( request = os_group->request
                             date = os_group->date
                             time = os_group->time
                             description = NEW zcl_blame_request( os_group->request )->description
                             author = os_group->author
                             name = os_group->name
                             line_count = lines( t_blame_request )
                             blame_percentage = lines( t_blame_request ) / lines( t_blame_all ) ) ).
    ENDLOOP.
  ENDMETHOD.


  method get_stats.
    DATA(t_blame_all) = VALUE zblame_line_t( FOR s_part IN it_part
                                 FOR s_blame IN s_part-t_blame
                                 ( s_blame ) ).
    rs_stats = new zcl_blame_stats( t_blame_all )->stats.
  ENDMETHOD.


  METHOD load_parts.
    DATA(o_object) = NEW zcl_blame_object_factory( )->get_instance( i_object_type = me->g_type
                                                                    i_object_name = me->g_name ).
    me->gt_part = o_object->get_part_list( ).
  ENDMETHOD.
ENDCLASS.
