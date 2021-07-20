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
        !object_type TYPE ztimem_object_type
        !object_name TYPE sobj_name
      RAISING
        zcx_timem .

    "! Returns a deep structure containing all the details for all the parts.
    METHODS get_data
      RETURNING
        VALUE(result) TYPE ztimem_data
      RAISING
        zcx_timem .

    METHODS revert
      IMPORTING
        ts TYPE timestamp.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA object_type TYPE ztimem_object_type .
    DATA object_name TYPE sobj_name .
    DATA parts TYPE zif_timem_object=>ty_t_part_ref .
    DATA userexits TYPE REF TO zcl_timem_userexits.
    DATA options TYPE REF TO zcl_timem_options.

    "! Load all the data, creating the actual parts
    "! which will load all the versions
    "! @parameter io_counter | To keep track of progress
    METHODS load
      RAISING
        zcx_timem .

    METHODS get_stats
      IMPORTING
                !parts        TYPE ztimem_part_source_t
      RETURNING
                VALUE(result) TYPE ztimem_stats
      RAISING   zcx_timem.

    METHODS get_timestamps
      RETURNING
        VALUE(result) TYPE ztimem_timestamp_t .

    METHODS get_lines
      IMPORTING
        !parts        TYPE ztimem_part_source_t
      RETURNING
        VALUE(result) TYPE ztimem_line_t.
ENDCLASS.



CLASS ZCL_TIMEM_PARTS IMPLEMENTATION.


  METHOD constructor.
    me->object_type = object_type.
    me->object_name = object_name.
    me->userexits = NEW #( ).
    me->options = zcl_timem_options=>get_instance( ).
    load( ).
  ENDMETHOD.


  METHOD get_data.
    DATA custom_fields TYPE ztimem_data_custom_fields.

    DATA(t_part) =
      VALUE ztimem_part_source_t(
        FOR part IN parts
        ( name = part->name
        type = part->vrsd_type
        object_name = part->vrsd_name
        lines = part->get_source( ) ) ).
    DELETE t_part WHERE lines IS INITIAL.

    userexits->modify_results(
      CHANGING
        parts         = t_part
        custom_fields = custom_fields ).

    result = VALUE #( name = object_name
                       type = object_type
                       version = zif_timem_consts=>version
                       parts = t_part
                       timestamps = get_timestamps( )
                       stats = get_stats( t_part )
                       timestamp = options->timestamp
                       aggregated_fields = NEW zcl_timem_aggregated_fields( )->build(
                         lines         = get_lines( t_part )
                         custom1_title = CONV #( custom_fields-custom1_title )
                         custom2_title = CONV #( custom_fields-custom2_title )
                         custom3_title = CONV #( custom_fields-custom3_title ) )
                       ignore_case = options->ignore_case
                       ignore_indentation = options->ignore_indentation ).

    result = CORRESPONDING #( BASE ( result ) custom_fields ).
  ENDMETHOD.


  METHOD get_lines.
    result = VALUE ztimem_line_t(
      FOR part IN parts
      FOR line IN part-lines
      ( line ) ).
  ENDMETHOD.


  METHOD get_stats.
    result = NEW zcl_timem_stats( get_lines( parts ) )->stats.
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
    DATA(options_timestamp) = options->timestamp.
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
          DATA(part) = NEW zcl_timem_part( name = s_part->name
                                           vrsd_name = s_part->object_name
                                           vrsd_type = s_part->type ).
          INSERT part INTO TABLE parts.
        CATCH zcx_timem.
          ASSERT 1 = 1. " Doesn't exist? Carry on
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD revert.
    LOOP AT parts INTO DATA(part).
      part->revert( ts ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
