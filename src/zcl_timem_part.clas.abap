"! Represents a part of an object, including all the versions of that part that
"! exist in the system.
CLASS zcl_timem_part DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Object name
    DATA name TYPE string READ-ONLY .
    "! Part type
    DATA vrsd_type TYPE versobjtyp READ-ONLY .
    "! Part name
    DATA vrsd_name TYPE versobjnam READ-ONLY .

    "! Constructs a new part
    "! @parameter name | Object name
    "! @parameter vrsd_type | Part type
    "! @parameter vrsd_name | Part object
    METHODS constructor
      IMPORTING
        !name      TYPE string
        !vrsd_type TYPE versobjtyp
        !vrsd_name TYPE versobjnam
      RAISING
        zcx_timem .

    METHODS get_timestamps
      RETURNING
        VALUE(result) TYPE ztimem_timestamp_t .

    METHODS get_source
      RETURNING
        VALUE(result) TYPE ztimem_line_t
      RAISING
        zcx_timem .

    METHODS revert
      IMPORTING
                ts TYPE timestamp
      RAISING   zcx_timem.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_t_version TYPE STANDARD TABLE OF REF TO zcl_timem_version WITH KEY table_line .

    DATA versions TYPE ty_t_version .

    METHODS load_versions
      IMPORTING
        !vrsd_type TYPE versobjtyp
        !vrsd_name TYPE versobjnam
      RAISING
        zcx_timem .

    METHODS get_version_at_timestamp
      IMPORTING
        ts            TYPE timestamp
      RETURNING
        VALUE(result) TYPE REF TO zcl_timem_version .

    METHODS get_versions_until_timestamp
      IMPORTING
        ts            TYPE timestamp
      RETURNING
        VALUE(result) TYPE ty_t_version .

    "! Calculates and returns a list of the diffed source already filled with blame
    "! details.
    METHODS get_diffed_source_with_blame
      IMPORTING
        ts            TYPE timestamp
      RETURNING
        VALUE(result) TYPE ztimem_line_t
      RAISING
        zcx_timem .

    METHODS get_source_at_timestamp
      IMPORTING
        ts            TYPE timestamp
      RETURNING
        VALUE(result) TYPE ztimem_line_t
      RAISING
        zcx_timem .
ENDCLASS.



CLASS ZCL_TIMEM_PART IMPLEMENTATION.


  METHOD  constructor.
    me->name = name.
    me->vrsd_type = vrsd_type.
    me->vrsd_name = vrsd_name.
    load_versions( vrsd_type = vrsd_type
                   vrsd_name = vrsd_name ).
  ENDMETHOD.


  METHOD get_diffed_source_with_blame.
    DATA(diff) = NEW zcl_timem_diff( ).
    LOOP AT get_versions_until_timestamp( ts ) INTO DATA(version).
      result = diff->compute( lines_old = result
                              lines_new =  version->get_source( ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_source.
    DATA(options) = zcl_timem_options=>get_instance( ).
    result = SWITCH #(
      options->get_instance( )->mode
      WHEN zif_timem_consts=>mode-blame THEN get_diffed_source_with_blame( options->timestamp )
      WHEN zif_timem_consts=>mode-time_machine THEN get_source_at_timestamp( options->timestamp ) ).
  ENDMETHOD.


  METHOD get_source_at_timestamp.
    DATA(version) = get_version_at_timestamp( ts ).
    IF version IS BOUND.
      result = version->get_source( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_timestamps.
    DATA ts LIKE LINE OF result.
    LOOP AT versions INTO DATA(version).
      ts = |{ version->date }{ version->time }|.
      COLLECT ts INTO result.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_versions_until_timestamp.
    CONVERT TIME STAMP ts TIME ZONE space INTO DATE DATA(date) TIME DATA(time).
    result = VALUE #(
      FOR version IN versions
        WHERE (
          table_line->date < date OR
          ( table_line->date = date AND
            table_line->time <= time ) )
        ( version ) ).
  ENDMETHOD.


  METHOD get_version_at_timestamp.
    CONVERT TIME STAMP ts TIME ZONE space INTO DATE DATA(date) TIME DATA(time).
    " The last one should be the one we want
    LOOP AT versions INTO result WHERE
          table_line->date < date OR
          ( table_line->date = date AND
            table_line->time <= time ).
    ENDLOOP.
  ENDMETHOD.


  METHOD load_versions.
    DATA(vrsd) = NEW zcl_timem_vrsd( type = vrsd_type
                                     name = vrsd_name ).

    versions = VALUE #( FOR s_vrsd IN vrsd->vrsd_list
                        ( NEW zcl_timem_version( s_vrsd ) ) ).
  ENDMETHOD.


  METHOD revert.
    DATA(version) = get_version_at_timestamp( ts ).
    IF version IS BOUND.
      version->retrieve( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
