"! Representation of a program/include object. This object consists of a single part which
"! this class  will be able to create and return.
CLASS zcl_timem_object_tr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_timem_object.

    METHODS constructor
      IMPORTING
        !id TYPE  trkorr.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_t_object TYPE TABLE OF REF TO zif_timem_object WITH KEY table_line.
    DATA id TYPE trkorr.

    METHODS get_object_keys
      RETURNING
        VALUE(result) TYPE trwbo_t_e071
      RAISING
        zcx_timem.

    METHODS get_objects_for_keys
      IMPORTING
        object_keys   TYPE trwbo_t_e071
      RETURNING
        VALUE(result) TYPE ty_t_object
      RAISING
        zcx_timem.

    METHODS get_object
      IMPORTING
        object_key    TYPE trwbo_s_e071
      RETURNING
        VALUE(result) TYPE REF TO zif_timem_object
      RAISING
        zcx_timem.
ENDCLASS.



CLASS zcl_timem_object_tr IMPLEMENTATION.


  METHOD constructor.
    me->id = id.
  ENDMETHOD.


  METHOD get_object.
    result = COND #(
      WHEN object_key-pgmid = 'R3TR' AND ( object_key-object = 'CLAS' OR object_key-object = 'FUGR' )
        OR object_key-pgmid = 'LIMU' AND object_key-object = 'FUNC'
      THEN NEW zcl_timem_object_factory( )->get_instance(
        object_type = object_key-object
        object_name = CONV #( object_key-obj_name ) )
      WHEN object_key-pgmid = 'LIMU' AND object_key-object = 'REPS'
      THEN NEW zcl_timem_object_factory( )->get_instance(
        object_type = 'PROG'
        object_name = CONV #( object_key-obj_name ) ) ).
  ENDMETHOD.


  METHOD get_object_keys.
    DATA request_data TYPE trwbo_request.

    request_data-h-trkorr = id.
    CALL FUNCTION 'TRINT_READ_REQUEST'
      EXPORTING
        iv_read_objs  = abap_true
      CHANGING
        cs_request    = request_data
      EXCEPTIONS
        error_occured = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_timem.
    ENDIF.
    result = request_data-objects.
    SORT result BY pgmid ASCENDING object ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM result COMPARING pgmid object obj_name.
  ENDMETHOD.


  METHOD get_objects_for_keys.
    result = VALUE #(
      FOR object_key IN object_keys
      ( get_object( object_key ) ) ).
    DELETE result WHERE table_line IS NOT BOUND.
  ENDMETHOD.


  METHOD zif_timem_object~check_exists.
    TRY.
        NEW zcl_timem_request( me->id ).
        result = abap_true.
      CATCH zcx_timem.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_timem_object~get_name.
    result = id.
  ENDMETHOD.


  METHOD zif_timem_object~get_tadir_list.
    DATA(object_keys) = get_object_keys( ).
    DATA(objects) = get_objects_for_keys( object_keys ).
    result = REDUCE #(
      INIT t = VALUE #(  )
      FOR object IN objects
      FOR tadir_item IN object->get_tadir_list( )
      NEXT t = VALUE #( BASE t ( tadir_item ) ) ).
  ENDMETHOD.
ENDCLASS.
