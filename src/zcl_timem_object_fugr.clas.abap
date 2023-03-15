"! Representation of a function group object. It will be able to create and
"! return a list of all the parts the function group is made of.
CLASS zcl_timem_object_fugr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_timem_object .

    "! Constructor for the function group object.
    "! @parameter i_name | Function group name
    METHODS constructor
      IMPORTING
        !name TYPE rs38l_area .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA name TYPE rs38l_area .

    METHODS get_main_name
      RETURNING
        VALUE(result) TYPE program .
    METHODS get_functions
      RETURNING
        VALUE(result) TYPE re_t_funcincl .
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_FUGR IMPLEMENTATION.


  METHOD constructor.
    me->name = name.
  ENDMETHOD.


  METHOD get_functions.
    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = name
      TABLES
        functab                 = result
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.
    IF sy-subrc = 0.
      SORT result BY funcname ASCENDING.
      DELETE ADJACENT DUPLICATES FROM result COMPARING funcname.
    ENDIF.
  ENDMETHOD.


  METHOD get_main_name.
    DATA namespace TYPE rs38l-namespace.
    DATA group     TYPE rs38l-area.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area = name
      IMPORTING
        namespace     = namespace
        group         = group
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc = 0.
      result = |{ namespace }SAPL{ group }|.
    ENDIF.
  ENDMETHOD.


  METHOD zif_timem_object~check_exists.
    CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
      EXPORTING
        function_pool   = name
      EXCEPTIONS
        pool_not_exists = 1.
    result = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_timem_object~get_name.
    result = name.
  ENDMETHOD.


  METHOD zif_timem_object~get_tadir_list.
    DATA t_include TYPE STANDARD TABLE OF progname.

    DATA(main_program) = get_main_name( ).
    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = main_program
      TABLES
        includetab   = t_include
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_timem.
    ENDIF.

    result = VALUE #( (
      name        = |Program { main_program }|
      object_name = CONV #( main_program )
      type        = 'REPS' ) ).

    DATA(t_function) = get_functions( ).
    LOOP AT t_include INTO DATA(include).
      IF NOT line_exists( t_function[ include = include ] ).
        result = VALUE #( BASE result
                         ( name        = |Include { include }|
                           object_name = CONV #( include )
                           type        = 'REPS' ) ).
      ENDIF.
    ENDLOOP.

    LOOP AT t_function REFERENCE INTO DATA(os_function).
      result = VALUE #( BASE result
                         ( name        = |Function module { os_function->funcname }|
                           object_name = CONV #( os_function->funcname )
                           type        = 'FUNC' ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
