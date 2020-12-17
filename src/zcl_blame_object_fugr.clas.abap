CLASS zcl_blame_object_fugr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_blame_object .

    METHODS constructor
      IMPORTING
        i_name TYPE rs38l_area.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_name TYPE rs38l_area.

    METHODS get_main_name
      RETURNING
        VALUE(r_program) TYPE program.

    METHODS get_functions
      RETURNING VALUE(rt_function) TYPE re_t_funcincl.
ENDCLASS.



CLASS zcl_blame_object_fugr IMPLEMENTATION.


  METHOD constructor.
    g_name = i_name.
  ENDMETHOD.


  METHOD get_functions.
    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = g_name
      TABLES
        functab                 = rt_function
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.
    IF sy-subrc = 0.
      SORT rt_function BY funcname ASCENDING.
      DELETE ADJACENT DUPLICATES FROM rt_function COMPARING funcname.
    ENDIF.
  ENDMETHOD.


  METHOD get_main_name.
    DATA namespace TYPE rs38l-namespace.
    DATA group     TYPE rs38l-area.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area = me->g_name
      IMPORTING
        namespace     = namespace
        group         = group
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc = 0.
      r_program = |{ namespace }SAPL{ group }|.
    ENDIF.
  ENDMETHOD.


  METHOD zif_blame_object~check_exists.
    CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
      EXPORTING
        function_pool   = g_name
      EXCEPTIONS
        pool_not_exists = 1.
    r_result = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_blame_object~get_name.
    r_name = g_name.
  ENDMETHOD.


  METHOD zif_blame_object~get_part_list.
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
      RAISE EXCEPTION TYPE zcx_blame.
    ENDIF.

    DATA(o_counter) = NEW zcl_blame_counter( 1 + lines( t_include ) ).

    rt_part = VALUE #( ( NEW #( i_name = |Program { main_program }|
                              i_vrsd_name = CONV #( main_program )
                              i_vrsd_type = 'REPS' ) ) ).
    RAISE EVENT zif_blame_object~percentage_complete EXPORTING percentage = o_counter->next( ) text = |Program { main_program }|.

    DATA(t_function) = get_functions( ).
    LOOP AT t_include INTO DATA(include).
      IF NOT line_exists( t_function[ include = include ] ).
        rt_part = VALUE #( BASE rt_part
                         ( NEW #( i_name = |Include { include }|
                                  i_vrsd_name = CONV #( include )
                                  i_vrsd_type = 'REPS' ) ) ).
        RAISE EVENT zif_blame_object~percentage_complete EXPORTING percentage = o_counter->next( ) text = |Include { include }|.
      ENDIF.
    ENDLOOP.

    LOOP AT t_function REFERENCE INTO DATA(os_function).
      rt_part = VALUE #( BASE rt_part
                         ( NEW #( i_name      = |Function module { os_function->funcname }|
                                i_vrsd_name = CONV #( os_function->funcname )
                                i_vrsd_type = 'FUNC' ) ) ).
      RAISE EVENT zif_blame_object~percentage_complete EXPORTING percentage = o_counter->next( ) text = |Function module { os_function->funcname }|.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
