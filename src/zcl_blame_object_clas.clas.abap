"! Representation of a class object. It will be able to create and return a list
"! of all the parts the class is made of.
CLASS zcl_blame_object_clas DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_blame_object .

    "! Constructor for the class object.
    "! @parameter i_name | Class name
    METHODS constructor
      IMPORTING
        !i_name TYPE seoclsname.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_name TYPE seoclsname.
ENDCLASS.



CLASS zcl_blame_object_clas IMPLEMENTATION.
  METHOD constructor.
    g_name = i_name.
  ENDMETHOD.


  METHOD zif_blame_object~get_part_list.
    DATA(t_method_include) = cl_oo_classname_service=>get_all_method_includes( g_name ).

    io_counter->initialize( 9 + lines( t_method_include ) ).

    INSERT NEW #( i_name = 'Class pool'
                  i_vrsd_name = CONV #( g_name )
                  i_vrsd_type = 'CLSD' ) INTO TABLE rt_part.
    io_counter->next( |Class pool { g_name }| ).

    INSERT NEW #( i_name = 'Public section'
                  i_vrsd_name = CONV #( g_name )
                  i_vrsd_type = 'CPUB' ) INTO TABLE rt_part.
    io_counter->next( |Public section { g_name }| ).

    INSERT NEW #( i_name = 'Protected section'
                  i_vrsd_name = CONV #( g_name )
                  i_vrsd_type = 'CPRO' ) INTO TABLE rt_part.
    io_counter->next( |Protected section { g_name }| ).

    INSERT NEW #( i_name = 'Private section'
                  i_vrsd_name = CONV #( g_name )
                  i_vrsd_type = 'CPRI' ) INTO TABLE rt_part.
    io_counter->next( |Private section { g_name }| ).

    TRY.
        INSERT NEW #( i_name = 'Local class definition'
                      i_vrsd_name = cl_oo_classname_service=>get_ccdef_name( g_name )
                      i_vrsd_type = 'CDEF' ) INTO TABLE rt_part.
      CATCH zcx_blame.
        ASSERT 1 = 1. " Doesn't exist? Carry on
    ENDTRY.
    io_counter->next( |Local class definition { g_name }| ).

    TRY.
        INSERT NEW #( i_name = 'Local class implementation'
                      i_vrsd_name = cl_oo_classname_service=>get_ccimp_name( g_name )
                      i_vrsd_type = 'CINC' ) INTO TABLE rt_part.
      CATCH zcx_blame.
        ASSERT 1 = 1. " Doesn't exist? Carry on
    ENDTRY.
    io_counter->next( |Local class implementation { g_name }| ).

    TRY.
        INSERT NEW #( i_name = 'Local macros'
                      i_vrsd_name = cl_oo_classname_service=>get_ccmac_name( g_name )
                      i_vrsd_type = 'CINC' ) INTO TABLE rt_part.
      CATCH zcx_blame.
        ASSERT 1 = 1. " Doesn't exist? Carry on
    ENDTRY.
    io_counter->next( |Local macros { g_name }| ).

    TRY.
        INSERT NEW #( i_name = 'Local types'
                      i_vrsd_name = cl_oo_classname_service=>get_cl_name( g_name )
                      i_vrsd_type = 'REPS' ) INTO TABLE rt_part.
      CATCH zcx_blame.
        ASSERT 1 = 1. " Doesn't exist? Carry on
    ENDTRY.
    io_counter->next( |Local types { g_name }| ).

    TRY.
        INSERT NEW #( i_name = 'Local test classes'
                      i_vrsd_name = cl_oo_classname_service=>get_ccau_name( g_name )
                      i_vrsd_type = 'CINC' ) INTO TABLE rt_part.
      CATCH zcx_blame.
        ASSERT 1 = 1. " Doesn't exist? Carry on
    ENDTRY.
    io_counter->next( |Local test classes { g_name }| ).

    LOOP AT cl_oo_classname_service=>get_all_method_includes( g_name ) INTO DATA(s_method_include).
      DATA(method_name) = cl_oo_classname_service=>get_method_by_include( s_method_include-incname )-cpdname.
      INSERT NEW #( i_name = |{ to_lower( method_name ) }()|
                    i_vrsd_name = |{ g_name WIDTH = 30 }{ method_name }|
                    i_vrsd_type = 'METH' ) INTO TABLE rt_part.
      io_counter->next( |Method { to_lower( method_name ) }()| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_blame_object~get_name.
    r_name = g_name.
  ENDMETHOD.


  METHOD zif_blame_object~check_exists.
    CALL METHOD cl_abap_classdescr=>describe_by_name
      EXPORTING
        p_name         = g_name
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2.
    IF sy-subrc = 0.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
