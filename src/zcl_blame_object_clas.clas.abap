CLASS zcl_blame_object_clas DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_blame_object .

    METHODS constructor
      IMPORTING
        !i_name TYPE seoclsname .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_name TYPE seoclsname.
ENDCLASS.



CLASS zcl_blame_object_clas IMPLEMENTATION.
  METHOD constructor.
    g_name = i_name.
  ENDMETHOD.

  METHOD zif_blame_object~get_includes.
    APPEND cl_oo_classname_service=>get_ccdef_name( g_name ) TO rt_include.
    APPEND cl_oo_classname_service=>get_ccmac_name( g_name ) TO rt_include.
    APPEND cl_oo_classname_service=>get_ccimp_name( g_name ) TO rt_include.
    APPEND cl_oo_classname_service=>get_cl_name( g_name ) TO rt_include.
    APPEND cl_oo_classname_service=>get_ccau_name( g_name ) TO rt_include.
    APPEND cl_oo_classname_service=>get_pubsec_name( g_name ) TO rt_include.
    APPEND cl_oo_classname_service=>get_prosec_name( g_name ) TO rt_include.
    APPEND cl_oo_classname_service=>get_prisec_name( g_name ) TO rt_include.
    APPEND cl_oo_classname_service=>get_classpool_name( g_name ) TO rt_include.
    APPEND cl_oo_classname_service=>get_ct_name( g_name ) TO rt_include.

    LOOP AT cl_oo_classname_service=>get_all_method_includes( g_name ) INTO DATA(s_method_include).
      APPEND s_method_include-incname TO rt_include.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_blame_object~get_name.
    r_name = g_name.
  ENDMETHOD.
ENDCLASS.
