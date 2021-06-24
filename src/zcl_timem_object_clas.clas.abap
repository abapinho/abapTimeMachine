"! Representation of a class object. It will be able to create and return a list
"! of all the parts the class is made of.
class ZCL_TIMEM_OBJECT_CLAS definition
  public
  final
  create public .

public section.

  interfaces ZIF_TIMEM_OBJECT .

    "! Constructor for the class object.
    "! @parameter i_name | Class name
  methods CONSTRUCTOR
    importing
      !I_NAME type SEOCLSNAME
    raising
      ZCX_TIMEM .
protected section.
private section.

  data G_NAME type SEOCLSNAME .
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_CLAS IMPLEMENTATION.


  METHOD constructor.
    g_name = i_name.
  ENDMETHOD.


  METHOD zif_timem_object~check_exists.
    cl_abap_classdescr=>describe_by_name(
      EXPORTING
        p_name         = g_name
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc = 0.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_timem_object~get_name.
    result = g_name.
  ENDMETHOD.


  METHOD zif_timem_object~get_part_list.
    " All sort of includes
    result = VALUE #(
      ( name = 'Class pool'                  object_name = CONV #( g_name )                                  type = 'CLSD' )
      ( name = 'Public section'              object_name = CONV #( g_name )                                  type = 'CPUB' )
      ( name = 'Protected section'           object_name = CONV #( g_name )                                  type = 'CPRO' )
      ( name = 'Private section'             object_name = CONV #( g_name )                                  type = 'CPRI' )
      ( name = 'Local class definition'      object_name = cl_oo_classname_service=>get_ccdef_name( g_name ) type = 'CDEF' )
      ( name = 'Local class implementation'  object_name = cl_oo_classname_service=>get_ccimp_name( g_name ) type = 'CINC' )
      ( name = 'Local macros'                object_name = cl_oo_classname_service=>get_ccmac_name( g_name ) type = 'CINC' )
      ( name = 'Local types'                 object_name = cl_oo_classname_service=>get_cl_name( g_name )    type = 'REPS' )
      ( name = 'Local test classes'          object_name = cl_oo_classname_service=>get_ccau_name( g_name )  type = 'CINC' )
    ).

    " Class methods
    result = VALUE #( BASE result
      FOR method_include IN cl_oo_classname_service=>get_all_method_includes( g_name )
      LET method_name = cl_oo_classname_service=>get_method_by_include( method_include-incname )-cpdname
      IN ( name        = |{ to_lower( method_name ) }()|
           object_name = |{ g_name WIDTH = 30 }{ method_name }|
           type        = 'METH' ) ).
  ENDMETHOD.
ENDCLASS.
