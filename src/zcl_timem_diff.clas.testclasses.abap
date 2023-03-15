*"* use this source file for your ABAP unit test classes
CLASS ltcl_diff DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: o_diff TYPE REF TO zcl_timem_diff.
    METHODS:
      setup,
      changed_line FOR TESTING RAISING cx_static_check,
      ignore_case FOR TESTING RAISING cx_static_check,
      ignore_indentation FOR TESTING RAISING cx_static_check,
      empty_old FOR TESTING RAISING cx_static_check,
      empty_both FOR TESTING RAISING cx_static_check,
      empty_new FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_diff IMPLEMENTATION.
  METHOD setup.
  ENDMETHOD.


  METHOD changed_line.
    o_diff = NEW #( ignore_case        = abap_false
                    ignore_indentation = abap_false ).
    DATA(t_blame) = o_diff->compute( lines_old   = VALUE #( ( source = 'AaA' author = 'A' ) )
                                     lines_new   = VALUE #( ( source = 'aAa' author = 'B' ) ) ).
    cl_abap_unit_assert=>assert_equals( act = t_blame[ 1 ]-author
                                        exp = 'B' ).
  ENDMETHOD.


  METHOD ignore_case.
    o_diff = NEW #( ignore_case        = abap_true
                    ignore_indentation = abap_false ).
    DATA(t_blame) = o_diff->compute( lines_old   = VALUE #( ( source = 'AaA' author = 'A' ) )
                                     lines_new   = VALUE #( ( source = 'aAa' author = 'B' ) ) ).
    cl_abap_unit_assert=>assert_equals( act = t_blame[ 1 ]-author
                                        exp = 'A' ).
  ENDMETHOD.


  METHOD ignore_indentation.
    o_diff = NEW #( ignore_case        = abap_false
                    ignore_indentation = abap_true ).
    DATA(t_blame) = o_diff->compute( lines_old   = VALUE #( ( source = '  AaA' author = 'A' ) )
                                     lines_new   = VALUE #( ( source = '    AaA' author = 'B' ) ) ).
    cl_abap_unit_assert=>assert_equals( act = t_blame[ 1 ]-author
                                        exp = 'A' ).
  ENDMETHOD.


  METHOD empty_old.
    DATA(t_blame) = o_diff->compute( lines_old   = VALUE #( )
                                     lines_new   = VALUE #( ( source = 'bbb' author = 'B' )
                                                         ( source = 'bbb' author = 'B' ) ) ).
    cl_abap_unit_assert=>assert_equals( act = t_blame[ 1 ]-author
                                        exp = 'B' ).
    cl_abap_unit_assert=>assert_equals( act = lines( t_blame )
                                        exp = 2 ).
  ENDMETHOD.


  METHOD empty_new.
    DATA(t_blame) = o_diff->compute( lines_old   = VALUE #( ( source = 'bbb' author = 'B' )
                                                         ( source = 'bbb' author = 'B' ) )
                                     lines_new   = VALUE #( ) ).
    cl_abap_unit_assert=>assert_equals( act = lines( t_blame )
                                        exp = 0 ).
  ENDMETHOD.


  METHOD empty_both.
    DATA(t_blame) = o_diff->compute( lines_old   = VALUE #( )
                                     lines_new   = VALUE #( ) ).
    cl_abap_unit_assert=>assert_equals( act = lines( t_blame )
                                        exp = 0 ).
  ENDMETHOD.
ENDCLASS.
