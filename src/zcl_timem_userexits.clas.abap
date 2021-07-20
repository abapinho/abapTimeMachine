CLASS zcl_timem_userexits DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS modify_results
      CHANGING
        !parts         TYPE ztimem_part_source_t
        !custom_fields TYPE ztimem_data_custom_fields .

    METHODS on_sapevent
      IMPORTING
        !action  TYPE c
        !getdata TYPE c .

    METHODS modify_part_list
      CHANGING
        !part_list TYPE ztimem_part_t .

    METHODS modify_asset_content
      IMPORTING
        !subtype TYPE c
      CHANGING
        !content TYPE string .

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_userexits TYPE STANDARD TABLE OF REF TO zif_timem_userexit WITH KEY table_line.
    data instances type ty_userexits.
    DATA options TYPE REF TO zcl_timem_options.

    METHODS get_instances
      RETURNING VALUE(result) TYPE ty_userexits.
ENDCLASS.



CLASS ZCL_TIMEM_USEREXITS IMPLEMENTATION.


  METHOD constructor.
    options = zcl_timem_options=>get_instance( ).
    instances = get_instances( ).
  ENDMETHOD.


  METHOD get_instances.
    DATA impkey TYPE  seoclskey.
    DATA impkeys TYPE seor_implementing_keys.

    impkey-clsname = 'ZIF_TIMEM_USEREXIT'.
    CALL FUNCTION 'SEO_INTERFACE_IMPLEM_GET_ALL'
      EXPORTING
        intkey       = impkey
      IMPORTING
        impkeys      = impkeys
      EXCEPTIONS
        not_existing = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA o TYPE REF TO zif_timem_userexit.
    LOOP AT impkeys INTO DATA(classdata).
      CREATE OBJECT o TYPE (classdata-clsname).
      INSERT o INTO TABLE result.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_asset_content.
    LOOP AT instances INTO DATA(instance).
      TRY.
          instance->modify_asset_content(
            EXPORTING
              options = options
              subtype = subtype
            CHANGING
              content = content ).
        CATCH cx_sy_dyn_call_illegal_method.
          ASSERT 1 = 1. " Not implemented? Carry on.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_part_list.
    LOOP AT instances INTO DATA(instance).
      TRY.
          instance->modify_part_list(
            EXPORTING
              options = options
            CHANGING
              part_list = part_list ).
        CATCH cx_sy_dyn_call_illegal_method.
          ASSERT 1 = 1. " Not implemented? Carry on.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_results.
    LOOP AT instances INTO DATA(instance).
      TRY.
          instance->modify_results(
            EXPORTING
              options       = options
            CHANGING
              parts         = parts
              custom_fields = custom_fields ).
        CATCH cx_sy_dyn_call_illegal_method.
          ASSERT 1 = 1. " Not implemented? Carry on.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD on_sapevent.
    LOOP AT instances INTO DATA(instance).
      TRY.
          instance->on_sapevent(
            options  = options
             action  = action
             getdata = getdata ).
        CATCH cx_sy_dyn_call_illegal_method.
          ASSERT 1 = 1. " Not implemented? Carry on.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
