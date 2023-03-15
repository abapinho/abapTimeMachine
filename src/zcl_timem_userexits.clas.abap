CLASS zcl_timem_userexits DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .

    METHODS constructor .

    METHODS before_rendering
      CHANGING
        !data TYPE ztimem_data .

    METHODS modify_parts
      CHANGING
        !parts TYPE ztimem_part_source_t .

    METHODS modify_summary
      CHANGING
        !summary TYPE ztimem_summary
      RAISING
        zcx_timem.

    METHODS on_sapevent
      IMPORTING
        !action  TYPE c
        !getdata TYPE c .

    METHODS modify_tadir_list
      CHANGING
        !tadir_list TYPE ztimem_part_t
      RAISING
        zcx_timem.

    METHODS modify_asset_content
      IMPORTING
        !subtype TYPE c
      CHANGING
        !content TYPE string .

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_userexits TYPE STANDARD TABLE OF REF TO zif_timem_userexit WITH KEY table_line.
    CLASS-DATA instances TYPE ty_userexits.
    DATA options TYPE REF TO zcl_timem_options.

    CLASS-METHODS load_instances.
ENDCLASS.



CLASS zcl_timem_userexits IMPLEMENTATION.


  METHOD before_rendering.
    LOOP AT instances INTO DATA(instance).
      TRY.
          instance->before_rendering(
            EXPORTING
              options = options
            CHANGING
              data = data ).
        CATCH cx_sy_dyn_call_illegal_method.
          " Not implemented? Carry on.
          ASSERT 1 = 1.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD class_constructor.
    load_instances( ).
  ENDMETHOD.


  METHOD constructor.
    options = zcl_timem_options=>get_instance( ).
  ENDMETHOD.


  METHOD load_instances.
    DATA impkeys TYPE seor_implementing_keys.
    DATA(impkey) = VALUE seoclskey( clsname = 'ZIF_TIMEM_USEREXIT' ).
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
      CREATE OBJECT o TYPE (classdata-clsname). "#EC PREF_NEW
      INSERT o INTO TABLE instances.
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
          " Not implemented? Carry on.
          ASSERT 1 = 1.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_parts.
    LOOP AT instances INTO DATA(instance).
      TRY.
          instance->modify_parts(
            EXPORTING
              options       = options
            CHANGING
              parts         = parts ).
        CATCH cx_sy_dyn_call_illegal_method.
          " Not implemented? Carry on.
          ASSERT 1 = 1.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_tadir_list.
    LOOP AT instances INTO DATA(instance).
      TRY.
          instance->modify_tadir_list(
            EXPORTING
              options = options
            CHANGING
              tadir_list = tadir_list ).
        CATCH cx_sy_dyn_call_illegal_method.
          " Not implemented? Carry on.
          ASSERT 1 = 1.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_summary.
    LOOP AT instances INTO DATA(instance).
      TRY.
          instance->modify_summary(
            EXPORTING
              options = options
            CHANGING
              summary = summary ).
        CATCH cx_sy_dyn_call_illegal_method.
          " Not implemented? Carry on.
          ASSERT 1 = 1.
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
          " Not implemented? Carry on.
          ASSERT 1 = 1.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
