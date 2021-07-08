class ZCL_TIMEM_USEREXITS definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods BEFORE_RENDERING
    changing
      !DATA type ZTIMEM_DATA .
  methods ON_SAPEVENT
    importing
      !ACTION type C
      !GETDATA type C .
  methods MODIFY_PART_LIST
    changing
      !PART_LIST type ZTIMEM_PART_T .
  methods MODIFY_ASSET_CONTENT
    importing
      !SUBTYPE type C
    changing
      !CONTENT type STRING .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_userexits TYPE STANDARD TABLE OF REF TO zif_timem_userexit WITH KEY table_line.
    DATA options TYPE REF TO zcl_timem_options.

    METHODS get_instances
      RETURNING VALUE(result) TYPE ty_userexits.
ENDCLASS.



CLASS ZCL_TIMEM_USEREXITS IMPLEMENTATION.


  METHOD before_rendering.
    DATA(userexits) = get_instances( ).
    LOOP AT userexits INTO DATA(userexit).
      TRY.
          userexit->before_rendering(
            EXPORTING
              options = options
            CHANGING
              data = data ).
        CATCH cx_sy_dyn_call_illegal_method.
          ASSERT 1 = 1. " Not implemented? Carry on.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    options = zcl_timem_options=>get_instance( ).
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
    DATA(userexits) = get_instances( ).
    LOOP AT userexits INTO DATA(userexit).
      TRY.
          userexit->modify_asset_content(
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
    DATA(userexits) = get_instances( ).
    LOOP AT userexits INTO DATA(userexit).
      TRY.
          userexit->modify_part_list(
            EXPORTING
              options = options
            CHANGING
              part_list = part_list ).
        CATCH cx_sy_dyn_call_illegal_method.
          ASSERT 1 = 1. " Not implemented? Carry on.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD on_sapevent.
    DATA(userexits) = get_instances( ).
    LOOP AT userexits INTO DATA(userexit).
      TRY.
          userexit->on_sapevent(
            options  = options
             action  = action
             getdata = getdata ).
        CATCH cx_sy_dyn_call_illegal_method.
          ASSERT 1 = 1. " Not implemented? Carry on.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
