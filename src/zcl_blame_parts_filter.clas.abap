CLASS zcl_blame_parts_filter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA version_number TYPE versno READ-ONLY.
    DATA r_object_type TYPE RANGE OF versobjtyp READ-ONLY.
    DATA r_object_name TYPE RANGE OF versobjnam READ-ONLY.
    DATA is_filtered TYPE flag READ-ONLY.

    METHODS constructor
      IMPORTING
        i_version_number TYPE versno OPTIONAL
        i_object_type    TYPE versobjtyp OPTIONAL
        i_object_name    TYPE versobjnam OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BLAME_PARTS_FILTER IMPLEMENTATION.


  METHOD constructor.
    IF i_object_name IS NOT INITIAL OR
       i_object_type IS NOT INITIAL OR
       ( i_version_number IS NOT INITIAL AND i_version_number <> zcl_blame_version=>c_version-modified ).
      r_object_type = VALUE #( option = 'EQ' sign = 'I' ( low = i_object_type ) ).
      r_object_name = VALUE #( option = 'EQ' sign = 'I' ( low = i_object_name ) ).
      is_filtered = abap_true.
    ENDIF.

    me->version_number = COND #(
      WHEN i_version_number IS INITIAL
      THEN zcl_blame_version=>c_version-modified
      ELSE i_version_number ).
  ENDMETHOD.
ENDCLASS.
