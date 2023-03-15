CLASS zcl_timem_versno DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS to_internal
      IMPORTING
                versno        TYPE versno
      RETURNING VALUE(result) TYPE versno.

    CLASS-METHODS to_external
      IMPORTING
                versno        TYPE versno
      RETURNING VALUE(result) TYPE versno.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_timem_versno IMPLEMENTATION.
  METHOD to_internal.
    " Technically the current version is 0 but in order to keep them properly sorted we're
    " setting it to magic number 99997 (because 'ACTIVE' is 99998 and 'MODIFIED' is 99999.
    " But when we're going to fetch it from the database we must use 0.
    result = COND #(
      WHEN versno = zcl_timem_version=>c_version-latest THEN zcl_timem_version=>c_version-latest_db
      ELSE versno ).
  ENDMETHOD.


  METHOD to_external.
    " We consider the latest version to be 99998 instead of 0 for sorting reasons
    result = COND #(
      WHEN versno = zcl_timem_version=>c_version-latest_db THEN zcl_timem_version=>c_version-latest
      ELSE versno ).
  ENDMETHOD.
ENDCLASS.
