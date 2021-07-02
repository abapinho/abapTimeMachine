"! An SAP user
class ZCL_TIMEM_AUTHOR definition
  public
  final
  create public .

public section.

    "! Returns the user's name (or the username if the user no longer exists)
  methods GET_NAME
    importing
      !UNAME type SYUNAME
    returning
      value(result) type STRING
    raising
      ZCX_TIMEM .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_author,
        uname TYPE syuname,
        name  TYPE string,
      END OF ty_s_author,
      ty_t_author TYPE SORTED TABLE OF ty_s_author WITH UNIQUE KEY uname.

    CLASS-DATA authors TYPE ty_t_author.
ENDCLASS.



CLASS ZCL_TIMEM_AUTHOR IMPLEMENTATION.


  METHOD get_name.
    DATA author LIKE LINE OF authors.

    READ TABLE authors INTO author WITH KEY uname = uname.
    IF sy-subrc <> 0.
      author-uname = uname.
      SELECT SINGLE name_textc INTO author-name
      FROM user_addr
      WHERE bname = uname.
      IF sy-subrc <> 0.
        author-name = uname.
      ENDIF.
      INSERT author INTO TABLE authors.
    ENDIF.
    result = author-name.
  ENDMETHOD.
ENDCLASS.
