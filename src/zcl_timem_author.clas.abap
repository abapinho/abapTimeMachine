"! An SAP user
class ZCL_TIMEM_AUTHOR definition
  public
  final
  create public .

public section.

    "! Returns the user's name (or the username if the user no longer exists)
  methods GET_NAME
    importing
      !I_UNAME type SYUNAME
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

    CLASS-DATA gt_author TYPE ty_t_author.
ENDCLASS.



CLASS ZCL_TIMEM_AUTHOR IMPLEMENTATION.


  METHOD get_name.
    DATA s_author LIKE LINE OF gt_author.

    READ TABLE gt_author INTO s_author WITH KEY uname = i_uname.
    IF sy-subrc <> 0.
      s_author-uname = i_uname.
      SELECT SINGLE name_textc INTO s_author-name
      FROM user_addr
      WHERE bname = i_uname.
      IF sy-subrc <> 0.
        s_author-name = i_uname.
      ENDIF.
      INSERT s_author INTO TABLE gt_author.
    ENDIF.
    result = s_author-name.
  ENDMETHOD.
ENDCLASS.
