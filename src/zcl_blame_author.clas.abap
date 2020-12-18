CLASS zcl_blame_author DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_name
      IMPORTING
                i_uname       TYPE syuname
      RETURNING VALUE(r_name) TYPE string
      RAISING   zcx_blame.

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



CLASS zcl_blame_author IMPLEMENTATION.
  METHOD get_name.
    DATA s_author LIKE LINE OF gt_author.

    READ TABLE gt_author INTO s_author WITH KEY uname = i_uname.
    IF sy-subrc <> 0.
      s_author-uname = i_uname.
      SELECT SINGLE name_textc INTO s_author-name
      FROM user_addr
      WHERE bname = i_uname.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_blame. " TODO
      ENDIF.
      INSERT s_author INTO TABLE gt_author.
    ENDIF.
    r_name = s_author-name.
  ENDMETHOD.
ENDCLASS.
