"! An SAP user
CLASS zcl_timem_author DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Returns the user's name (or the username if the user no longer exists)
    METHODS get_name
      IMPORTING
        !uname        TYPE syuname
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_timem .
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
      SELECT name_textc INTO author-name
      UP TO 1 ROWS
      FROM user_addr
      WHERE bname = uname
      ORDER BY name_textc.
        EXIT.
      ENDSELECT.
      IF sy-subrc <> 0.
        author-name = uname.
      ENDIF.
      INSERT author INTO TABLE authors.
    ENDIF.
    result = author-name.
  ENDMETHOD.
ENDCLASS.
