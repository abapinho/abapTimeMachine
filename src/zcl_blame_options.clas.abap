CLASS zcl_blame_options DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA ignore_case TYPE boolean READ-ONLY .
    DATA ignore_indentation TYPE boolean READ-ONLY .

    METHODS constructor
      IMPORTING
        !i_ignore_case        TYPE boolean DEFAULT abap_false
        !i_ignore_indentation TYPE boolean DEFAULT abap_false.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_blame_options IMPLEMENTATION.


  METHOD constructor.
    me->ignore_case = i_ignore_case.
    me->ignore_indentation = i_ignore_indentation.
  ENDMETHOD.
ENDCLASS.
