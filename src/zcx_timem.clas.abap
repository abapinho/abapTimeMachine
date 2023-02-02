"! Exception class for all the ZBLAME classes
class ZCX_TIMEM definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of OBJECT_NOT_FOUND,
      msgid type symsgid value 'ZTIMEMACHINE',
      msgno type symsgno value '000',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of OBJECT_NOT_FOUND .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional .
  class-methods RAISE_FROM_SYST
    raising
      ZCX_TIMEM .

  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCX_TIMEM IMPLEMENTATION.


  method CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD if_message~get_longtext.
    " Get deepest long text from an exception chain
    IF me->previous IS BOUND.
      result = me->previous->get_longtext( preserve_newlines ).
    ELSE.
      result = super->if_message~get_longtext( preserve_newlines ).
    ENDIF.
  ENDMETHOD.


  METHOD if_message~get_text.
    " Get deepest text from an exception chain
    IF me->previous IS BOUND.
      result = me->previous->get_text( ).
    ELSE.
      result = super->if_message~get_text( ).
    ENDIF.
  ENDMETHOD.


  METHOD raise_from_syst.
    TRY.
        cx_proxy_t100=>raise_from_sy_msg( ).
      CATCH cx_proxy_t100 INTO DATA(exc_t100).
        RAISE EXCEPTION TYPE zcx_timem
          EXPORTING
            previous = exc_t100.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
