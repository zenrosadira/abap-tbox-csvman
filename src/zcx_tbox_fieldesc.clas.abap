class ZCX_TBOX_FIELDESC definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  constants ZCX_TBOX_FIELDESC type SOTR_CONC value '00155D57F8A61EEDB2FC2CC12E049642' ##NO_TEXT.
  data METHOD type SEOCMPNAME .
  data SAP_VALUE type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !METHOD type SEOCMPNAME optional
      !SAP_VALUE type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_TBOX_FIELDESC IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_TBOX_FIELDESC .
 ENDIF.
me->METHOD = METHOD .
me->SAP_VALUE = SAP_VALUE .
  endmethod.
ENDCLASS.
