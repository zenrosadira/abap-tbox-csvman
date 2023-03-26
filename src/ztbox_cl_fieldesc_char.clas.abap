class ZTBOX_CL_FIELDESC_CHAR definition
  public
  inheriting from ZTBOX_CL_FIELDESC
  final
  create public .

public section.

  methods CONSTRUCTOR .
protected section.

  methods _WRITE_TO_STR
    redefinition .
private section.
ENDCLASS.



CLASS ZTBOX_CL_FIELDESC_CHAR IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

  ENDMETHOD.


  METHOD _write_to_str.

    ASSIGN _char_ref->* TO FIELD-SYMBOL(<val_c>).

    CLEAR <val_c>.
    WRITE value TO <val_c>.

    output = COND #( WHEN _convexit EQ abap_true THEN <val_c> ELSE value ).

    IF _escapator IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF _enclosing IN output WITH |{ _escapator }{ _enclosing }|.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
