class ZTBOX_CL_CSVFIELD_CHAR definition
  public
  inheriting from ZTBOX_CL_CSVFIELD
  final
  create public .

public section.

  methods CONSTRUCTOR .
protected section.

  methods _WRITE_TO_STR
    redefinition .
private section.
ENDCLASS.



CLASS ZTBOX_CL_CSVFIELD_CHAR IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    super->constructor( ).

  ENDMETHOD.


  METHOD _write_to_str.

    IF _conf-convexit EQ abap_false.

      output = value.

    ELSE.

      ASSIGN _char_ref->* TO FIELD-SYMBOL(<val_c>).

      CLEAR <val_c>.
      WRITE value TO <val_c>.

      output = <val_c>.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
