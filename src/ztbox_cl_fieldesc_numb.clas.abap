class ZTBOX_CL_FIELDESC_NUMB definition
  public
  inheriting from ZTBOX_CL_FIELDESC
  final
  create public .

public section.

  methods DECIMALS_SEP
    importing
      !I_DECIMALS_SEP type CHAR1
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC_NUMB .
  methods THOUSAND_SEP
    importing
      !I_THOUSAND_SEP type CHAR1
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC_NUMB .
  methods CONSTRUCTOR .
  methods VALID_NUMB
    importing
      !VALUE type STRING
    returning
      value(FAIL) type FLAG .

  methods CONFIG
    redefinition .
protected section.

  methods OUTPUT
    redefinition .
  methods _WRITE_TO_STR
    redefinition .
private section.

  data _DECIMALS type I .

  methods WRITE_NUMB
    importing
      !VALUE type NUMERIC
    returning
      value(OUTPUT) type STRING .
ENDCLASS.



CLASS ZTBOX_CL_FIELDESC_NUMB IMPLEMENTATION.


  METHOD config.

    super->config( i_config ).

    thousand_sep( i_config-thousand_sep ).
    decimals_sep( i_config-decimals_sep ).

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    super->constructor( ).

    _add_post_validation( |VALID_NUMB| ).

  ENDMETHOD.


  METHOD DECIMALS_SEP.

    _decimals_sep = i_decimals_sep.

    r_res = me.

  ENDMETHOD.


  METHOD output.

    super->output( CHANGING value = value ).

    IF _thousand_sep IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF  _thousand_sep IN value WITH space.
    ENDIF.

    IF _decimals_sep IS NOT INITIAL.
      REPLACE FIRST OCCURRENCE OF _decimals_sep IN value WITH '.'.
    ENDIF.

    CONDENSE value NO-GAPS.

  ENDMETHOD.


  METHOD thousand_sep.

    _thousand_sep = i_thousand_sep.

    r_res = me.

  ENDMETHOD.


  METHOD valid_numb.

    fail = abap_true.

    FIND ALL OCCURRENCES OF '.' IN value MATCH COUNT DATA(dot_cnt).
    CHECK dot_cnt LE 1.

    FIND ALL OCCURRENCES OF '-' IN value MATCH COUNT DATA(sign_cnt) MATCH OFFSET DATA(sign_off).
    CHECK sign_cnt LE 1.
    CHECK sign_off EQ 0 OR sign_off EQ strlen( value ) - 1.

    CHECK value CO '.-0123456789'.

    fail = abap_false.

  ENDMETHOD.


  METHOD write_numb.

    IF _decimals IS NOT INITIAL.

      DATA c_num TYPE c LENGTH cl_abap_elemdescr=>type_c_max_length.
      WRITE value TO c_num DECIMALS _decimals LEFT-JUSTIFIED.
      output = c_num.

    ELSE.

      output = |{ value COUNTRY = _country }|.

    ENDIF.

    CONDENSE output.

  ENDMETHOD.


  METHOD _write_to_str.

    output = write_numb( value ).

  ENDMETHOD.
ENDCLASS.
