class ZTBOX_CL_CSVFIELD_NUMB definition
  public
  inheriting from ZTBOX_CL_CSVFIELD
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods OVERFLOW
    importing
      !VALUE type STRING
    returning
      value(FAIL) type FLAG .
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
  methods _REFORMAT
    redefinition .
private section.

  data _THOUSAND_SEP type CHAR1 .
  data _DECIMALS_SEP type CHAR1 .

  methods WRITE_NUMB
    importing
      !VALUE type NUMERIC
    returning
      value(OUTPUT) type STRING .
  methods _SET_NUMBER_FORMAT
    importing
      !I_CONFIG type TY_CONF .
ENDCLASS.



CLASS ZTBOX_CL_CSVFIELD_NUMB IMPLEMENTATION.


  METHOD config.

    super->config( i_config ).

    _set_number_format( i_config ).

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    super->constructor( ).

    add_post_validation(
      check_object  = me
      check_method  = |VALID_NUMB| ).

    add_post_validation(
      check_object  = me
      check_method  = |OVERFLOW| ).

  ENDMETHOD.


  METHOD output.

    super->output( CHANGING value = value ).

    IF _conf-convexit EQ abap_true AND _convexit_routine IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF _thousand_sep IS NOT INITIAL.
      value = replace( val = value occ = 0 sub = _thousand_sep with = ` ` ).
    ENDIF.

    IF _decimals_sep IS NOT INITIAL.
      value = replace( val = value occ = 0 sub = _decimals_sep with = `.` ).
    ENDIF.

    CONDENSE value NO-GAPS.

  ENDMETHOD.


  METHOD overflow.

    fail = abap_true.

    SPLIT value AT '.' INTO DATA(int) DATA(dec).
    int = replace( val = int occ = 0 sub = `-` with = ` ` ).
    CONDENSE int.

    CHECK strlen( int ) LE _elem->length * 2 - 1 - _elem->decimals.

    fail = abap_false.

  ENDMETHOD.


  METHOD VALID_NUMB.

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

    DATA val_str TYPE string.
    val_str = value.

    SPLIT val_str AT '.' INTO DATA(int) DATA(dec).
    IF dec IS INITIAL.
      output = int.
      CONDENSE output NO-GAPS.
      RETURN.
    ENDIF.

    DATA(country) = COND land1(
      WHEN  _conf-use_number_format  EQ abap_false
        THEN _conf-country
      WHEN  _conf-number_format      EQ ztbox_cl_csvman=>c_dec_separator_1 OR
            _conf-number_format      EQ ztbox_cl_csvman=>c_dec_separator_3
        THEN 'IT'
      WHEN  _conf-number_format      EQ ztbox_cl_csvman=>c_dec_separator_2
        THEN 'US' ).

    output = COND #(
      WHEN _conf-decimals IS NOT INITIAL THEN |{ value DECIMALS = _conf-decimals COUNTRY = country }|
      ELSE |{ value COUNTRY = country }| ).

    IF _conf-use_number_format EQ abap_true AND _conf-number_format EQ ztbox_cl_csvman=>c_dec_separator_3.
      output = replace( val = output occ = 0 sub = `.` with = ` ` ).
    ENDIF.

  ENDMETHOD.


  METHOD _reformat.

    _set_number_format( _conf ).

  ENDMETHOD.


  METHOD _SET_NUMBER_FORMAT.

    DATA(number_format) = COND #(
      WHEN i_config-use_number_format EQ abap_true
        THEN i_config-number_format
      ELSE VALUE #( _t005x[ land = i_config-country ]-xdezp OPTIONAL ) ).

    _thousand_sep = COND #(
      WHEN number_format EQ ztbox_cl_csvman=>c_dec_separator_1 THEN '.'
      WHEN number_format EQ ztbox_cl_csvman=>c_dec_separator_2 THEN ','
      WHEN number_format EQ ztbox_cl_csvman=>c_dec_separator_3 THEN ' ' ).

    _decimals_sep = COND #(
      WHEN number_format EQ ztbox_cl_csvman=>c_dec_separator_1 THEN ','
      WHEN number_format EQ ztbox_cl_csvman=>c_dec_separator_2 THEN '.'
      WHEN number_format EQ ztbox_cl_csvman=>c_dec_separator_3 THEN ',' ).

  ENDMETHOD.


  METHOD _WRITE_TO_STR.

    output = write_numb( value ).

  ENDMETHOD.
ENDCLASS.
