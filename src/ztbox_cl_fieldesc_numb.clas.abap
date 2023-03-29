CLASS ztbox_cl_fieldesc_numb DEFINITION
  PUBLIC
  INHERITING FROM ztbox_cl_fieldesc
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
    METHODS valid_numb
      IMPORTING
        !value      TYPE string
      RETURNING
        VALUE(fail) TYPE flag .

    METHODS config
        REDEFINITION .
  PROTECTED SECTION.

    METHODS output
        REDEFINITION .
    METHODS _write_to_str
        REDEFINITION .
  PRIVATE SECTION.

    DATA _thousand_sep TYPE char1 .
    DATA _decimals_sep TYPE char1 .

    METHODS write_numb
      IMPORTING
        !value        TYPE numeric
      RETURNING
        VALUE(output) TYPE string .
ENDCLASS.



CLASS ZTBOX_CL_FIELDESC_NUMB IMPLEMENTATION.


  METHOD config.

    super->config( i_config ).

    _thousand_sep = COND #(
      WHEN i_config-number_format EQ ztbox_cl_csvman=>c_dec_separator_1 THEN '.'
      WHEN i_config-number_format EQ ztbox_cl_csvman=>c_dec_separator_2 THEN ','
      WHEN i_config-number_format EQ ztbox_cl_csvman=>c_dec_separator_3 THEN ' ' ).

    _decimals_sep = COND #(
      WHEN i_config-number_format EQ ztbox_cl_csvman=>c_dec_separator_1 THEN ','
      WHEN i_config-number_format EQ ztbox_cl_csvman=>c_dec_separator_2 THEN '.'
      WHEN i_config-number_format EQ ztbox_cl_csvman=>c_dec_separator_3 THEN ',' ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    add_post_validation(
      check_object  = me
      check_method  = |VALID_NUMB| ).

  ENDMETHOD.


  METHOD output.

    super->output( CHANGING value = value ).

    IF _convexit EQ abap_true AND _convexit_routine IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF _thousand_sep IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF  _thousand_sep IN value WITH space.
    ENDIF.

    IF _decimals_sep IS NOT INITIAL.
      REPLACE FIRST OCCURRENCE OF _decimals_sep IN value WITH '.'.
    ENDIF.

    CONDENSE value NO-GAPS.

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

    DATA(country) = COND land1(
      WHEN _use_number_format EQ abap_false THEN _country
      WHEN _number_format EQ ztbox_cl_csvman=>c_dec_separator_1 THEN 'IT'
      WHEN _number_format EQ ztbox_cl_csvman=>c_dec_separator_2 THEN 'US'
      WHEN _number_format EQ ztbox_cl_csvman=>c_dec_separator_3 THEN 'US' ).

    output = COND #(
      WHEN _decimals IS NOT INITIAL THEN |{ value DECIMALS = _decimals COUNTRY = country }|
      ELSE |{ value COUNTRY = country }| ).

    IF _number_format EQ ztbox_cl_csvman=>c_dec_separator_3.
      REPLACE ALL OCCURRENCES OF ',' IN output WITH ` `.
    ENDIF.

  ENDMETHOD.


  METHOD _write_to_str.

    output = write_numb( value ).

  ENDMETHOD.
ENDCLASS.
