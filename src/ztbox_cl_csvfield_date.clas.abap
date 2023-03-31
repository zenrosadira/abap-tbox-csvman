class ZTBOX_CL_CSVFIELD_DATE definition
  public
  inheriting from ZTBOX_CL_CSVFIELD
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods DATE_LENGTH
    importing
      !VALUE type STRING
    returning
      value(FAIL) type FLAG .
  methods DATE_PLAUSIBILITY
    importing
      !VALUE type STRING
    returning
      value(FAIL) type FLAG .

  methods CONFIG
    redefinition .
  methods DATE_FORMAT
    redefinition .
protected section.

  methods OUTPUT
    redefinition .
  methods _WRITE_TO_STR
    redefinition .
  methods _REFORMAT
    redefinition .
private section.

  data:
    BEGIN OF _date_format,
          dd_off TYPE i,
          mm_off TYPE i,
          yy_off TYPE i,
          yy_len TYPE i,
          sep    TYPE c LENGTH 1,
        END OF _date_format .
  data C_NULL_DATE type CHAR8 value '        ' ##NO_TEXT.

  methods WRITE_DATE
    importing
      !VALUE type DATUM
    returning
      value(OUTPUT) type STRING .
ENDCLASS.



CLASS ZTBOX_CL_CSVFIELD_DATE IMPLEMENTATION.


  METHOD CONFIG.

    super->config( i_config ).

    date_format( i_config-date_format ).

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    super->constructor( ).

    add_pre_validation(
      check_object  = me
      check_method  = |DATE_LENGTH| ).

    add_post_validation(
      check_object  = me
      check_method  = |DATE_PLAUSIBILITY| ).

  ENDMETHOD.


  METHOD DATE_FORMAT.

    CHECK date_format IS NOT INITIAL.

    FIND |dd|   IN date_format MATCH OFFSET DATA(d_off)   IGNORING CASE.
    FIND |mm|   IN date_format MATCH OFFSET DATA(m_off)   IGNORING CASE.
    FIND |yy|   IN date_format MATCH OFFSET DATA(y2_off)  IGNORING CASE.
    FIND |yyyy| IN date_format MATCH OFFSET DATA(y4_off)  MATCH COUNT DATA(y4_cnt) IGNORING CASE.

    DATA(sep_off) = nmin( val1 = d_off val2 = m_off ) + 2.

    _date_format = VALUE #(
      sep     = COND #( WHEN to_upper( date_format+sep_off(1) ) CA sy-abcde THEN space ELSE date_format+sep_off(1) )
      dd_off  = d_off
      mm_off  = m_off
      yy_off  = COND #( WHEN y4_cnt > 0 THEN y4_off ELSE y2_off )
      yy_len  = COND #( WHEN y4_cnt > 0 THEN 4 ELSE 2 ) ).

    r_res = me.

  ENDMETHOD.


  METHOD DATE_LENGTH.

    CHECK value IS NOT INITIAL.

    fail = xsdbool( strlen( value ) LE 7 ).

  ENDMETHOD.


  METHOD DATE_PLAUSIBILITY.

    DATA date TYPE datum.

    date = value.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = date
      EXCEPTIONS
        error_message             = -1
        plausibility_check_failed = 1
        OTHERS                    = 2.

    fail = xsdbool( sy-subrc NE 0 ).

  ENDMETHOD.


  METHOD OUTPUT.

    super->output( CHANGING value = value ).

    IF _conf-convexit EQ abap_true AND _convexit_routine IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF value IS INITIAL.
      value = '00000000'.
      RETURN.
    ENDIF.

    DATA c_out TYPE c LENGTH 8.

    c_out(4) = COND #(
      WHEN _date_format-yy_len EQ 4 THEN value+_date_format-yy_off(4)
      ELSE |{ sy-datum(2) }{ value+_date_format-yy_off(2) }| ).

    c_out+4(2) = value+_date_format-mm_off(2).
    c_out+6(2) = value+_date_format-dd_off(2).

    CLEAR value.
    value = c_out.

  ENDMETHOD.


  METHOD write_date.

    DATA c_date TYPE c LENGTH 10 VALUE '..........'.

    IF _conf-country IS NOT INITIAL.
      output = |{ value COUNTRY = _conf-country }|.
      RETURN.
    ENDIF.

    CHECK _date_format IS NOT INITIAL.

    DATA(dd) = value+6(2).
    DATA(mm) = value+4(2).
    DATA(yy) = COND #( WHEN _date_format-yy_len EQ 4 THEN value(4) ELSE value+2(2) ).

    c_date = replace( val = c_date off = _date_format-dd_off len = 2 with = dd ).
    c_date = replace( val = c_date off = _date_format-mm_off len = 2 with = mm ).
    c_date = replace( val = c_date off = _date_format-yy_off len = _date_format-yy_len with = yy ).
    c_date = replace( val = c_date occ = 0 sub = ` ` with = '0' ).
    c_date = replace( val = c_date occ = 0 sub = '.' with = _date_format-sep ).

    CLEAR output.
    output = c_date.

  ENDMETHOD.


  METHOD _reformat.

    date_format( _conf-date_format ).

  ENDMETHOD.


  METHOD _WRITE_TO_STR.

    DATA date TYPE datum.

    date = value.

    IF value EQ c_null_date.
      CLEAR date.
    ENDIF.

    output = write_date( date ).

  ENDMETHOD.
ENDCLASS.
