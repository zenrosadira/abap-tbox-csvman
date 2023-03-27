class ZTBOX_CL_FIELDESC_DATE definition
  public
  inheriting from ZTBOX_CL_FIELDESC
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods VALID_DATE
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
private section.

  data:
    BEGIN OF _date_format,
          dd_off TYPE i,
          mm_off TYPE i,
          yy_off TYPE i,
          yy_len TYPE i,
          sep    TYPE c LENGTH 1,
        END OF _date_format .

  methods WRITE_DATE
    importing
      !VALUE type DATUM
    returning
      value(OUTPUT) type STRING .
ENDCLASS.



CLASS ZTBOX_CL_FIELDESC_DATE IMPLEMENTATION.


  METHOD config.

    super->config( i_config ).

    date_format( i_config-date_format ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    _add_post_validation( |VALID_DATE| ).

  ENDMETHOD.


  METHOD date_format.

    CHECK i_date_format IS NOT INITIAL.

    FIND |dd|   IN i_date_format MATCH OFFSET DATA(d_off)   IGNORING CASE.
    FIND |mm|   IN i_date_format MATCH OFFSET DATA(m_off)   IGNORING CASE.
    FIND |yy|   IN i_date_format MATCH OFFSET DATA(y2_off)  IGNORING CASE.
    FIND |yyyy| IN i_date_format MATCH OFFSET DATA(y4_off)  MATCH COUNT DATA(y4_cnt) IGNORING CASE.

    DATA(sep_off) = nmin( val1 = d_off val2 = m_off ) + 2.

    _date_format = VALUE #(
      sep     = i_date_format+sep_off(1)
      dd_off  = d_off
      mm_off  = m_off
      yy_off  = COND #( WHEN y4_cnt > 0 THEN y4_off ELSE y2_off )
      yy_len  = COND #( WHEN y4_cnt > 0 THEN 4 ELSE 2 ) ).

    r_res = me.

  ENDMETHOD.


  METHOD output.

    super->output( CHANGING value = value ).

    IF _convexit EQ abap_true AND _convexit_routine IS NOT INITIAL.
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


  METHOD valid_date.

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


  METHOD write_date.

    DATA c_date TYPE c LENGTH 10 VALUE '..........'.

    output = |{ value COUNTRY = _country }|.

    CHECK _date_format IS NOT INITIAL.

    c_date+_date_format-dd_off(2) = value+6(2).
    c_date+_date_format-mm_off(2) = value+4(2).
    c_date+_date_format-yy_off(_date_format-yy_len) = COND #( WHEN _date_format-yy_len EQ 4 THEN value(4) ELSE value+2(2) ).

    REPLACE ALL OCCURRENCES OF '.' IN c_date WITH _date_format-sep.

    CLEAR output.
    output = c_date.

  ENDMETHOD.


  METHOD _write_to_str.

    output = write_date( value ).

  ENDMETHOD.
ENDCLASS.
