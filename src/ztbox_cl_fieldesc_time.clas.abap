class ZTBOX_CL_FIELDESC_TIME definition
  public
  inheriting from ZTBOX_CL_FIELDESC
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods VALID_TIME
    importing
      !VALUE type STRING
    returning
      value(FAIL) type FLAG .

  methods CONFIG
    redefinition .
  methods TIME_FORMAT
    redefinition .
protected section.

  methods OUTPUT
    redefinition .
  methods _WRITE_TO_STR
    redefinition .
private section.

  data:
    BEGIN OF _time_format,
          hh_off TYPE i,
          mm_off TYPE i,
          ss_off TYPE i,
          sep    TYPE c LENGTH 1,
        END OF _time_format .

  methods WRITE_TIME
    importing
      !VALUE type UZEIT
    returning
      value(OUTPUT) type STRING .
ENDCLASS.



CLASS ZTBOX_CL_FIELDESC_TIME IMPLEMENTATION.


  METHOD config.

    super->config( i_config ).

    time_format( i_config-time_format ).

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    super->constructor( ).

    _add_post_validation( |VALID_TIME| ).

  ENDMETHOD.


  METHOD output.

    super->output( CHANGING value = value ).

    IF _convexit EQ abap_true AND _convexit_routine IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF value IS INITIAL.
      value = '0000000'.
      RETURN.
    ENDIF.

    DATA c_out TYPE c LENGTH 6.

    c_out(2)    = value+_time_format-hh_off.
    c_out+2(2)  = value+_time_format-mm_off.
    c_out+4(2)  = value+_time_format-ss_off.

    CLEAR value.
    value = c_out.

  ENDMETHOD.


  METHOD time_format.

    CHECK i_time_format IS NOT INITIAL.

    FIND |hh|   IN i_time_format MATCH OFFSET DATA(h_off) IGNORING CASE.
    FIND |mm|   IN i_time_format MATCH OFFSET DATA(m_off) IGNORING CASE.
    FIND |ss|   IN i_time_format MATCH OFFSET DATA(s_off) IGNORING CASE.

    DATA(p_off) = nmin( val1 = s_off val2 = m_off ) + 2.

    _time_format = VALUE #(
      sep     = i_time_format+p_off(1)
      hh_off  = h_off
      mm_off  = m_off
      ss_off  = s_off ).

    r_res = me.

  ENDMETHOD.


  METHOD valid_time.

    DATA time TYPE uzeit.

    time = value.

    CALL FUNCTION 'TIME_CHECK_PLAUSIBILITY'
      EXPORTING
        time                      = time
      EXCEPTIONS
        error_message             = -1
        plausibility_check_failed = 1
        OTHERS                    = 2.

    fail = xsdbool( sy-subrc NE 0 ).

  ENDMETHOD.


  METHOD write_time.

    DATA c_time TYPE c LENGTH 8 VALUE '........'.

    output = |{ value COUNTRY = _country }|.

    CHECK _time_format IS NOT INITIAL.

    c_time+_time_format-hh_off(2) = value(2).
    c_time+_time_format-mm_off(2) = value+2(2).
    c_time+_time_format-ss_off(2) = value+4(2).

    REPLACE ALL OCCURRENCES OF '.' IN c_time WITH _time_format-sep.

    CLEAR output.
    output = c_time.

  ENDMETHOD.


  METHOD _write_to_str.

    output = write_time( value ).

  ENDMETHOD.
ENDCLASS.
