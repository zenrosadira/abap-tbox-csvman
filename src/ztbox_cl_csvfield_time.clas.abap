class ZTBOX_CL_CSVFIELD_TIME definition
  public
  inheriting from ZTBOX_CL_CSVFIELD
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods TIME_LENGTH
    importing
      !VALUE type STRING
    returning
      value(FAIL) type FLAG .
  methods TIME_PLAUSIBILITY
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
  methods _REFORMAT
    redefinition .
private section.

  data:
    BEGIN OF _time_format,
          hh_off TYPE i,
          mm_off TYPE i,
          ss_off TYPE i,
          sep    TYPE c LENGTH 1,
        END OF _time_format .
  constants C_NULL_TIME type CHAR6 value '      ' ##NO_TEXT.

  methods WRITE_TIME
    importing
      !VALUE type UZEIT
    returning
      value(OUTPUT) type STRING .
ENDCLASS.



CLASS ZTBOX_CL_CSVFIELD_TIME IMPLEMENTATION.


  METHOD CONFIG.

    super->config( i_config ).

    time_format( i_config-time_format ).

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    super->constructor( ).

    add_pre_validation(
      check_object  = me
      check_method  = |TIME_LENGTH| ).

    add_post_validation(
      check_object  = me
      check_method  = |TIME_PLAUSIBILITY| ).

  ENDMETHOD.


  METHOD OUTPUT.

    super->output( CHANGING value = value ).

    IF _conf-convexit EQ abap_true AND _convexit_routine IS NOT INITIAL.
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


  METHOD TIME_FORMAT.

    CHECK time_format IS NOT INITIAL.

    FIND |hh|   IN time_format MATCH OFFSET DATA(h_off) IGNORING CASE.
    FIND |mm|   IN time_format MATCH OFFSET DATA(m_off) IGNORING CASE.
    FIND |ss|   IN time_format MATCH OFFSET DATA(s_off) IGNORING CASE.

    DATA(p_off) = nmin( val1 = s_off val2 = m_off ) + 2.

    _time_format = VALUE #(
      sep     = COND #( WHEN to_upper( time_format+p_off(1) ) CA sy-abcde THEN space ELSE time_format+p_off(1) )
      hh_off  = h_off
      mm_off  = m_off
      ss_off  = s_off ).

    r_res = me.

  ENDMETHOD.


  METHOD TIME_LENGTH.

    CHECK value IS NOT INITIAL.

    fail = xsdbool( strlen( value ) LE 5 ).

  ENDMETHOD.


  METHOD TIME_PLAUSIBILITY.

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


  METHOD WRITE_TIME.

    DATA c_time TYPE c LENGTH 8 VALUE '........'.

    IF _conf-country IS NOT INITIAL.
      output = |{ value COUNTRY = _conf-country }|.
      RETURN.
    ENDIF.

    CHECK _time_format IS NOT INITIAL.

    c_time = replace( val = c_time off = _time_format-hh_off len = 2 with = value(2) ).
    c_time = replace( val = c_time off = _time_format-mm_off len = 2 with = value+2(2) ).
    c_time = replace( val = c_time off = _time_format-ss_off len = 2 with = value+4(2) ).
    c_time = replace( val = c_time occ = 0 sub = ` ` with = '0' ).
    c_time = replace( val = c_time occ = 0 sub = '.' with = _time_format-sep ).

    CLEAR output.
    output = c_time.

  ENDMETHOD.


  METHOD _reformat.

    time_format( _conf-time_format ).

  ENDMETHOD.


  METHOD _WRITE_TO_STR.

    DATA time TYPE uzeit.

    time = value.

    IF time EQ c_null_time.
      CLEAR time.
    ENDIF.

    output = write_time( time ).

  ENDMETHOD.
ENDCLASS.
