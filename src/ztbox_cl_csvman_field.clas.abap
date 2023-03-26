class ZTBOX_CL_CSVMAN_FIELD definition
  public
  final
  create private

  global friends ZTBOX_CL_CSVMAN .

public section.

  methods ORDER
    importing
      !I_PAR type I
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods DECIMALS
    importing
      !I_PAR type I
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods ALIGNMENT
    importing
      !I_PAR type I
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods KEEP_INIT
    importing
      !I_PAR type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods EXCLUDE
    importing
      !I_PAR type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods INCLUDE
    importing
      !I_PAR type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods DECIMAL_SEP
    importing
      !I_PAR type CHAR1
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods THOUSAND_SEP
    importing
      !I_PAR type CHAR1
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods CONDENSE
    importing
      !I_PAR type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods CONVEXIT
    importing
      !I_PAR type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods TIME_FORMAT
    importing
      !I_TIME_FORMAT type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods DATE_FORMAT
    importing
      !I_DATE_FORMAT type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods COUNTRY_FORMAT
    importing
      !I_PAR type LAND1
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods LABEL
    importing
      !I_PAR type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods ENCLOSING
    importing
      !I_PAR type CHAR1
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods READ
    importing
      !VALUE type STRING
    returning
      value(OUTPUT) type STRING .
protected section.
private section.

  types:
    BEGIN OF ty_validations,
      validation TYPE seocmpname,
      failed     TYPE flag,
    END OF ty_validations .
  types:
    BEGIN OF ty_transformations,
      order  TYPE i,
      transf TYPE seocmpname,
    END OF ty_transformations .
  types:
    BEGIN OF ty_field_config,
      label          TYPE string,
      enclosing      TYPE char1,
      convexit       TYPE flag,
      condense       TYPE flag,
      alignment      TYPE i,
      keep_initials  TYPE flag,
      country_format TYPE land1,
      decimals       TYPE i,
      order          TYPE i,
      include        TYPE flag,
      exclude        TYPE flag,
      type_cat       TYPE abap_typekind,
      outlen         TYPE i,
      thousand_sep   TYPE c LENGTH 1,
      decimals_sep   TYPE c LENGTH 1,
      c_ref          TYPE REF TO data,
      elem_desc      TYPE REF TO cl_abap_elemdescr,
    END OF ty_field_config .

  data:
    BEGIN OF _date_format,
      dd_off TYPE i,
      mm_off TYPE i,
      yy_off TYPE i,
      yy_len TYPE i,
      sep    TYPE c LENGTH 1,
    END OF _date_format .
  data:
    BEGIN OF _time_format,
      hh_off TYPE i,
      mm_off TYPE i,
      ss_off TYPE i,
      sep    TYPE c LENGTH 1,
    END OF _time_format .
  data _FIELD_CONFIG type TY_FIELD_CONFIG .
  data _CSV type ref to ZTBOX_CL_CSVMAN .
  data _POSITION_SET type FLAG .
  data:
    _transformations TYPE TABLE OF ty_transformations WITH DEFAULT KEY .
  data:
    _pre_validations TYPE TABLE OF ty_validations WITH KEY validation .
  data:
    _post_validations TYPE TABLE OF ty_validations WITH KEY validation .

  methods CONSTRUCTOR
    importing
      !I_CSV type ref to ZTBOX_CL_CSVMAN .
  methods TYPE_CATEGORY
    importing
      !I_TYPE_KIND type ABAP_TYPEKIND
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods CHAR_REFERENCE
    importing
      !I_ELEM_DESC type ref to CL_ABAP_ELEMDESCR
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods INIT_CONFIG .
  methods OUTLEN
    importing
      !I_PAR type I
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods WRITE
    importing
      !I_VAL type ANY
    returning
      value(R_RES) type STRING .
  methods WRITE_TIME
    importing
      !VALUE type TIMS
    returning
      value(OUTPUT) type STRING .
  methods WRITE_DATE
    importing
      !VALUE type DATUM
    returning
      value(OUTPUT) type STRING .
  methods WRITE_NUM
    importing
      !VALUE type NUMERIC
    returning
      value(OUTPUT) type STRING .
  methods _SET_TRANSFORMATION .
  methods NUM_OUTPUT
    changing
      !VALUE type STRING .
  methods TIME_OUTPUT
    changing
      !VALUE type STRING .
  methods DATE_OUTPUT
    changing
      !VALUE type STRING .
  methods CONVEXIT_OUTPUT
    changing
      !VALUE type STRING .
  methods CONDENSE_OUTPUT
    changing
      !VALUE type STRING .
ENDCLASS.



CLASS ZTBOX_CL_CSVMAN_FIELD IMPLEMENTATION.


  METHOD alignment.

    _field_config-alignment = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD char_reference.

    _field_config-elem_desc = i_elem_desc.

    outlen( i_elem_desc->output_length ).

    i_elem_desc->get_ddic_field(
      RECEIVING
        p_flddescr = DATA(ddic_field)
      EXCEPTIONS
        no_ddic_type  = 1
        not_found     = 2 ).
    IF sy-subrc EQ 0.
      _field_config-label = ddic_field-scrtext_l.
    ENDIF.

    TRY.
        DATA(dyn_type) = cl_abap_elemdescr=>get_c(
          COND #(
            WHEN i_elem_desc->output_length IS NOT INITIAL
              THEN i_elem_desc->output_length
            ELSE cl_abap_elemdescr=>type_c_max_length ) ).
      CATCH cx_parameter_invalid_range.
        RETURN.
    ENDTRY.

    CREATE DATA _field_config-c_ref TYPE HANDLE dyn_type.

  ENDMETHOD.


  METHOD CONDENSE.

    _field_config-condense = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD CONDENSE_OUTPUT.

    CONDENSE value.

  ENDMETHOD.


  METHOD constructor.

    _csv  = i_csv.

    init_config( ).

  ENDMETHOD.


  METHOD convexit.

    _field_config-convexit = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD convexit_output.

    CHECK _field_config-elem_desc->is_ddic_type( ).

    DATA(dfies) = _field_config-elem_desc->get_ddic_field( sy-langu ).

    CHECK dfies-convexit IS NOT INITIAL.

    CONDENSE value.

    cl_rsan_ut_conversion_exit=>convert_to_intern(
      EXPORTING
        i_fieldinfo      = CORRESPONDING #( dfies )
        i_external_value  = value
      IMPORTING
        e_internal_value  = value ).

  ENDMETHOD.


  METHOD country_format.

    _field_config-country_format = i_par.

    r_res = me.

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


  METHOD date_output.

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


  METHOD DECIMALS.

    _field_config-decimals = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD decimal_sep.

    _field_config-decimals_sep = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD enclosing.

    _field_config-enclosing = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD exclude.

    _field_config-exclude = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD include.

    _csv->_configuration-include_only = abap_true.

    _field_config-include = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD init_config.

    alignment( _csv->_configuration-alignment ).
    convexit( _csv->_configuration-convexit ).
    country_format( _csv->_configuration-country_format ).
    enclosing( _csv->_configuration-enclosing ).
    keep_init( _csv->_configuration-keep_initials ).
    date_format( _csv->_configuration-date_format ).
    time_format( _csv->_configuration-time_format ).
    decimal_sep( _csv->_configuration-decimal_sep ).
    thousand_sep( _csv->_configuration-thousand_sep ).
    condense( _csv->_configuration-condense ).

  ENDMETHOD.


  METHOD keep_init.

    _field_config-keep_initials = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD LABEL.

    _field_config-label = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD num_output.

    IF _field_config-thousand_sep IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF  _field_config-thousand_sep IN value WITH space.
    ENDIF.

    IF _field_config-decimals_sep IS NOT INITIAL.
      REPLACE FIRST OCCURRENCE OF _field_config-decimals_sep IN value WITH '.'.
    ENDIF.

    CONDENSE value NO-GAPS.

  ENDMETHOD.


  METHOD order.

    CHECK _position_set IS INITIAL.

    _field_config-order = i_par.

    r_res = me.

    _position_set = abap_true.

  ENDMETHOD.


  METHOD outlen.

    _field_config-outlen = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD READ.

    LOOP AT _pre_validations ASSIGNING FIELD-SYMBOL(<pre_val>).

      CALL METHOD (<pre_val>-validation) EXPORTING value = value RECEIVING fail = <pre_val>-failed.

      IF <pre_val>-failed EQ abap_true.
        RETURN.
      ENDIF.

    ENDLOOP.

    output = value.
    LOOP AT _transformations INTO DATA(trans).

      CALL METHOD (trans-transf) CHANGING value = output.

    ENDLOOP.

    LOOP AT _post_validations ASSIGNING FIELD-SYMBOL(<post_val>).

      CALL METHOD (<post_val>-validation) EXPORTING value = output RECEIVING fail = <post_val>-failed.

      IF <post_val>-failed EQ abap_true.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD thousand_sep.

    _field_config-thousand_sep = i_par.

    r_res = me.

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


  METHOD time_output.

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


  METHOD type_category.

    _field_config-type_cat = i_type_kind.

    r_res = me.

  ENDMETHOD.


  METHOD WRITE.

    DATA(val) = VALUE string( ).

    ASSIGN _field_config-c_ref->* TO FIELD-SYMBOL(<val_c>).
    CLEAR <val_c>.

    IF i_val IS NOT INITIAL OR _field_config-keep_initials EQ abap_true.

      CASE _field_config-type_cat.

        WHEN cl_abap_typedescr=>typekind_packed.

          val = write_num( i_val ).

        WHEN cl_abap_typedescr=>typekind_date.

          val = write_date( i_val ).

        WHEN cl_abap_typedescr=>typekind_time.

          val = write_time( i_val ).

        WHEN OTHERS.

          WRITE i_val TO <val_c>.
          val = COND #( WHEN _field_config-convexit EQ abap_true THEN <val_c> ELSE i_val ).

          IF _csv->_configuration-escapator IS NOT INITIAL.
            REPLACE ALL OCCURRENCES OF _csv->_configuration-enclosing IN val WITH |{ _csv->_configuration-escapator }{ _csv->_configuration-enclosing }|.
          ENDIF.

      ENDCASE.

      CLEAR <val_c>.
      <val_c> = |{ val WIDTH = _field_config-outlen ALIGN = (_field_config-alignment) }|.

    ENDIF.

    CLEAR r_res.
    r_res = COND #(
      WHEN _csv->_configuration-enclosing IS NOT INITIAL
        THEN |{ _field_config-enclosing }{ <val_c> }{ _field_config-enclosing }|
      ELSE <val_c> ).

  ENDMETHOD.


  METHOD WRITE_DATE.

    DATA c_date TYPE c LENGTH 10 VALUE '..........'.

    output = |{ value COUNTRY = _field_config-country_format }|.

    CHECK _date_format IS NOT INITIAL.

    c_date+_date_format-dd_off(2) = value+6(2).
    c_date+_date_format-mm_off(2) = value+4(2).
    c_date+_date_format-yy_off(_date_format-yy_len) = COND #( WHEN _date_format-yy_len EQ 4 THEN value(4) ELSE value+2(2) ).

    REPLACE ALL OCCURRENCES OF '.' IN c_date WITH _date_format-sep.

    CLEAR output.
    output = c_date.

  ENDMETHOD.


  METHOD write_num.

    IF _field_config-decimals IS NOT INITIAL.

      DATA c_num TYPE c LENGTH cl_abap_elemdescr=>type_c_max_length.
      WRITE value TO c_num DECIMALS _field_config-decimals LEFT-JUSTIFIED.
      output = c_num.

    ELSE.

      output = |{ value COUNTRY = _field_config-country_format }|.

    ENDIF.

    CONDENSE output.

  ENDMETHOD.


  METHOD write_time.

    DATA c_time TYPE c LENGTH 8 VALUE '........'.

    output = |{ value COUNTRY = _field_config-country_format }|.

    CHECK _time_format IS NOT INITIAL.

    c_time+_time_format-hh_off(2) = value(2).
    c_time+_time_format-mm_off(2) = value+2(2).
    c_time+_time_format-ss_off(2) = value+4(2).

    REPLACE ALL OCCURRENCES OF '.' IN c_time WITH _time_format-sep.

    CLEAR output.
    output = c_time.

  ENDMETHOD.


  METHOD _set_transformation.

    IF _field_config-condense EQ abap_true.
      APPEND VALUE #( order = lines( _transformations ) + 1 transf = |CONDENSE_OUTPUT| ) TO _transformations.
    ENDIF.

    CASE _field_config-type_cat.

      WHEN cl_abap_typedescr=>typekind_date.
      APPEND VALUE #( order = lines( _transformations ) + 1 transf = |DATE_OUTPUT| ) TO _transformations.

      WHEN cl_abap_typedescr=>typekind_time.
      APPEND VALUE #( order = lines( _transformations ) + 1 transf = |TIME_OUTPUT| ) TO _transformations.

      WHEN cl_abap_typedescr=>typekind_packed.
      APPEND VALUE #( order = lines( _transformations ) + 1 transf = |NUM_OUTPUT| ) TO _transformations.

    ENDCASE.

    IF _field_config-convexit EQ abap_true.
      APPEND VALUE #( order = lines( _transformations ) + 1 transf = |CONVEXIT| ) TO _transformations.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
