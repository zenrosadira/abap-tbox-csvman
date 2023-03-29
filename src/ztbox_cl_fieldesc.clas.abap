class ZTBOX_CL_FIELDESC definition
  public
  create public

  global friends ZTBOX_CL_CSVMAN .

public section.

  constants C_DEC_SEPARATOR_3 type XUDCPFM value 'Y' ##NO_TEXT.
  constants C_DEC_SEPARATOR_2 type XUDCPFM value 'X' ##NO_TEXT.
  constants C_DEC_SEPARATOR_1 type XUDCPFM value '' ##NO_TEXT.

  methods ADD_POST_VALIDATION
    importing
      !CHECK_OBJECT type ref to OBJECT optional
      !CHECK_METHOD type SEOCMPNAME .
  methods CONSTRUCTOR .
  methods ADD_PRE_VALIDATION
    importing
      !CHECK_OBJECT type ref to OBJECT optional
      !CHECK_METHOD type SEOCMPNAME .
  methods WRITE
    importing
      !VALUE type ANY
    returning
      value(OUTPUT) type STRING .
  methods READ
    importing
      !VALUE type STRING
    returning
      value(OUTPUT) type STRING
    raising
      ZCX_TBOX_FIELDESC .
  methods ASSIGN_ELEMENT
    importing
      !ELEM_DESC type ref to CL_ABAP_ELEMDESCR .
  methods COUNTRY
    importing
      !COUNTRY type LAND1
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods COPY
    importing
      !I_OBJ type ref to ZTBOX_CL_FIELDESC .
  methods ALIGNMENT
    importing
      !ALIGNMENT type I
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods CSV_POSITION
    importing
      !I_PAR type I
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods KEEP_INIT
    importing
      !KEEP_INIT type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods LABEL
    importing
      !LABEL type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods EXCLUDE
    importing
      !EXCLUDE type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods INCLUDE
    importing
      !INCLUDE type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods CONDENSE
    importing
      !CONDENSE type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods OUTPUT_LEN
    importing
      !OUTPUT_LEN type I
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods CONVEXIT
    importing
      !CONVEXIT type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods DECIMALS
    importing
      !DECIMALS type I
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods ESCAPECHAR
    importing
      !ESCAPECHAR type CHAR1
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods QUOTECHAR
    importing
      !QUOTECHAR type CHAR1
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods CONFIG
    importing
      !I_CONFIG type ZTBOX_CL_CSVMAN=>TY_FIELD_CONF .
  methods TO_CHAR
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC_CHAR .
  methods TO_NUMB
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC_NUMB .
  methods TO_TIME
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC_TIME .
  methods TO_DATE
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC_DATE .
  methods NUMBER_FORMAT
    importing
      !NUMBER_FORMAT type XUDCPFM
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC_NUMB .
  methods TIME_FORMAT
    importing
      !TIME_FORMAT type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC_TIME .
  methods DATE_FORMAT
    importing
      !DATE_FORMAT type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC_DATE .
protected section.

  data _CHAR_REF type ref to DATA .
  data _COUNTRY type LAND1 .
  data _OUTLEN type I .
  data _LABEL type STRING .
  data _ESCAPECHAR type CHAR1 .
  data _DOUBLEQUOTE type FLAG .
  data _DELIMITER type CHAR1 .
  data _QUOTECHAR type CHAR1 .
  data _CONVEXIT type FLAG .
  data _CONDENSE type FLAG .
  data _QUOTING type I .
  data _ALIGNMENT type I .
  data _KEEP_INIT type FLAG .
  data _INCLUDE type FLAG .
  data _EXCLUDE type FLAG .
  data _DATE_FORMAT_STR type STRING .
  data _TIME_FORMAT_STR type STRING .
  data _NUMBER_FORMAT type XUDCPFM .
  data _CSV_POSITION type I .
  data _CONVEXIT_ROUTINE type CONVEXIT .
  data _DECIMALS type I .
  data _USE_NUMBER_FORMAT type FLAG .

  methods _WRITE_TO_STR
    importing
      !VALUE type ANY
    returning
      value(OUTPUT) type STRING .
  methods _ADD_TRANSFORMATION
    importing
      !METHOD_NAME type SEOCMPNAME
      !ORDER type I optional .
  methods CONVEXIT_OUTPUT
    changing
      !VALUE type STRING .
  methods OUTPUT
    changing
      !VALUE type STRING .
PRIVATE SECTION.

  TYPES:
    BEGIN OF ty_validations,
      object     TYPE REF TO object,
      validation TYPE seocmpname,
      failed     TYPE flag,
    END OF ty_validations .
  TYPES:
    BEGIN OF ty_transformations,
      order  TYPE i,
      transf TYPE seocmpname,
    END OF ty_transformations .

  DATA _elem TYPE REF TO cl_abap_elemdescr .
  DATA:
    _transformations TYPE TABLE OF ty_transformations WITH DEFAULT KEY .
  DATA:
    _pre_validations TYPE TABLE OF ty_validations WITH KEY validation .
  DATA:
    _post_validations TYPE TABLE OF ty_validations WITH KEY validation .

  METHODS _set_label .
  METHODS _set_char_ref .
  METHODS _set_outlen .
  METHODS _set_convexit_routine .
ENDCLASS.



CLASS ZTBOX_CL_FIELDESC IMPLEMENTATION.


  METHOD add_post_validation.

    APPEND VALUE #(
      object      = check_object
      validation  = check_method ) TO _post_validations.

  ENDMETHOD.


  METHOD add_pre_validation.

    APPEND VALUE #(
      object      = check_object
      validation  = check_method ) TO _pre_validations.

  ENDMETHOD.


  METHOD alignment.

    _alignment = alignment.

    r_res = me.

  ENDMETHOD.


  METHOD assign_element.

    _elem = elem_desc.

    _outlen = _elem->output_length.
    _set_label( ).
    _set_char_ref( ).
    _set_convexit_routine( ).

  ENDMETHOD.


  METHOD CONDENSE.

    _condense = condense.

    r_res = me.

  ENDMETHOD.


  METHOD config.

    convexit( i_config-convexit ).
    quotechar( i_config-quotechar ).
    escapechar( i_config-escapechar ).
    condense( i_config-condense ).
    keep_init( i_config-keep_init ).
    alignment( i_config-alignment ).
    country( i_config-country ).
    decimals( i_config-decimals ).

    _delimiter          = i_config-delimiter.
    _doublequote        = i_config-doublequote.
    _quoting            = i_config-quoting.
    _use_number_format  = i_config-use_number_format.
    _number_format      = i_config-number_format.

  ENDMETHOD.


  METHOD constructor.

    _add_transformation( |OUTPUT| ).

  ENDMETHOD.


  METHOD convexit.

    _convexit = convexit.

    r_res = me.

  ENDMETHOD.


  METHOD convexit_output.

    CHECK _elem->is_ddic_type( ).

    DATA(dfies) = _elem->get_ddic_field( sy-langu ).

    CHECK dfies-convexit IS NOT INITIAL.

    cl_rsan_ut_conversion_exit=>convert_to_intern(
      EXPORTING
        i_fieldinfo       = CORRESPONDING #( dfies )
        i_external_value  = value
      IMPORTING
        e_internal_value  = value
      EXCEPTIONS
        failed            = 1 ).

  ENDMETHOD.


  METHOD copy.

    _country            = i_obj->_country.
    _outlen             = i_obj->_outlen.
    _label              = i_obj->_label.
    _escapechar         = i_obj->_escapechar.
    _quotechar          = i_obj->_quotechar.
    _convexit           = i_obj->_convexit.
    _condense           = i_obj->_condense.
    _alignment          = i_obj->_alignment.
    _keep_init          = i_obj->_keep_init.
    _include            = i_obj->_include.
    _exclude            = i_obj->_exclude.
    _date_format_str    = i_obj->_date_format_str.
    _time_format_str    = i_obj->_time_format_str.
    _number_format      = i_obj->_number_format.
    _decimals           = i_obj->_decimals.
    _csv_position       = i_obj->_csv_position.
    _delimiter          = i_obj->_delimiter.
    _doublequote        = i_obj->_doublequote.
    _use_number_format  = i_obj->_use_number_format.
    _quoting            = i_obj->_quoting.
    _post_validations   = i_obj->_post_validations.
    _pre_validations    = i_obj->_pre_validations.

  ENDMETHOD.


  METHOD country.

    _country = country.

    r_res = me.

  ENDMETHOD.


  METHOD csv_position.

    CHECK _csv_position IS INITIAL.

    _csv_position = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD date_format.

    _date_format_str = date_format.

    r_res = to_date( ).

  ENDMETHOD.


  METHOD DECIMALS.

    _decimals = decimals.

    r_res = me.

  ENDMETHOD.


  METHOD escapechar.

    _escapechar = escapechar.

    r_res = me.

  ENDMETHOD.


  METHOD exclude.

    _exclude = exclude.

    r_res = me.

  ENDMETHOD.


  METHOD INCLUDE.

    _include = include.

    r_res = me.

  ENDMETHOD.


  METHOD keep_init.

    _keep_init = keep_init.

    r_res = me.

  ENDMETHOD.


  METHOD LABEL.

    _label = label.

    r_res = me.

  ENDMETHOD.


  METHOD number_format.

    _use_number_format = abap_true.

    _number_format = number_format.

    r_res = to_numb( ).

  ENDMETHOD.


  METHOD output.

    IF _condense EQ abap_true.
      CONDENSE value.
    ENDIF.

    IF _convexit EQ abap_true AND _convexit_routine IS NOT INITIAL.
      convexit_output( CHANGING value = value ).
    ENDIF.

  ENDMETHOD.


  METHOD output_len.

    _outlen = output_len.

    r_res = me.

  ENDMETHOD.


  METHOD QUOTECHAR.

    _quotechar = quotechar.

    r_res = me.

  ENDMETHOD.


  METHOD read.

    LOOP AT _pre_validations ASSIGNING FIELD-SYMBOL(<pre_val>).

      CALL METHOD <pre_val>-object->(<pre_val>-validation) EXPORTING value = value RECEIVING fail = <pre_val>-failed.

      IF <pre_val>-failed EQ abap_true.
        RAISE EXCEPTION TYPE zcx_tbox_fieldesc
          EXPORTING
            method    = <pre_val>-validation
            sap_value = output.
      ENDIF.

    ENDLOOP.

    output = value.
    SORT _transformations BY order ASCENDING.
    LOOP AT _transformations INTO DATA(trans).

      CALL METHOD (trans-transf) CHANGING value = output.

    ENDLOOP.

    LOOP AT _post_validations ASSIGNING FIELD-SYMBOL(<post_val>).

      CALL METHOD <post_val>-object->(<post_val>-validation) EXPORTING value = output RECEIVING fail = <post_val>-failed.

      IF <post_val>-failed EQ abap_true.
        RAISE EXCEPTION TYPE zcx_tbox_fieldesc
          EXPORTING
            method    = <post_val>-validation
            sap_value = output.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD time_format.

    _time_format_str = time_format.

    r_res = to_time( ).

  ENDMETHOD.


  METHOD to_char.

    r_res = NEW #( ).

    r_res->copy( me ).

  ENDMETHOD.


  METHOD to_date.

    r_res = NEW #( ).

    r_res->copy( me ).

  ENDMETHOD.


  METHOD TO_NUMB.

    r_res = new #( ).

    r_res->copy( me ).

  ENDMETHOD.


  METHOD to_time.

    r_res = NEW #( ).

    r_res->copy( me ).

  ENDMETHOD.


  METHOD write.

    output = _write_to_str( value ).

    IF _alignment NE cl_abap_format=>a_left.

      ASSIGN _char_ref->* TO FIELD-SYMBOL(<val_c>).

      CLEAR <val_c>.
      <val_c> = |{ output WIDTH = _outlen ALIGN = (_alignment) }|.

      output = <val_c>.

    ENDIF.

    IF _condense EQ abap_true.
      CONDENSE output.
    ENDIF.

*    IF _escapechar IS INITIAL AND _doublequote EQ abap_true AND _quotechar IS NOT INITIAL.
*      output = replace( val = output occ = 0 sub = _quotechar with = _quotechar && _quotechar ).
*    ENDIF.
*
*    IF _escapechar IS NOT INITIAL AND _quotechar IS NOT INITIAL.
*      output = replace( val = output occ = 0 sub = _quotechar with = _escapechar && _quotechar ).
*    ENDIF.
*
*    IF _escapechar IS NOT INITIAL AND ( _quotechar IS INITIAL OR _quoting EQ ztbox_cl_csvman=>c_quote_none ).
*      output = replace( val = output occ = 0 sub = _delimiter with = _escapechar && _delimiter ).
*    ENDIF.

  ENDMETHOD.


  METHOD _add_transformation.

    APPEND VALUE #(
      order   = COND #( WHEN order IS SUPPLIED THEN order ELSE lines( _transformations ) + 1 )
      transf  = method_name ) TO _transformations.

  ENDMETHOD.


  METHOD _set_char_ref.

    CLEAR _char_ref.

    TRY.
        DATA(dyn_type) = cl_abap_elemdescr=>get_c(
          COND #(
            WHEN _outlen IS NOT INITIAL THEN _outlen
            ELSE cl_abap_elemdescr=>type_c_max_length ) ).
      CATCH cx_parameter_invalid_range.
        RETURN.
    ENDTRY.

    CREATE DATA _char_ref TYPE HANDLE dyn_type.

  ENDMETHOD.


  METHOD _set_convexit_routine.

    CHECK _elem->is_ddic_type( ).

    DATA(dfies) = _elem->get_ddic_field( sy-langu ).

    _convexit_routine = dfies-convexit.

  ENDMETHOD.


  METHOD _set_label.

    CHECK _label IS INITIAL.

    _elem->get_ddic_field(
      RECEIVING
        p_flddescr = DATA(ddic_field)
      EXCEPTIONS
        no_ddic_type  = 1
        not_found     = 2 ).

    IF sy-subrc EQ 0.
      _label = ddic_field-scrtext_l.
    ENDIF.

  ENDMETHOD.


  METHOD _set_outlen.

    CHECK _outlen IS INITIAL.

    _outlen = _elem->output_length.

  ENDMETHOD.


  method _WRITE_TO_STR.
  endmethod.
ENDCLASS.
