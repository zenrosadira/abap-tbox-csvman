class ZTBOX_CL_CSVFIELD definition
  public
  create public

  global friends ZTBOX_CL_CSVMAN .

public section.

  interfaces ZTBOX_IF_CSVMAN .

  aliases TY_CONF
    for ZTBOX_IF_CSVMAN~TY_CONF .

  constants C_DEC_SEPARATOR_3 type XUDCPFM value 'Y' ##NO_TEXT.
  constants C_DEC_SEPARATOR_2 type XUDCPFM value 'X' ##NO_TEXT.
  constants C_DEC_SEPARATOR_1 type XUDCPFM value '' ##NO_TEXT.

  class-methods CLASS_CONSTRUCTOR .
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
      ZCX_TBOX_CSVMAN .
  methods ASSIGN_ELEMENT
    importing
      !ELEM_DESC type ref to CL_ABAP_ELEMDESCR .
  methods COUNTRY
    importing
      !COUNTRY type LAND1
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods COPY
    importing
      !I_OBJ type ref to ZTBOX_CL_CSVFIELD .
  methods ALIGNMENT
    importing
      !ALIGNMENT type I
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods CSV_POSITION
    importing
      !I_PAR type I
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods KEEP_INIT
    importing
      !KEEP_INIT type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods LABEL
    importing
      !LABEL type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods EXCLUDE
    importing
      !EXCLUDE type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods INCLUDE
    importing
      !INCLUDE type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods CONDENSE
    importing
      !CONDENSE type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods OUTPUT_LEN
    importing
      !OUTPUT_LEN type I
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods CONVEXIT
    importing
      !CONVEXIT type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods DECIMALS
    importing
      !DECIMALS type I
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods ESCAPECHAR
    importing
      !ESCAPECHAR type CHAR1
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods QUOTECHAR
    importing
      !QUOTECHAR type CHAR1
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods CONFIG
    importing
      !I_CONFIG type TY_CONF .
  methods TO_CHAR
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD_CHAR .
  methods TO_NUMB
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD_NUMB .
  methods TO_TIME
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD_TIME .
  methods TO_DATE
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD_DATE .
  methods NUMBER_FORMAT
    importing
      !NUMBER_FORMAT type XUDCPFM
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD_NUMB .
  methods TIME_FORMAT
    importing
      !TIME_FORMAT type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD_TIME .
  methods DATE_FORMAT
    importing
      !DATE_FORMAT type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD_DATE .
  methods IS_MANDATORY
    importing
      !VALUE type STRING
    returning
      value(FAIL) type FLAG .
PROTECTED SECTION.

  ALIASES _conf
    FOR ztbox_if_csvman~_conf .

  DATA _char_ref TYPE REF TO data .
  DATA _outlen TYPE i .
  DATA _label TYPE string .
  DATA _include TYPE flag .
  DATA _exclude TYPE flag .
  DATA _csv_position TYPE i .
  DATA _convexit_routine TYPE convexit .
  DATA _elem TYPE REF TO cl_abap_elemdescr .
  CLASS-DATA _t005x TYPE TABLE OF t005x.

  METHODS _write_to_str
    IMPORTING
      !value        TYPE any
    RETURNING
      VALUE(output) TYPE string .
  METHODS _add_transformation
    IMPORTING
      !method_name TYPE seocmpname
      !order       TYPE i OPTIONAL .
  METHODS convexit_output
    CHANGING
      !value TYPE string .
  METHODS output
    CHANGING
      !value TYPE string .
  METHODS _reformat .
private section.

  aliases TY_TRANSFORMATIONS
    for ZTBOX_IF_CSVMAN~TY_TRANSFORMATIONS .
  aliases TY_TRANSFORMATIONS_T
    for ZTBOX_IF_CSVMAN~TY_TRANSFORMATIONS_T .
  aliases TY_VALIDATIONS
    for ZTBOX_IF_CSVMAN~TY_VALIDATIONS .
  aliases TY_VALIDATIONS_T
    for ZTBOX_IF_CSVMAN~TY_VALIDATIONS_T .

  data _TRANSFORMATIONS type TY_TRANSFORMATIONS_T .
  data _PRE_VALIDATIONS type TY_VALIDATIONS_T .
  data _POST_VALIDATIONS type TY_VALIDATIONS_T .

  methods _SET_LABEL .
  methods _SET_CHAR_REF .
  methods _SET_OUTLEN .
  methods _SET_CONVEXIT_ROUTINE .
ENDCLASS.



CLASS ZTBOX_CL_CSVFIELD IMPLEMENTATION.


  METHOD ADD_POST_VALIDATION.

    APPEND VALUE #(
      object      = COND #( WHEN check_object IS SUPPLIED THEN check_object ELSE me )
      validation  = check_method ) TO _post_validations.

  ENDMETHOD.


  METHOD ADD_PRE_VALIDATION.

    APPEND VALUE #(
      object      = COND #( WHEN check_object IS SUPPLIED THEN check_object ELSE me )
      validation  = check_method ) TO _pre_validations.

  ENDMETHOD.


  METHOD ALIGNMENT.

    _conf-alignment = alignment.

    r_res = me.

  ENDMETHOD.


  METHOD ASSIGN_ELEMENT.

    _elem = elem_desc.

    _outlen = _elem->output_length.
    _set_label( ).
    _set_char_ref( ).
    _set_convexit_routine( ).

  ENDMETHOD.


  METHOD class_constructor.

    SELECT * FROM t005x INTO TABLE @_t005x.

  ENDMETHOD.


  METHOD CONDENSE.

    _conf-condense = condense.

    r_res = me.

  ENDMETHOD.


  METHOD config.

    _conf = i_config.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    _add_transformation( |OUTPUT| ).

  ENDMETHOD.


  METHOD CONVEXIT.

    _conf-convexit = convexit.

    r_res = me.

  ENDMETHOD.


  METHOD CONVEXIT_OUTPUT.

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

    _conf               = i_obj->_conf.

    _outlen             = i_obj->_outlen.
    _label              = i_obj->_label.

    _include            = i_obj->_include.
    _exclude            = i_obj->_exclude.

    _csv_position       = i_obj->_csv_position.

    APPEND LINES OF i_obj->_pre_validations   TO _pre_validations.
    APPEND LINES OF i_obj->_post_validations  TO _post_validations.

    _reformat( ).

  ENDMETHOD.


  METHOD country.

    _conf-country = country.

    CLEAR _conf-use_number_format.

    r_res = me.

  ENDMETHOD.


  METHOD CSV_POSITION.

    CHECK _csv_position IS INITIAL.

    _csv_position = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD DATE_FORMAT.

    _conf-date_format = date_format.

    r_res = to_date( ).

  ENDMETHOD.


  METHOD DECIMALS.

    _conf-decimals = decimals.

    r_res = me.

  ENDMETHOD.


  METHOD ESCAPECHAR.

    _conf-escapechar = escapechar.

    r_res = me.

  ENDMETHOD.


  METHOD EXCLUDE.

    _exclude = exclude.

    r_res = me.

  ENDMETHOD.


  METHOD INCLUDE.

    _include = include.

    r_res = me.

  ENDMETHOD.


  METHOD IS_MANDATORY.

    fail = xsdbool( value IS INITIAL ).

  ENDMETHOD.


  METHOD KEEP_INIT.

    _conf-keep_init = keep_init.

    r_res = me.

  ENDMETHOD.


  METHOD LABEL.

    _label = label.

    r_res = me.

  ENDMETHOD.


  METHOD number_format.

    _conf-use_number_format = abap_true.
    _conf-number_format     = number_format.

    r_res = to_numb( ).

  ENDMETHOD.


  METHOD output.

    IF _conf-condense EQ abap_true.
      CONDENSE value.
    ENDIF.

    IF _conf-convexit EQ abap_true AND _convexit_routine IS NOT INITIAL.
      convexit_output( CHANGING value = value ).
    ENDIF.

  ENDMETHOD.


  METHOD OUTPUT_LEN.

    _outlen = output_len.

    r_res = me.

  ENDMETHOD.


  METHOD QUOTECHAR.

    _conf-quotechar = quotechar.

    r_res = me.

  ENDMETHOD.


  METHOD READ.

    LOOP AT _pre_validations ASSIGNING FIELD-SYMBOL(<pre_val>).

      CALL METHOD <pre_val>-object->(<pre_val>-validation) EXPORTING value = value RECEIVING fail = <pre_val>-failed.

      IF <pre_val>-failed EQ abap_true.
        RAISE EXCEPTION TYPE zcx_tbox_csvman
          EXPORTING
            method_name = <pre_val>-validation
            sap_value   = output.
      ENDIF.

    ENDLOOP.

    output = value.
    SORT _transformations BY order ASCENDING.
    LOOP AT _transformations INTO DATA(trans).

      CALL METHOD trans-object->(trans-transf) CHANGING value = output.

    ENDLOOP.

    LOOP AT _post_validations ASSIGNING FIELD-SYMBOL(<post_val>).

      CALL METHOD <post_val>-object->(<post_val>-validation) EXPORTING value = output RECEIVING fail = <post_val>-failed.

      IF <post_val>-failed EQ abap_true.
        RAISE EXCEPTION TYPE zcx_tbox_csvman
          EXPORTING
            method_name = <post_val>-validation
            sap_value   = output.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD TIME_FORMAT.

    _conf-time_format = time_format.

    r_res = to_time( ).

  ENDMETHOD.


  METHOD TO_CHAR.

    r_res = NEW #( ).

    r_res->copy( me ).

  ENDMETHOD.


  METHOD TO_DATE.

    r_res = NEW #( ).

    r_res->copy( me ).

  ENDMETHOD.


  METHOD TO_NUMB.

    r_res = new #( ).

    r_res->copy( me ).

  ENDMETHOD.


  METHOD TO_TIME.

    r_res = NEW #( ).

    r_res->copy( me ).

  ENDMETHOD.


  METHOD write.

    output = _write_to_str( value ).

    IF _conf-alignment NE cl_abap_format=>a_left.

      ASSIGN _char_ref->* TO FIELD-SYMBOL(<val_c>).

      CLEAR <val_c>.
      <val_c> = |{ output WIDTH = _outlen ALIGN = (_conf-alignment) }|.

      output = <val_c>.

    ENDIF.

    IF _conf-condense EQ abap_true.
      CONDENSE output.
    ENDIF.

  ENDMETHOD.


  METHOD _ADD_TRANSFORMATION.

    APPEND VALUE #(
      order   = COND #( WHEN order IS SUPPLIED THEN order ELSE lines( _transformations ) + 1 )
      object  = me
      transf  = method_name ) TO _transformations.

  ENDMETHOD.


  method _REFORMAT.
  endmethod.


  METHOD _SET_CHAR_REF.

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


  METHOD _SET_CONVEXIT_ROUTINE.

    CHECK _elem->is_ddic_type( ).

    DATA(dfies) = _elem->get_ddic_field( sy-langu ).

    _convexit_routine = dfies-convexit.

  ENDMETHOD.


  METHOD _SET_LABEL.

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


  METHOD _SET_OUTLEN.

    CHECK _outlen IS INITIAL.

    _outlen = _elem->output_length.

  ENDMETHOD.


  METHOD _WRITE_TO_STR.
  ENDMETHOD.
ENDCLASS.
