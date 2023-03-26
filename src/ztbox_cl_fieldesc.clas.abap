class ZTBOX_CL_FIELDESC definition
  public
  create public

  global friends ZTBOX_CL_CSVMAN .

public section.

  methods CONSTRUCTOR .
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
  methods COUNTRY_FORMAT
    importing
      !I_COUNTRY type LAND1 .
  class-methods CREATE
    importing
      !ELEM_DESC type ref to CL_ABAP_ELEMDESCR
    returning
      value(R_OBJ) type ref to ZTBOX_CL_FIELDESC .
  methods COPY
    importing
      !I_OBJ type ref to ZTBOX_CL_FIELDESC .
  methods VALIDATION_FAILURE
    exporting
      !E_METHOD type SEOCMPNAME
    returning
      value(R_FAIL) type FLAG .
  methods ALIGNMENT
    importing
      !I_PAR type I
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods CSV_POSITION
    importing
      !I_PAR type I
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods KEEP_INIT
    importing
      !I_PAR type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods LABEL
    importing
      !I_PAR type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods EXCLUDE
    importing
      !I_PAR type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods INCLUDE
    importing
      !I_PAR type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods CONDENSE
    importing
      !I_PAR type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods OUTPUT_LEN
    importing
      !I_PAR type I
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods CONVEXIT
    importing
      !I_PAR type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods ESCAPATOR
    importing
      !I_PAR type CHAR1
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods ENCLOSING
    importing
      !I_PAR type CHAR1
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
  methods TIME_FORMAT
    importing
      !I_TIME_FORMAT type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC_TIME .
  methods DATE_FORMAT
    importing
      !I_DATE_FORMAT type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC_DATE .
protected section.

  data _CHAR_REF type ref to DATA .
  data _COUNTRY type LAND1 .
  data _OUTLEN type I .
  data _LABEL type STRING .
  data _ESCAPATOR type CHAR1 .
  data _ENCLOSING type CHAR1 .
  data _CONVEXIT type FLAG .
  data _CONDENSE type FLAG .
  data _ALIGNMENT type I .
  data _KEEP_INIT type FLAG .
  data _INCLUDE type FLAG .
  data _EXCLUDE type FLAG .
  data _DATE_FORMAT_STR type STRING .
  data _TIME_FORMAT_STR type STRING .
  data _THOUSAND_SEP type CHAR1 .
  data _DECIMALS_SEP type CHAR1 .
  data _CSV_POSITION type I .

  methods _ADD_POST_VALIDATION
    importing
      !METHOD_NAME type SEOCMPNAME
      !STOP type FLAG optional .
  methods _ADD_PRE_VALIDATION
    importing
      !METHOD_NAME type SEOCMPNAME
      !STOP type FLAG optional .
  methods _WRITE_TO_STR
    importing
      !VALUE type ANY
    returning
      value(OUTPUT) type STRING .
  methods _ADD_TRANSFORMATION
    importing
      !METHOD_NAME type SEOCMPNAME .
  methods OUTPUT
    changing
      !VALUE type STRING .
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

  data _ELEM type ref to CL_ABAP_ELEMDESCR .
  data:
    _transformations TYPE TABLE OF ty_transformations WITH DEFAULT KEY .
  data:
    _pre_validations TYPE TABLE OF ty_validations WITH KEY validation .
  data:
    _post_validations TYPE TABLE OF ty_validations WITH KEY validation .

  methods _SET_LABEL .
  methods _SET_CHAR_REF .
  methods _SET_OUTLEN .
ENDCLASS.



CLASS ZTBOX_CL_FIELDESC IMPLEMENTATION.


  METHOD alignment.

    _alignment = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD assign_element.

    _elem = elem_desc.

    _outlen = _elem->output_length.
    _set_label( ).
    _set_char_ref( ).

  ENDMETHOD.


  METHOD CONDENSE.

    _condense = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD config.

    convexit( i_config-convexit ).
    enclosing( i_config-enclosing ).
    escapator( i_config-escapator ).
    condense( i_config-condense ).
    keep_init( i_config-keep_initials ).
    alignment( i_config-alignment ).
    country_format( i_config-country_format ).

  ENDMETHOD.


  METHOD constructor.

    _add_transformation( |OUTPUT| ).

  ENDMETHOD.


  METHOD CONVEXIT.

    _convexit = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD copy.

    _country          = i_obj->_country.
    _outlen           = i_obj->_outlen.
    _label            = i_obj->_label.
    _escapator        = i_obj->_escapator.
    _enclosing        = i_obj->_enclosing.
    _convexit         = i_obj->_convexit.
    _condense         = i_obj->_condense.
    _alignment        = i_obj->_alignment.
    _keep_init        = i_obj->_keep_init.
    _include          = i_obj->_include.
    _exclude          = i_obj->_exclude.
    _date_format_str  = i_obj->_date_format_str.
    _time_format_str  = i_obj->_time_format_str.
    _thousand_sep     = i_obj->_thousand_sep.
    _decimals_sep     = i_obj->_decimals_sep.
    _csv_position     = i_obj->_csv_position.

  ENDMETHOD.


  METHOD country_format.

    _country = i_country.

  ENDMETHOD.


  METHOD create.

    DATA(types_map) = ztbox_cl_tabdesc=>get_type_kind_map( ).

    DATA(type_cat) = VALUE #( types_map[ typekind = elem_desc->type_kind ]-type_cat OPTIONAL ).
    CHECK type_cat IS NOT INITIAL.

    r_obj = COND #(
      WHEN type_cat EQ cl_abap_elemdescr=>typekind_date   THEN NEW ztbox_cl_fieldesc_date( )
      WHEN type_cat EQ cl_abap_elemdescr=>typekind_time   THEN NEW ztbox_cl_fieldesc_time( )
      WHEN type_cat EQ cl_abap_elemdescr=>typekind_packed THEN NEW ztbox_cl_fieldesc_numb( )
      ELSE NEW ztbox_cl_fieldesc_char( ) ).

    r_obj->assign_element( elem_desc ).

  ENDMETHOD.


  METHOD CSV_POSITION.

    _csv_position = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD date_format.

    _date_format_str = i_date_format.

    r_res = to_date( ).

  ENDMETHOD.


  METHOD enclosing.

    _enclosing = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD escapator.

    _escapator = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD EXCLUDE.

    _exclude = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD INCLUDE.

    _include = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD KEEP_INIT.

    _keep_init = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD LABEL.

    _label = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD output.

    IF _condense EQ abap_true.
      CONDENSE value.
    ENDIF.

  ENDMETHOD.


  METHOD OUTPUT_LEN.

    _outlen = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD read.

    LOOP AT _pre_validations ASSIGNING FIELD-SYMBOL(<pre_val>).

      CALL METHOD (<pre_val>-validation) EXPORTING value = value RECEIVING fail = <pre_val>-failed.

      IF <pre_val>-failed EQ abap_true.
        RAISE EXCEPTION TYPE zcx_tbox_fieldesc
          EXPORTING
            method    = <pre_val>-validation
            sap_value = output.
      ENDIF.

    ENDLOOP.

    output = value.
    LOOP AT _transformations INTO DATA(trans).

      CALL METHOD (trans-transf) CHANGING value = output.

    ENDLOOP.

    LOOP AT _post_validations ASSIGNING FIELD-SYMBOL(<post_val>).

      CALL METHOD (<post_val>-validation) EXPORTING value = output RECEIVING fail = <post_val>-failed.

      IF <post_val>-failed EQ abap_true.
        RAISE EXCEPTION TYPE zcx_tbox_fieldesc
          EXPORTING
            method    = <post_val>-validation
            sap_value = output.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD time_format.

    _time_format_str = i_time_format.

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


  METHOD validation_failure.

    e_method = VALUE #( _pre_validations[ failed = abap_true ]-validation OPTIONAL ).

    IF e_method IS NOT INITIAL.
      r_fail = abap_true.
      RETURN.
    ENDIF.

    e_method = VALUE #( _post_validations[ failed = abap_true ]-validation OPTIONAL ).

    IF e_method IS NOT INITIAL.
      r_fail = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD write.

    DATA(val) = _write_to_str( value ).

    ASSIGN _char_ref->* TO FIELD-SYMBOL(<val_c>).
    CLEAR <val_c>.

    IF value IS NOT INITIAL OR _keep_init EQ abap_true.
      <val_c> = |{ val WIDTH = _outlen ALIGN = (_alignment) }|.
    ENDIF.

    output = COND #( WHEN _enclosing IS NOT INITIAL THEN |{ _enclosing }{ <val_c> }{ _enclosing }| ELSE <val_c> ).

  ENDMETHOD.


  METHOD _add_post_validation.

    APPEND VALUE #(
      validation  = method_name ) TO _post_validations.

  ENDMETHOD.


  METHOD _add_pre_validation.

    APPEND VALUE #(
      validation  = method_name ) TO _pre_validations.

  ENDMETHOD.


  METHOD _add_transformation.

    APPEND VALUE #(
      order   = lines( _transformations ) + 1
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
