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
  methods CONVEXIT
    importing
      !I_PAR type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods COUNTRY_FORMAT
    importing
      !I_PAR type LAND1
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
  methods ENCLOSING
    importing
      !I_PAR type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
protected section.
private section.

  types:
    BEGIN OF ty_field_config,
      enclosing      TYPE string,
      convexit       TYPE flag,
      alignment      TYPE i,
      keep_initials  TYPE flag,
      country_format TYPE land1,
      order          TYPE i,
      include        TYPE flag,
      exclude        TYPE flag,
      type_cat       TYPE abap_typekind,
      outlen         TYPE i,
      c_ref          TYPE REF TO data,
    END OF ty_field_config .

  data _FIELD_CONFIG type TY_FIELD_CONFIG .
  data _NAME type FELD_NAME .
  data _CSV type ref to ZTBOX_CL_CSVMAN .

  methods CONSTRUCTOR
    importing
      !I_NAME type FELD_NAME
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
  methods GET_VALUE
    importing
      !I_VAL type ANY
    returning
      value(R_RES) type STRING .
ENDCLASS.



CLASS ZTBOX_CL_CSVMAN_FIELD IMPLEMENTATION.


  METHOD alignment.

    _field_config-alignment = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD char_reference.

    DATA(dyn_type) = cl_abap_elemdescr=>get_c(
      COND #(
        WHEN i_elem_desc->output_length IS NOT INITIAL
          THEN i_elem_desc->output_length
        ELSE cl_abap_elemdescr=>type_c_max_length ) ).

    CREATE DATA _field_config-c_ref TYPE HANDLE dyn_type.

  ENDMETHOD.


  METHOD constructor.

    _name = i_name.
    _csv  = i_csv.

    init_config( ).

  ENDMETHOD.


  METHOD convexit.

    _field_config-convexit = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD country_format.

    _field_config-country_format = i_par.

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


  method GET_VALUE.
  endmethod.


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

  ENDMETHOD.


  METHOD keep_init.

    _field_config-keep_initials = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD order.

    _field_config-order = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD outlen.

    _field_config-outlen = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD type_category.

    _field_config-type_cat = i_type_kind.

    r_res = me.

  ENDMETHOD.
ENDCLASS.
