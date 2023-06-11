class ZCL_TBOX_CSVFIELD definition
  public
  final
  create private

  global friends ZCL_TBOX_CSV_COMMON .

public section.

  methods EXCLUDE
    importing
      !I_ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods POSITION
    importing
      !I_POSITION type I
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods INCLUDE
    importing
      !I_ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods LABEL
    importing
      !I_LABEL type STRING
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods DECIMALS
    importing
      !I_DECIMALS type I
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods NUMBER_FORMAT
    importing
      !I_NUMBER_FORMAT type CLIKE
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods TIME_FORMAT
    importing
      !I_TIME_FORMAT type CLIKE
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods DATE_FORMAT
    importing
      !I_DATE_FORMAT type CLIKE
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods COUNTRY
    importing
      !I_COUNTRY type LAND1
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods FIELD_LENGTH
    importing
      !I_FIELD_LENGTH type I
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods KEEP_INIT
    importing
      !I_ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods ALIGNMENT
    importing
      !I_ALIGNMENT type I
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods CONV_EXIT
    importing
      !I_ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods CONDENSE_VALUES
    importing
      !I_ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZCL_TBOX_CSVFIELD .
  methods ADD_POST_VALIDATION
    importing
      !I_OBJECT_VALIDATOR type ref to OBJECT optional
      !I_METHOD_VALIDATOR type CLIKE .
  methods ADD_PRE_VALIDATION
    importing
      !I_OBJECT_VALIDATOR type ref to OBJECT optional
      !I_METHOD_VALIDATOR type CLIKE .
  methods _OVERFLOW
    importing
      !I_VALUE type STRING
    returning
      value(R_FAIL) type ABAP_BOOL .
  methods CONSTRUCTOR
    importing
      !I_CSV type ref to ZCL_TBOX_CSV_COMMON
      !I_NAME type CLIKE .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ts_validation,
        object_validator TYPE REF TO object,
        method_validator TYPE string,
      END OF ts_validation .

  data:
    m_pre_validations  TYPE STANDARD TABLE OF ts_validation .
  data:
    m_post_validations TYPE STANDARD TABLE OF ts_validation .
  data M_CSV type ref to ZCL_TBOX_CSV_COMMON .
  data M_FIELD_NAME type STRING .
  data M_BASIC_TYPE type ABAP_TYPEKIND .
  data M_FORMAT_PARAMS type ZCL_TBOX_CSV_COMMON=>TS_FORMAT_PARAMS .
  data M_LABEL type STRING .
  data M_ABORT type ABAP_BOOL .
  constants C_INITIAL_DATE type STRING value '00000000' ##NO_TEXT.
  constants C_INITIAL_TIME type STRING value '000000' ##NO_TEXT.
  constants:
    c_dec_separator_3 TYPE c LENGTH 1 value 'Y' ##NO_TEXT.
  constants:
    c_dec_separator_2 TYPE c LENGTH 1 value 'X' ##NO_TEXT.
  constants:
    c_dec_separator_1 TYPE c LENGTH 1 value '' ##NO_TEXT.
  data M_ELEMENT type ref to CL_ABAP_ELEMDESCR .
  data M_FIELD_LENGTH type I .
  data M_DECIMALS_SET type ABAP_BOOL .
  data M_CONVERSION_EXIT type ABAP_BOOL .

  methods _SET_BASIC_TYPE
    importing
      !I_BASIC_TYPE type ABAP_TYPEKIND .
  methods _WRITE
    importing
      !I_SAP_VALUE type DATA
    returning
      value(R_CSV_VALUE) type STRING .
  methods _READ
    importing
      !I_CSV_VALUE type STRING
    returning
      value(R_SAP_VALUE) type STRING
    raising
      ZCX_TBOX_CSVMAN .
  methods _POST_VALIDATIONS
    importing
      !I_VALUE type STRING
    raising
      ZCX_TBOX_CSVMAN .
  methods _PRE_VALIDATIONS
    importing
      !I_VALUE type STRING
    raising
      ZCX_TBOX_CSVMAN .
  methods _BASIC_WRITE
    importing
      !I_SAP_VALUE type STRING
    returning
      value(R_CSV_VALUE) type STRING .
  methods _BASIC_READ
    importing
      !I_CSV_VALUE type STRING
    returning
      value(R_SAP_VALUE) type STRING .
  methods _WRITE_CHAR
    importing
      !I_SAP_VALUE type DATA
    returning
      value(R_CSV_VALUE) type STRING .
  methods _READ_CHAR
    importing
      !I_CSV_VALUE type STRING
    returning
      value(R_SAP_VALUE) type STRING .
  methods _WRITE_DATE
    importing
      !I_SAP_VALUE type D
    returning
      value(R_CSV_VALUE) type STRING .
  methods _READ_DATE
    importing
      !I_CSV_VALUE type STRING
    returning
      value(R_SAP_VALUE) type STRING .
  methods _WRITE_TIME
    importing
      !I_SAP_VALUE type T
    returning
      value(R_CSV_VALUE) type STRING .
  methods _READ_TIME
    importing
      !I_CSV_VALUE type STRING
    returning
      value(R_SAP_VALUE) type STRING .
  methods _WRITE_NUMB
    importing
      !I_SAP_VALUE type NUMERIC
    returning
      value(R_CSV_VALUE) type STRING .
  methods _READ_NUMB
    importing
      !I_CSV_VALUE type STRING
    returning
      value(R_SAP_VALUE) type STRING .
  methods _TRANSFORMATION
    importing
      !I_CSV_VALUE type STRING
    returning
      value(R_SAP_VALUE) type STRING .
  methods _ASSIGN_ELEMENT
    importing
      !I_ELEMENT type ref to CL_ABAP_ELEMDESCR .
  methods _GET_FIELD_LENGTH
    returning
      value(RESULT) type I .
  methods _GET_COUNTRY
    returning
      value(RESULT) type LAND1 .
  methods _DEFAULT_CONFIGURATION .
  methods _GET_DECIMALS
    returning
      value(R) type I .
  methods _INVALID_NUMBER
    importing
      !I_VALUE type STRING
    returning
      value(R_FAIL) type ABAP_BOOL .
  methods _INVALID_TIME
    importing
      !I_VALUE type STRING
    returning
      value(R_FAIL) type ABAP_BOOL .
  methods _INVALID_DATE
    importing
      !I_VALUE type STRING
    returning
      value(R_FAIL) type ABAP_BOOL .
  methods _VALIDATION_TIME
    importing
      !I_VALUE type STRING .
  methods _VALIDATION_DATE
    importing
      !I_VALUE type STRING .
  methods _VALIDATION_NUMBER
    importing
      !I_VALUE type STRING .
  methods _TO_EXTERNAL_FORMAT
    importing
      !I_SAP_VALUE type DATA
    returning
      value(R_CSV_VALUE) type STRING .
  methods _TO_INTERNAL_FORMAT
    importing
      !I_CSV_VALUE type STRING
    returning
      value(R_SAP_VALUE) type STRING
    raising
      ZCX_TBOX_CSVMAN .
ENDCLASS.



CLASS ZCL_TBOX_CSVFIELD IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->ADD_POST_VALIDATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJECT_VALIDATOR             TYPE REF TO OBJECT(optional)
* | [--->] I_METHOD_VALIDATOR             TYPE        CLIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_post_validation.

    INSERT VALUE #(
      object_validator = COND #( WHEN i_object_validator IS BOUND THEN i_object_validator ELSE me )
      method_validator = i_method_validator ) INTO TABLE m_post_validations.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->ADD_PRE_VALIDATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJECT_VALIDATOR             TYPE REF TO OBJECT(optional)
* | [--->] I_METHOD_VALIDATOR             TYPE        CLIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_pre_validation.

    INSERT VALUE #(
      object_validator = COND #( WHEN i_object_validator IS BOUND THEN i_object_validator ELSE me )
      method_validator = i_method_validator ) INTO TABLE m_pre_validations.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->ALIGNMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ALIGNMENT                    TYPE        I
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD alignment.

    m_format_params-alignment = i_alignment.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->CONDENSE_VALUES
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ACTIVE                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD condense_values.

    m_format_params-condense = i_active.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CSV                          TYPE REF TO ZCL_TBOX_CSV_COMMON
* | [--->] I_NAME                         TYPE        CLIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    m_csv           = i_csv.
    m_field_name    = i_name.

    m_format_params = m_csv->m_format_params.
    m_decimals_set  = m_csv->m_decimals_set.

    _default_configuration( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->CONV_EXIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ACTIVE                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_exit.

    m_format_params-convexit = i_active.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->COUNTRY
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_COUNTRY                      TYPE        LAND1
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD country.

    m_format_params-country = i_country.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->DATE_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DATE_FORMAT                  TYPE        CLIKE
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD date_format.

    m_format_params-date_format = zcl_tbox_csv_common=>_create_date_format( i_date_format ).

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->DECIMALS
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DECIMALS                     TYPE        I
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD decimals.

    m_format_params-decimals  = i_decimals.
    m_decimals_set            = abap_true.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->EXCLUDE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ACTIVE                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD exclude.

    IF i_active = abap_true AND NOT line_exists( m_csv->m_exclude_fields[ low = m_field_name ] ).

      INSERT VALUE #(
        sign    = |I|
        option  = |EQ|
        low     = m_field_name ) INTO TABLE m_csv->m_exclude_fields.

    ELSEIF i_active = abap_false.

      DELETE m_csv->m_exclude_fields WHERE low = m_field_name.

    ENDIF.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->FIELD_LENGTH
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_FIELD_LENGTH                 TYPE        I
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD field_length.

    m_field_length      = i_field_length.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->INCLUDE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ACTIVE                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD include.

    IF i_active = abap_true AND NOT line_exists( m_csv->m_include_fields[ low = m_field_name ] ).

      INSERT VALUE #(
        sign    = |I|
        option  = |EQ|
        low     = m_field_name ) INTO TABLE m_csv->m_include_fields.

    ELSEIF i_active = abap_false.

      DELETE m_csv->m_include_fields WHERE low = m_field_name.

    ENDIF.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->KEEP_INIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ACTIVE                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD keep_init.

    m_format_params-keep_init = i_active.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->LABEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_LABEL                        TYPE        STRING
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD label.

    m_label = i_label.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->NUMBER_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NUMBER_FORMAT                TYPE        CLIKE
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD number_format.

    m_format_params-number_format = i_number_format.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->POSITION
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_POSITION                     TYPE        I
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD position.

    IF line_exists( m_csv->m_fields_mapping[ field = m_field_name ] ).

      MODIFY TABLE m_csv->m_fields_mapping FROM VALUE #(
        field     = m_field_name
        position  = i_position ).

    ELSE.

      INSERT VALUE #(
        field     = m_field_name
        position  = i_position ) INTO TABLE m_csv->m_fields_mapping.

    ENDIF.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->TIME_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TIME_FORMAT                  TYPE        CLIKE
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD time_format.

    m_format_params-time_format = zcl_tbox_csv_common=>_create_time_format( i_time_format ).

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_ASSIGN_ELEMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ELEMENT                      TYPE REF TO CL_ABAP_ELEMDESCR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _assign_element.

    m_element = i_element.

    DATA(basic_type)  = zcl_tbox_csvman=>get_normalized_type( m_element->type_kind ).

    _set_basic_type( basic_type ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_BASIC_READ
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CSV_VALUE                    TYPE        STRING
* | [<-()] R_SAP_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _basic_read.

    CLEAR m_conversion_exit.

    r_sap_value = COND #(
      WHEN m_format_params-condense = abap_true
        THEN condense( i_csv_value )
        ELSE i_csv_value ).

    IF m_format_params-convexit = abap_true.
      r_sap_value = _to_internal_format( i_csv_value ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_BASIC_WRITE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SAP_VALUE                    TYPE        STRING
* | [<-()] R_CSV_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _basic_write.

    r_csv_value = i_sap_value.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_DEFAULT_CONFIGURATION
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _default_configuration.

    IF m_format_params-alignment IS INITIAL.
      m_format_params-alignment = cl_abap_format=>a_left.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_GET_COUNTRY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RESULT                         TYPE        LAND1
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_country.

    result = COND #(

      WHEN  m_format_params-use_number_format = abap_false
        THEN m_format_params-country

      WHEN  m_format_params-number_format = c_dec_separator_1 OR
            m_format_params-number_format = c_dec_separator_3
        THEN `IT`

      WHEN  m_format_params-number_format = c_dec_separator_2
        THEN `US` ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_GET_DECIMALS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R                              TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_decimals.

    r = COND #(
      WHEN m_decimals_set = abap_true
        THEN m_format_params-decimals
        ELSE m_element->decimals ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_GET_FIELD_LENGTH
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RESULT                         TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_field_length.

    result = COND #(
      WHEN m_field_length > 0 THEN m_field_length
      ELSE m_element->output_length ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_INVALID_DATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VALUE                        TYPE        STRING
* | [<-()] R_FAIL                         TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _invalid_date.

    r_fail = abap_true.

    IF NOT contains( val    = i_value
                     regex  = '^\d{8}$' ).
      RETURN.
    ENDIF.

    DATA(value_date) = CONV d( i_value ).

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = value_date
      EXCEPTIONS
        error_message             = -1
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    r_fail = abap_false.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_INVALID_NUMBER
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VALUE                        TYPE        STRING
* | [<-()] R_FAIL                         TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _invalid_number.

    r_fail = abap_true.

    CHECK count( val = i_value
                 sub = `.` ) <= 1.
    CHECK count( val = i_value
                 sub = `-` ) <= 1.

    DATA(sign_off) = find( val = i_value
                           sub = `-` ).
    CHECK sign_off <= 0 OR sign_off = strlen( i_value ) - 1.

    CHECK i_value CO '.-0123456789'.

    r_fail = abap_false.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_INVALID_TIME
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VALUE                        TYPE        STRING
* | [<-()] R_FAIL                         TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _invalid_time.

    r_fail = abap_true.

    IF NOT contains( val   = i_value
                     regex = '^\d{6}$' ).
      RETURN.
    ENDIF.

    DATA(value_time) = CONV t( i_value ).

    CALL FUNCTION 'TIME_CHECK_PLAUSIBILITY'
      EXPORTING
        time                      = value_time
      EXCEPTIONS
        error_message             = -1
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    r_fail = abap_false.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSVFIELD->_OVERFLOW
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VALUE                        TYPE        STRING
* | [<-()] R_FAIL                         TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _overflow.


    r_fail = abap_false.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_POST_VALIDATIONS
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VALUE                        TYPE        STRING
* | [!CX!] ZCX_TBOX_CSVMAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _post_validations.

    DATA fail TYPE abap_bool.

    LOOP AT m_post_validations INTO DATA(post_val).

      CALL METHOD post_val-object_validator->(post_val-method_validator)
        EXPORTING
          i_value = i_value
        RECEIVING
          r_fail  = fail.

      IF fail = abap_true.

        DATA(csv) = CAST zcl_tbox_csv_reader( m_csv ).

        csv->_add_validation_fail(
          i_check_failed  = post_val-method_validator
          i_sap_value     = i_value ).

        m_abort = abap_true.
        RETURN.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_PRE_VALIDATIONS
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VALUE                        TYPE        STRING
* | [!CX!] ZCX_TBOX_CSVMAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _pre_validations.

    DATA fail TYPE abap_bool.

    LOOP AT m_pre_validations INTO DATA(pre_val).

      TRY.
          CALL METHOD pre_val-object_validator->(pre_val-method_validator)
            EXPORTING
              i_value = i_value
            RECEIVING
              r_fail  = fail.
        CATCH cx_root INTO DATA(x_root).
          RAISE EXCEPTION TYPE zcx_tbox_csvman EXPORTING text = x_root->get_text( ).
      ENDTRY.

      IF fail = abap_true.

        DATA(csv) = CAST zcl_tbox_csv_reader( m_csv ).
        csv->_add_validation_fail( pre_val-method_validator ).

        m_abort = abap_true.
        RETURN.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_READ
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CSV_VALUE                    TYPE        STRING
* | [<-()] R_SAP_VALUE                    TYPE        STRING
* | [!CX!] ZCX_TBOX_CSVMAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _read.

    CLEAR m_abort.

    _pre_validations( i_csv_value ).

    IF m_abort = abap_true.
      RETURN.
    ENDIF.

    DATA(sap_value) = _transformation( i_csv_value ).

    _post_validations( sap_value ).

    IF m_abort = abap_true.
      RETURN.
    ENDIF.

    r_sap_value = sap_value.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_READ_CHAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CSV_VALUE                    TYPE        STRING
* | [<-()] R_SAP_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _read_char.

    r_sap_value = i_csv_value.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_READ_DATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CSV_VALUE                    TYPE        STRING
* | [<-()] R_SAP_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _read_date.

    DATA(csv_value) = condense( i_csv_value ).

    IF csv_value IS INITIAL.
      r_sap_value = c_initial_date.
      RETURN.
    ENDIF.

    DATA internal_date TYPE c LENGTH 8.

    internal_date(4) = COND #(
      WHEN m_format_params-date_format-yy_len = 4
        THEN csv_value+m_format_params-date_format-yy_off(4)
        ELSE |{ sy-datum(2) }{ csv_value+m_format_params-date_format-yy_off(2) }| ).

    internal_date+4(2) = csv_value+m_format_params-date_format-mm_off(2).
    internal_date+6(2) = csv_value+m_format_params-date_format-dd_off(2).

    r_sap_value = internal_date.

    _validation_date( r_sap_value ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_READ_NUMB
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CSV_VALUE                    TYPE        STRING
* | [<-()] R_SAP_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _read_numb.

    IF i_csv_value IS INITIAL.
      r_sap_value = 0.
      RETURN.
    ENDIF.

    r_sap_value = i_csv_value.

    DATA(thousand_sep) = COND #(
      WHEN m_format_params-number_format = c_dec_separator_1 THEN '.'
      WHEN m_format_params-number_format = c_dec_separator_2 THEN ','
      WHEN m_format_params-number_format = c_dec_separator_3 THEN ' ' ).

    DATA(decimals_sep) = COND #(
      WHEN m_format_params-number_format = c_dec_separator_1 THEN ','
      WHEN m_format_params-number_format = c_dec_separator_2 THEN '.'
      WHEN m_format_params-number_format = c_dec_separator_3 THEN ',' ).

    IF thousand_sep IS NOT INITIAL.
      r_sap_value  = replace( val = r_sap_value
                              occ = 0
                              sub = thousand_sep
                              with = ` ` ).
    ENDIF.

    IF decimals_sep IS NOT INITIAL.
      r_sap_value  = replace( val = r_sap_value
                              occ = 0
                              sub = decimals_sep
                              with = `.` ).
    ENDIF.

    CONDENSE r_sap_value  NO-GAPS.

    _validation_number( r_sap_value ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_READ_TIME
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CSV_VALUE                    TYPE        STRING
* | [<-()] R_SAP_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _read_time.

    DATA(csv_value) = condense( i_csv_value ).

    IF csv_value IS INITIAL.
      r_sap_value = c_initial_time.
      RETURN.
    ENDIF.

    DATA internal_time TYPE c LENGTH 6.

    internal_time(2)    = csv_value+m_format_params-time_format-hh_off.
    internal_time+2(2)  = csv_value+m_format_params-time_format-mm_off.
    internal_time+4(2)  = csv_value+m_format_params-time_format-ss_off.

    r_sap_value = internal_time.

    _validation_time( r_sap_value ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_SET_BASIC_TYPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_BASIC_TYPE                   TYPE        ABAP_TYPEKIND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _set_basic_type.

    m_basic_type = i_basic_type.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_TO_EXTERNAL_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SAP_VALUE                    TYPE        DATA
* | [<-()] R_CSV_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _to_external_format.

    CHECK m_element->is_ddic_type( ).

    DATA(dfies) = m_element->get_ddic_field( sy-langu ).

    CHECK dfies-convexit IS NOT INITIAL.

    cl_rsan_ut_conversion_exit=>convert_to_extern(
      EXPORTING
        i_fieldinfo       = CORRESPONDING #( dfies )
        i_internal_value  = i_sap_value
      IMPORTING
        e_external_value  = r_csv_value ).

    m_conversion_exit = abap_true.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_TO_INTERNAL_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CSV_VALUE                    TYPE        STRING
* | [<-()] R_SAP_VALUE                    TYPE        STRING
* | [!CX!] ZCX_TBOX_CSVMAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _to_internal_format.

    r_sap_value = i_csv_value.

    CHECK m_element->is_ddic_type( ).

    DATA(dfies) = m_element->get_ddic_field( sy-langu ).

    CHECK dfies-convexit IS NOT INITIAL.

    cl_rsan_ut_conversion_exit=>convert_to_intern(
      EXPORTING
        i_fieldinfo       = CORRESPONDING #( dfies )
        i_external_value  = i_csv_value
      IMPORTING
        e_internal_value  = r_sap_value
      EXCEPTIONS
        failed            = 1 ).
    IF sy-subrc = 0.
      m_conversion_exit = abap_true.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_TRANSFORMATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CSV_VALUE                    TYPE        STRING
* | [<-()] R_SAP_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _transformation.

    DATA(basic_value) = _basic_read( i_csv_value ).

    r_sap_value = SWITCH #( m_basic_type

      WHEN cl_abap_typedescr=>typekind_char
        THEN _read_char( basic_value )

      WHEN cl_abap_typedescr=>typekind_date
        THEN _read_date( basic_value )

      WHEN cl_abap_typedescr=>typekind_time
        THEN _read_time( basic_value )

      WHEN cl_abap_typedescr=>typekind_packed
        THEN _read_numb( basic_value ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_VALIDATION_DATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VALUE                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _validation_date.

    DATA(csv) = CAST zcl_tbox_csv_reader( m_csv ).

    IF _invalid_date( i_value ).

      csv->_add_validation_fail(
        i_check_failed  = |INVALID_DATE|
        i_sap_value     = i_value ).

      m_abort = abap_true.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_VALIDATION_NUMBER
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VALUE                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _validation_number.

    DATA(csv) = CAST zcl_tbox_csv_reader( m_csv ).

    IF _invalid_number( i_value ).

      csv->_add_validation_fail(
        i_check_failed  = |INVALID_NUMBER|
        i_sap_value     = i_value ).

      m_abort = abap_true.
      RETURN.

    ENDIF.

    IF _overflow( i_value ).

      csv->_add_validation_fail(
        i_check_failed  = |OVERFLOW|
        i_sap_value     = i_value ).

      m_abort = abap_true.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_VALIDATION_TIME
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VALUE                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _validation_time.

    DATA(csv) = CAST zcl_tbox_csv_reader( m_csv ).

    IF _invalid_time( i_value ).

      csv->_add_validation_fail(
        i_check_failed  = |INVALID_TIME|
        i_sap_value     = i_value ).

      m_abort = abap_true.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_WRITE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SAP_VALUE                    TYPE        DATA
* | [<-()] R_CSV_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _write.

    CLEAR m_conversion_exit.

    IF m_format_params-convexit = abap_true.
      r_csv_value = _to_external_format( i_sap_value ).
    ENDIF.

    CHECK m_conversion_exit = abap_false.

    DATA(string_value) = SWITCH #( m_basic_type

      WHEN cl_abap_typedescr=>typekind_char
        THEN _write_char( i_sap_value )

      WHEN cl_abap_typedescr=>typekind_date
        THEN _write_date( i_sap_value )

      WHEN cl_abap_typedescr=>typekind_time
        THEN _write_time( i_sap_value )

      WHEN cl_abap_typedescr=>typekind_packed
        THEN _write_numb( i_sap_value ) ).

    r_csv_value = _basic_write( string_value ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_WRITE_CHAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SAP_VALUE                    TYPE        DATA
* | [<-()] R_CSV_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _write_char.

    r_csv_value = |{ i_sap_value WIDTH = _get_field_length( ) ALIGN = (m_format_params-alignment) }|.

    r_csv_value =
      COND #( WHEN m_format_params-condense = abap_true
        THEN condense( r_csv_value )
        ELSE r_csv_value ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_WRITE_DATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SAP_VALUE                    TYPE        D
* | [<-()] R_CSV_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _write_date.

    DATA c_date TYPE c LENGTH 10 VALUE '..........'.

    DATA(sap_value_str) = CONV string( i_sap_value ).

    IF sap_value_str = c_initial_date AND m_format_params-keep_init = abap_false.
      RETURN.
    ENDIF.

    CHECK sap_value_str IS NOT INITIAL.

    DATA(dd) = sap_value_str+6(2).
    DATA(mm) = sap_value_str+4(2).
    DATA(yy) = COND #( WHEN m_format_params-date_format-yy_len = 4 THEN sap_value_str(4) ELSE sap_value_str+2(2) ).

    c_date = replace( val = c_date
                      off = m_format_params-date_format-dd_off
                      len = 2
                      with = dd ).
    c_date = replace( val = c_date
                      off = m_format_params-date_format-mm_off
                      len = 2
                      with = mm ).
    c_date = replace( val = c_date
                      off = m_format_params-date_format-yy_off
                      len = m_format_params-date_format-yy_len
                      with = yy ).
    c_date = replace( val = c_date
                      occ = 0
                      sub = ` `
                      with = '0' ).
    c_date = replace( val = c_date
                      occ = 0
                      sub = '.'
                      with = m_format_params-date_format-sep ).

    r_csv_value = c_date.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_WRITE_NUMB
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SAP_VALUE                    TYPE        NUMERIC
* | [<-()] R_CSV_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _write_numb.

    DATA(country)   = _get_country( ).

    DATA(csv_value) = |{ i_sap_value DECIMALS = _get_decimals( ) COUNTRY = country }|.

    IF    m_format_params-use_number_format = abap_true
      AND m_format_params-number_format     = c_dec_separator_3.

      csv_value = replace( val = csv_value
                           occ = 0
                           sub = `.`
                           with = ` ` ).

    ENDIF.

    r_csv_value = csv_value.

    IF i_sap_value = 0 AND m_format_params-keep_init = abap_false.
      CLEAR r_csv_value.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSVFIELD->_WRITE_TIME
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SAP_VALUE                    TYPE        T
* | [<-()] R_CSV_VALUE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _write_time.

    DATA c_time TYPE c LENGTH 8 VALUE '........'.

    DATA(sap_value_str) = CONV string( i_sap_value ).

    IF sap_value_str = c_initial_time AND m_format_params-keep_init = abap_false.
      RETURN.
    ENDIF.

    CHECK sap_value_str IS NOT INITIAL.

    c_time = replace( val = c_time
                      off = m_format_params-time_format-hh_off
                      len = 2
                      with = sap_value_str(2) ).
    c_time = replace( val = c_time
                      off = m_format_params-time_format-mm_off
                      len = 2
                      with = sap_value_str+2(2) ).
    c_time = replace( val = c_time
                      off = m_format_params-time_format-ss_off
                      len = 2
                      with = sap_value_str+4(2) ).
    c_time = replace( val = c_time
                      occ = 0
                      sub = ` `
                      with = '0' ).
    c_time = replace( val = c_time
                      occ = 0
                      sub = '.'
                      with = m_format_params-time_format-sep ).

    r_csv_value = c_time.

  ENDMETHOD.
ENDCLASS.
