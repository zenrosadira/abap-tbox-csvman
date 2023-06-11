CLASS zcl_tbox_csvfield DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_tbox_csv_common .

  PUBLIC SECTION.

    METHODS exclude
    IMPORTING
      !i_active TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(r) TYPE REF TO zcl_tbox_csvfield .
    METHODS position
    IMPORTING
      !i_position TYPE i
    RETURNING
      VALUE(r) TYPE REF TO zcl_tbox_csvfield .
    METHODS include
    IMPORTING
      !i_active TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(r) TYPE REF TO zcl_tbox_csvfield .
    METHODS label
    IMPORTING
      !i_label TYPE string
    RETURNING
      VALUE(r) TYPE REF TO zcl_tbox_csvfield .
    METHODS decimals
    IMPORTING
      !i_decimals TYPE i
    RETURNING
      VALUE(r) TYPE REF TO zcl_tbox_csvfield .
    METHODS number_format
    IMPORTING
      !i_number_format TYPE clike
    RETURNING
      VALUE(r) TYPE REF TO zcl_tbox_csvfield .
    METHODS time_format
    IMPORTING
      !i_time_format TYPE clike
    RETURNING
      VALUE(r) TYPE REF TO zcl_tbox_csvfield .
    METHODS date_format
    IMPORTING
      !i_date_format TYPE clike
    RETURNING
      VALUE(r) TYPE REF TO zcl_tbox_csvfield .
    METHODS country
    IMPORTING
      !i_country TYPE land1
    RETURNING
      VALUE(r) TYPE REF TO zcl_tbox_csvfield .
    METHODS field_length
    IMPORTING
      !i_field_length TYPE i
    RETURNING
      VALUE(r) TYPE REF TO zcl_tbox_csvfield .
    METHODS keep_init
    IMPORTING
      !i_active TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(r) TYPE REF TO zcl_tbox_csvfield .
    METHODS alignment
    IMPORTING
      !i_alignment TYPE i
    RETURNING
      VALUE(r) TYPE REF TO zcl_tbox_csvfield .
    METHODS condense_values
    IMPORTING
      !i_active TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(r) TYPE REF TO zcl_tbox_csvfield .
    METHODS add_post_validation
    IMPORTING
      !i_object_validator TYPE REF TO object OPTIONAL
      !i_method_validator TYPE clike .
    METHODS add_pre_validation
    IMPORTING
      !i_object_validator TYPE REF TO object OPTIONAL
      !i_method_validator TYPE clike .
    METHODS _overflow
    IMPORTING
      !i_value TYPE string
    RETURNING
      VALUE(r_fail) TYPE abap_bool .
    METHODS constructor
      IMPORTING
        !i_csv TYPE REF TO zcl_tbox_csv_common
        !i_name TYPE clike .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
    BEGIN OF ts_validation,
      object_validator TYPE REF TO object,
      method_validator TYPE string,
    END OF ts_validation .

    DATA:
    m_pre_validations  TYPE STANDARD TABLE OF ts_validation .
    DATA:
    m_post_validations TYPE STANDARD TABLE OF ts_validation .
    DATA m_csv TYPE REF TO zcl_tbox_csv_common .
    DATA m_field_name TYPE string .
    DATA m_basic_type TYPE abap_typekind .
    DATA m_format_params TYPE zcl_tbox_csv_common=>ts_format_params .
    DATA m_label TYPE string .
    DATA m_abort TYPE abap_bool .
    CONSTANTS c_initial_date TYPE string VALUE '00000000' ##NO_TEXT.
    CONSTANTS c_initial_time TYPE string VALUE '000000' ##NO_TEXT.
    CONSTANTS:
    c_dec_separator_3 TYPE c LENGTH 1 VALUE 'Y' ##NO_TEXT.
    CONSTANTS:
    c_dec_separator_2 TYPE c LENGTH 1 VALUE 'X' ##NO_TEXT.
    CONSTANTS:
    c_dec_separator_1 TYPE c LENGTH 1 VALUE '' ##NO_TEXT.
    DATA m_element TYPE REF TO cl_abap_elemdescr .
    DATA m_field_length TYPE i .
    DATA m_decimals_set TYPE abap_bool .

    METHODS _set_basic_type
    IMPORTING
      !i_basic_type TYPE abap_typekind .
    METHODS _write
    IMPORTING
      !i_sap_value TYPE data
    RETURNING
      VALUE(r_csv_value) TYPE string .
    METHODS _read
    IMPORTING
      !i_csv_value TYPE string
    RETURNING
      VALUE(r_sap_value) TYPE string
    RAISING
      zcx_tbox_csvman .
    METHODS _post_validations
    IMPORTING
      !i_value TYPE string
    RAISING
      zcx_tbox_csvman .
    METHODS _pre_validations
    IMPORTING
      !i_value TYPE string
    RAISING
      zcx_tbox_csvman .
    METHODS _basic_write
    IMPORTING
      !i_sap_value TYPE string
    RETURNING
      VALUE(r_csv_value) TYPE string .
    METHODS _basic_read
    IMPORTING
      !i_csv_value TYPE string
    RETURNING
      VALUE(r_sap_value) TYPE string .
    METHODS _write_char
    IMPORTING
      !i_sap_value TYPE data
    RETURNING
      VALUE(r_csv_value) TYPE string .
    METHODS _read_char
    IMPORTING
      !i_csv_value TYPE string
    RETURNING
      VALUE(r_sap_value) TYPE string .
    METHODS _write_date
    IMPORTING
      !i_sap_value TYPE d
    RETURNING
      VALUE(r_csv_value) TYPE string .
    METHODS _read_date
    IMPORTING
      !i_csv_value TYPE string
    RETURNING
      VALUE(r_sap_value) TYPE string .
    METHODS _write_time
    IMPORTING
      !i_sap_value TYPE t
    RETURNING
      VALUE(r_csv_value) TYPE string .
    METHODS _read_time
    IMPORTING
      !i_csv_value TYPE string
    RETURNING
      VALUE(r_sap_value) TYPE string .
    METHODS _write_numb
    IMPORTING
      !i_sap_value TYPE numeric
    RETURNING
      VALUE(r_csv_value) TYPE string .
    METHODS _read_numb
    IMPORTING
      !i_csv_value TYPE string
    RETURNING
      VALUE(r_sap_value) TYPE string .
    METHODS _transformation
    IMPORTING
      !i_csv_value TYPE string
    RETURNING
      VALUE(r_sap_value) TYPE string .
    METHODS _assign_element
    IMPORTING
      !i_element TYPE REF TO cl_abap_elemdescr .
    METHODS _get_field_length
    RETURNING
      VALUE(result) TYPE i .
    METHODS _get_country
    RETURNING
      VALUE(result) TYPE land1 .
    METHODS _default_configuration .
    METHODS _get_decimals
    RETURNING
      VALUE(r) TYPE i .
    METHODS _invalid_number
    IMPORTING
      !i_value TYPE string
    RETURNING
      VALUE(r_fail) TYPE abap_bool .
    METHODS _invalid_time
    IMPORTING
      !i_value TYPE string
    RETURNING
      VALUE(r_fail) TYPE abap_bool .
    METHODS _invalid_date
    IMPORTING
      !i_value TYPE string
    RETURNING
      VALUE(r_fail) TYPE abap_bool .
    METHODS _validation_time
    IMPORTING
      !i_value TYPE string .
    METHODS _validation_date
    IMPORTING
      !i_value TYPE string .
    METHODS _validation_number
    IMPORTING
      !i_value TYPE string .
ENDCLASS.


CLASS zcl_tbox_csvfield IMPLEMENTATION.


  METHOD add_post_validation.

    INSERT VALUE #(
      object_validator = COND #( WHEN i_object_validator IS BOUND THEN i_object_validator ELSE me )
      method_validator = i_method_validator ) INTO TABLE m_post_validations.

  ENDMETHOD.


  METHOD add_pre_validation.

    INSERT VALUE #(
      object_validator = COND #( WHEN i_object_validator IS BOUND THEN i_object_validator ELSE me )
      method_validator = i_method_validator ) INTO TABLE m_pre_validations.

  ENDMETHOD.


  METHOD alignment.

    m_format_params-alignment = i_alignment.

    r = me.

  ENDMETHOD.


  METHOD condense_values.

    m_format_params-condense = i_active.

    r = me.

  ENDMETHOD.


  METHOD constructor.

    m_csv           = i_csv.
    m_field_name    = i_name.

    m_format_params = m_csv->m_format_params.
    m_decimals_set  = m_csv->m_decimals_set.

    _default_configuration( ).

  ENDMETHOD.


  METHOD country.

    m_format_params-country = i_country.

    r = me.

  ENDMETHOD.


  METHOD date_format.

    m_format_params-date_format = zcl_tbox_csv_common=>_create_date_format( i_date_format ).

    r = me.

  ENDMETHOD.


  METHOD decimals.

    m_format_params-decimals  = i_decimals.
    m_decimals_set            = abap_true.

    r = me.

  ENDMETHOD.


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


  METHOD field_length.

    m_field_length      = i_field_length.

    r = me.

  ENDMETHOD.


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


  METHOD keep_init.

    m_format_params-keep_init = i_active.

    r = me.

  ENDMETHOD.


  METHOD label.

    m_label = i_label.

    r = me.

  ENDMETHOD.


  METHOD number_format.

    m_format_params-number_format = i_number_format.

    r = me.

  ENDMETHOD.


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


  METHOD time_format.

    m_format_params-time_format = zcl_tbox_csv_common=>_create_time_format( i_time_format ).

    r = me.

  ENDMETHOD.


  METHOD _assign_element.

    m_element = i_element.

    DATA(basic_type)  = zcl_tbox_csvman=>get_normalized_type( m_element->type_kind ).

    _set_basic_type( basic_type ).

  ENDMETHOD.


  METHOD _basic_read.

    r_sap_value = COND #( WHEN m_format_params-condense = abap_true THEN condense( i_csv_value ) ELSE i_csv_value ).

  ENDMETHOD.


  METHOD _basic_write.

    r_csv_value = i_sap_value.

  ENDMETHOD.


  METHOD _default_configuration.

    IF m_format_params-alignment IS INITIAL.
      m_format_params-alignment = cl_abap_format=>a_left.
    ENDIF.

  ENDMETHOD.


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


  METHOD _get_decimals.

    r = COND #(
      WHEN m_decimals_set = abap_true
        THEN m_format_params-decimals
        ELSE m_element->decimals ).

  ENDMETHOD.


  METHOD _get_field_length.

    result = COND #(
      WHEN m_field_length > 0 THEN m_field_length
      ELSE m_element->output_length ).

  ENDMETHOD.


  METHOD _invalid_date.

    r_fail = abap_true.

    IF NOT contains( val    = i_value
                     pcre   = '^\d{8}$' ).
      RETURN.
    ENDIF.


    r_fail = abap_false.

  ENDMETHOD.


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


  METHOD _invalid_time.

    r_fail = abap_true.


    r_fail = abap_false.

  ENDMETHOD.


  METHOD _overflow.


    r_fail = abap_false.

  ENDMETHOD.


  METHOD _post_validations.

    DATA fail TYPE abap_bool.

    LOOP AT m_post_validations INTO DATA(post_val).

      CALL METHOD post_val-object_validator->(post_val-method_validator)
        EXPORTING i_value = i_value
        RECEIVING r_fail  = fail.

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


  METHOD _pre_validations.

    DATA fail TYPE abap_bool.

    LOOP AT m_pre_validations INTO DATA(pre_val).

      TRY.
          CALL METHOD pre_val-object_validator->(pre_val-method_validator) 
            EXPORTING i_value = i_value 
            RECEIVING r_fail  = fail.
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


  METHOD _read_char.

    r_sap_value = i_csv_value.

  ENDMETHOD.


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


  METHOD _set_basic_type.

    m_basic_type = i_basic_type.

  ENDMETHOD.


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


  METHOD _validation_date.

    DATA(csv) = CAST zcl_tbox_csv_reader( m_csv ).

    IF _invalid_date( i_value ).

      csv->_add_validation_fail(
        i_check_failed  = |INVALID_DATE|
        i_sap_value     = i_value ).

      m_abort = abap_true.

    ENDIF.

  ENDMETHOD.


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


  METHOD _validation_time.

    DATA(csv) = CAST zcl_tbox_csv_reader( m_csv ).

    IF _invalid_time( i_value ).

      csv->_add_validation_fail(
        i_check_failed  = |INVALID_TIME|
        i_sap_value     = i_value ).

      m_abort = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD _write.

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


  METHOD _write_char.

    r_csv_value = |{ i_sap_value WIDTH = _get_field_length( ) ALIGN = (m_format_params-alignment) }|.

    r_csv_value =
      COND #( WHEN m_format_params-condense = abap_true
        THEN condense( r_csv_value )
        ELSE r_csv_value ).

  ENDMETHOD.


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
