CLASS zcl_tbox_csv_reader DEFINITION
  PUBLIC
  INHERITING FROM zcl_tbox_csv_common
  FINAL
  CREATE PRIVATE
GLOBAL FRIENDS zcl_tbox_csvfield
                 zcl_tbox_csvman .

  PUBLIC SECTION.

    TYPES:
    BEGIN OF ts_validation_fails,
        row          TYPE i,
        col          TYPE i,
        table_field  TYPE c LENGTH 30,
        csv_value    TYPE string,
        sap_value    TYPE string,
        check_failed TYPE c LENGTH 30,
      END OF ts_validation_fails .
    TYPES:
    tt_validation_fails TYPE TABLE OF ts_validation_fails WITH EMPTY KEY .
    TYPES:
    BEGIN OF ts_transposed_row,
             column_index TYPE i,
             value        TYPE string,
           END OF ts_transposed_row .
    TYPES:
    BEGIN OF ts_transposed_tab,
             row_index TYPE i,
             row       TYPE TABLE OF ts_transposed_row WITH KEY column_index,
           END OF ts_transposed_tab .
    TYPES:
    tt_transposed_tab TYPE TABLE OF ts_transposed_tab WITH KEY row_index .

    METHODS read
    IMPORTING
      !i_csv TYPE data
    EXPORTING
      !e_table TYPE ANY TABLE
    RAISING
      zcx_tbox_csvman .
    METHODS get_validation_fails
    RETURNING
      VALUE(r) TYPE tt_validation_fails .
    METHODS constructor .
    METHODS dereference
    IMPORTING
      !i_reference TYPE REF TO data
    EXPORTING
      !e_table TYPE ANY TABLE
    RAISING
      zcx_tbox_csvman .
    METHODS detranspose
    IMPORTING
      !i_transposed TYPE tt_transposed_tab
    EXPORTING
      VALUE(e_table) TYPE ANY TABLE
    RAISING
      zcx_tbox_csvman .
    METHODS read_with_transposition
    IMPORTING
      !i_csv TYPE data
    RETURNING
      VALUE(r) TYPE tt_transposed_tab
    RAISING
      zcx_tbox_csvman .
    METHODS read_with_reference
    IMPORTING
      !i_csv TYPE data
    RETURNING
      VALUE(r) TYPE REF TO data
    RAISING
      zcx_tbox_csvman .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA _csv_string_table TYPE string_table .
    DATA _table_of_strings TYPE REF TO data .
    DATA m_validation_fails TYPE tt_validation_fails .
    DATA m_current_row TYPE i .
    DATA m_current_col TYPE i .
    DATA m_current_field TYPE string .
    DATA m_current_value TYPE string .
    DATA m_current_line TYPE string .

    METHODS _set_csv_string_table
    IMPORTING
      !csv TYPE data .
    METHODS _parse_csv
    EXPORTING
      !e_table TYPE ANY TABLE
    RAISING
      zcx_tbox_csvman .
    METHODS _parse_line
    IMPORTING
      !line TYPE string
    RETURNING
      VALUE(r) TYPE string_table .
    METHODS _parse_fields
    IMPORTING
      !values TYPE string_table
    RETURNING
      VALUE(r) TYPE string_table .
    METHODS _add_validation_fail
    IMPORTING
      !i_check_failed TYPE clike
      !i_sap_value TYPE string OPTIONAL .
    METHODS _default_config .
    CLASS-METHODS _new_guid
    RETURNING
      VALUE(result) TYPE sysuuid_c22 .
    METHODS _identify_special_character
    IMPORTING
      !i_special_characeter TYPE string
    RETURNING
      VALUE(r) TYPE sysuuid_c22 .
    METHODS _transpose_csv
    RETURNING
      VALUE(r) TYPE tt_transposed_tab .
ENDCLASS.


CLASS zcl_tbox_csv_reader IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    _default_config( ).

  ENDMETHOD.


  METHOD dereference.

    CHECK i_reference IS BOUND.
    ASSIGN i_reference->* TO FIELD-SYMBOL(<tab>).
    CHECK sy-subrc = 0.

    DATA(csv_tab)    = zcl_tbox_csvman=>get_writer( )->write_to_string_table( <tab> ).

    read( EXPORTING i_csv = csv_tab IMPORTING e_table = e_table ).

  ENDMETHOD.


  METHOD detranspose.

    DATA row_ref TYPE REF TO data.

    _set_catalog( e_table ).

    LOOP AT i_transposed INTO DATA(csv_line).

      m_current_row = sy-tabix.

      DATA(values) = csv_line-row.

      CREATE DATA row_ref LIKE LINE OF e_table.
      ASSIGN row_ref->* TO FIELD-SYMBOL(<row>).
      CHECK sy-subrc = 0.

      LOOP AT m_fields_mapping INTO DATA(map).

        CLEAR m_current_value.
        READ TABLE values INTO DATA(csv_row) INDEX map-position.
        CHECK sy-subrc = 0.

        m_current_value = csv_row-value.
        m_current_col   = map-position.
        m_current_field = map-field.

        DATA(field_obj) = VALUE #( m_catalog[ field = m_current_field ]-field_obj OPTIONAL ).
        CHECK field_obj IS BOUND.

        ASSIGN COMPONENT m_current_field OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
        CHECK sy-subrc = 0.
        <val> = field_obj->_read( m_current_value ).

      ENDLOOP.

      INSERT <row> INTO TABLE e_table.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_validation_fails.

    r = m_validation_fails.

  ENDMETHOD.


  METHOD read.

    _set_catalog( e_table ).

    _set_csv_string_table( i_csv ).

    _parse_csv( IMPORTING e_table = e_table ).

  ENDMETHOD.


  METHOD read_with_reference.

    CLEAR: m_catalog, m_fields_mapping.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.

    _set_csv_string_table( i_csv ).

    CHECK _csv_string_table IS NOT INITIAL.

    DATA(first_line) = _parse_line( _csv_string_table[ 1 ] ).

    DATA(tab_of_strings) = lcl_utils=>create_type_table_from_catalog(
      VALUE #( FOR _col IN first_line INDEX INTO i
        ( name = |F{ i }|
          type = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( VALUE string( ) ) ) ) ) ).

    ASSIGN tab_of_strings->* TO <tab>.
    CHECK sy-subrc = 0.

    _set_catalog( <tab> ).

    _parse_csv( IMPORTING e_table = <tab> ).

    r = tab_of_strings.

    CLEAR: m_catalog, m_fields_mapping.

  ENDMETHOD.


  METHOD read_with_transposition.

    CLEAR: m_catalog, m_fields_mapping.

    _set_csv_string_table( i_csv ).

    r = _transpose_csv( ).

  ENDMETHOD.


  METHOD _add_validation_fail.

    INSERT VALUE #(
      row = m_current_row
      col          = m_current_col
      table_field  = m_current_field
      csv_value    = m_current_value
      sap_value    = i_sap_value
      check_failed = i_check_failed ) INTO TABLE m_validation_fails.

  ENDMETHOD.


  METHOD _default_config.

    header( ).
    delimiter( `;` ).
    end_of_line( cl_abap_char_utilities=>cr_lf ).
    date_format( `dd.mm.yyyy` ).
    time_format( `hh:mm:ss` ).

  ENDMETHOD.


  METHOD _identify_special_character.

    CHECK m_general_params-escapechar IS NOT INITIAL.
    CHECK count(
      val = m_current_line
      sub = i_special_characeter ) <> 0.

    r = _new_guid( ).

    m_current_line = replace(
      val   = m_current_line
      occ   = 0
      sub   = i_special_characeter
      with  = r ).

  ENDMETHOD.


  METHOD _new_guid.

    TRY.
        result = cl_system_uuid=>create_uuid_c22_static( ).
      CATCH cx_uuid_error.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD _parse_csv.

    DATA row_ref TYPE REF TO data.

    IF m_general_params-header = abap_true.
      DELETE _csv_string_table INDEX 1.
    ENDIF.

    LOOP AT _csv_string_table INTO DATA(csv_line).

      m_current_row = sy-tabix.

      DATA(values) = _parse_line( csv_line ).

      CREATE DATA row_ref LIKE LINE OF e_table.
      ASSIGN row_ref->* TO FIELD-SYMBOL(<row>).
      CHECK sy-subrc = 0.

      LOOP AT m_fields_mapping INTO DATA(map).

        CLEAR m_current_value.
        READ TABLE values INTO m_current_value INDEX map-position.
        CHECK sy-subrc = 0.

        m_current_col   = map-position.
        m_current_field = map-field.

        DATA(field_obj) = VALUE #( m_catalog[ field = m_current_field ]-field_obj OPTIONAL ).
        CHECK field_obj IS BOUND.

        ASSIGN COMPONENT m_current_field OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
        CHECK sy-subrc = 0.
        <val> = field_obj->_read( m_current_value ).

      ENDLOOP.

      INSERT <row> INTO TABLE e_table.

    ENDLOOP.

  ENDMETHOD.


  METHOD _parse_fields.

    IF lines( values ) = 1.
      r = values.
      RETURN.
    ENDIF.

    DATA(tot_enc)       = 0.
    DATA(combine)       = abap_false.
    DATA(val_combined)  = VALUE string( ).

    LOOP AT values INTO DATA(value).

      IF m_general_params-escapechar IS NOT INITIAL.
        DATA(c_esc) = count( val = value
                             off = 0 
                             sub = |{ m_general_params-escapechar }{ m_general_params-quotechar }| ).
      ENDIF.

      IF m_general_params-quotechar IS NOT INITIAL.
        DATA(c_enc) = count( val = value
                             off = 0 
                             sub = m_general_params-quotechar ).
      ENDIF.

      tot_enc = tot_enc + c_enc - c_esc.

      CASE tot_enc MOD 2.

        WHEN 0.

          IF combine = abap_true.
            val_combined = val_combined && m_general_params-delimiter && value.
            value = val_combined.
          ENDIF.

          IF value IS NOT INITIAL AND m_general_params-quotechar IS NOT INITIAL.

            DATA(condensed_value) = condense( value ).
            IF condensed_value IS NOT INITIAL.
              DATA(val_first)       = condensed_value(1).
              DATA(val_last)        = substring( val = condensed_value
                                                 off = strlen( condensed_value ) - 1 
                                                 len = 1 ).

              IF val_last = m_general_params-quotechar AND val_first = m_general_params-quotechar.
                value = substring( val = condensed_value
                                   off = 1 
                                   len = strlen( condensed_value ) - 2 ).
              ENDIF.
            ENDIF.

          ENDIF.

          IF m_general_params-escapechar IS NOT INITIAL.
            value = replace( val = value
                             occ = 0 
                             sub = m_general_params-escapechar && m_general_params-quotechar 
                             with = m_general_params-quotechar ).
          ENDIF.

          APPEND value TO r.
          CLEAR: value, val_combined, combine.

        WHEN OTHERS.

          combine = abap_true.
          val_combined = COND #(
            WHEN val_combined IS INITIAL 
              THEN value 
              ELSE val_combined && m_general_params-delimiter && value ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD _parse_line.

    m_current_line = line.

    DATA(guid_esc) = _identify_special_character( m_general_params-escapechar && m_general_params-escapechar ).
    DATA(guid_del) = _identify_special_character( m_general_params-escapechar && m_general_params-delimiter ).

    SPLIT m_current_line AT m_general_params-delimiter INTO TABLE DATA(values).

    DATA(nextolast) = strlen( m_current_line ) - 1.
    IF m_current_line+nextolast(1) = m_general_params-delimiter.
      APPEND VALUE string( ) TO values.
    ENDIF.

    r = _parse_fields( values ).

    IF guid_del IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF guid_del IN TABLE r WITH m_general_params-delimiter.
    ENDIF.

    IF guid_esc IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF guid_del IN TABLE r WITH m_general_params-escapechar.
    ENDIF.

  ENDMETHOD.


  METHOD _set_csv_string_table.

    CASE abap_true.

      WHEN lcl_utils=>is_elem( csv ).
        SPLIT csv AT m_general_params-end_of_line INTO TABLE _csv_string_table.

      WHEN lcl_utils=>is_table_of_elem( csv ).
        MOVE-CORRESPONDING csv TO _csv_string_table.

      WHEN lcl_utils=>is_table_of_one_component( csv ).
        FIELD-SYMBOLS <tab> TYPE ANY TABLE.
        ASSIGN csv TO <tab>.
        CHECK sy-subrc = 0.

        LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).

          ASSIGN COMPONENT 1 OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
          CHECK sy-subrc = 0.
          APPEND <val> TO _csv_string_table.

        ENDLOOP.

    ENDCASE.

  ENDMETHOD.


  METHOD _transpose_csv.

    IF m_general_params-header = abap_true.
      DELETE _csv_string_table INDEX 1.
    ENDIF.

    r = VALUE #(
      FOR _line IN _csv_string_table INDEX INTO i
      LET _values = _parse_line( _line ) IN (
        row_index = i
        row       = VALUE #(
          FOR _val IN _values INDEX INTO j (
            column_index  = j
            value         = _val ) ) ) ).

  ENDMETHOD.
ENDCLASS.
