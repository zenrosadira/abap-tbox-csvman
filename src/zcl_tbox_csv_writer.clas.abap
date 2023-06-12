CLASS zcl_tbox_csv_writer DEFINITION
  PUBLIC
  INHERITING FROM zcl_tbox_csv_common
  FINAL
  CREATE PRIVATE
GLOBAL FRIENDS zcl_tbox_csvman.

  PUBLIC SECTION.
    CONSTANTS c_quote_all        TYPE i VALUE 3.
    CONSTANTS c_quote_minimal    TYPE i VALUE 1.
    CONSTANTS c_quote_nonnumeric TYPE i VALUE 2.
    CONSTANTS c_quote_none       TYPE i VALUE 0.

    METHODS constructor.

    METHODS write
      IMPORTING i_table      TYPE ANY TABLE
      RETURNING VALUE(r_csv) TYPE string
      RAISING   zcx_tbox_csvman.

    METHODS _create_csv
      IMPORTING i_table      TYPE ANY TABLE
      RETURNING VALUE(r_csv) TYPE string.

    METHODS write_to_string_table
      IMPORTING i_table  TYPE ANY TABLE
      RETURNING VALUE(r) TYPE string_table
      RAISING   zcx_tbox_csvman.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA m_current_field_obj TYPE REF TO zcl_tbox_csvfield.

    METHODS _apply_escapings
      CHANGING c_value TYPE string.

    METHODS _apply_quotings
      CHANGING c_value TYPE string.

    METHODS _get_header
      RETURNING VALUE(r) TYPE string.

    METHODS _default_config.

    METHODS _to_string_table
      IMPORTING i_csv_string TYPE string
      RETURNING VALUE(r)     TYPE string_table.
ENDCLASS.


CLASS zcl_tbox_csv_writer IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    _default_config( ).
  ENDMETHOD.

  METHOD write.
    _set_catalog( i_table ).

    r_csv = _create_csv( i_table ).
  ENDMETHOD.

  METHOD write_to_string_table.
    DATA(csv_string) = write( i_table ).

    r = _to_string_table( csv_string ).
  ENDMETHOD.

  METHOD _apply_escapings.
    IF     m_general_params-escapechar  IS INITIAL
       AND m_general_params-doublequote  = abap_true
       AND m_general_params-quotechar   IS NOT INITIAL.
      c_value = replace( val  = c_value
                         occ  = 0
                         sub  = m_general_params-quotechar
                         with = m_general_params-quotechar && m_general_params-quotechar ).
    ENDIF.

    IF     m_general_params-escapechar IS NOT INITIAL
       AND m_general_params-quotechar  IS NOT INITIAL.
      c_value = replace( val  = c_value
                         occ  = 0
                         sub  = m_general_params-quotechar
                         with = m_general_params-escapechar && m_general_params-quotechar ).
    ENDIF.

    IF     m_general_params-escapechar IS NOT INITIAL
       AND (    m_general_params-quotechar IS INITIAL
             OR m_general_params-quoting    = c_quote_none ).
      c_value = replace( val  = c_value
                         occ  = 0
                         sub  = m_general_params-delimiter
                         with = m_general_params-escapechar && m_general_params-delimiter ).
    ENDIF.

    IF m_general_params-escapechar IS NOT INITIAL.
      c_value = replace( val  = c_value
                         occ  = 0
                         sub  = m_general_params-end_of_line
                         with = m_general_params-escapechar && m_general_params-end_of_line ).
    ENDIF.
  ENDMETHOD.

  METHOD _apply_quotings.
    CASE m_general_params-quoting.

      WHEN c_quote_all.
        c_value = m_general_params-quotechar && c_value && m_general_params-quotechar.

      WHEN c_quote_minimal.

        IF    find( val = c_value
                    sub = m_general_params-delimiter )   >= 0
           OR find( val = c_value
                    sub = m_general_params-end_of_line ) >= 0
           OR ( m_general_params-quotechar IS NOT INITIAL AND find( val = c_value
                                                                    sub = m_general_params-quotechar ) >= 0 ).

          c_value = m_general_params-quotechar && c_value && m_general_params-quotechar.

        ENDIF.

      WHEN c_quote_nonnumeric.

        IF m_current_field_obj->m_basic_type <> cl_abap_typedescr=>typekind_packed.
          c_value = m_general_params-quotechar && c_value && m_general_params-quotechar.
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD _create_csv.
    LOOP AT i_table ASSIGNING FIELD-SYMBOL(<row>).
      DATA(tab_ix) = sy-tabix.

      LOOP AT m_fields_mapping INTO DATA(map).
        DATA(map_ix) = sy-tabix.

        m_current_field_obj = VALUE #( m_catalog[ field = map-field ]-field_obj OPTIONAL ).
        IF m_current_field_obj IS NOT BOUND.
          CONTINUE.
        ENDIF.

        ASSIGN COMPONENT map-field OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        DATA(val) = m_current_field_obj->_write( <val> ).

        _apply_escapings( CHANGING c_value = val ).
        _apply_quotings( CHANGING c_value = val ).

        r_csv = r_csv && val.

        IF map_ix < lines( m_fields_mapping ).
          r_csv = r_csv && m_general_params-delimiter.
        ENDIF.

      ENDLOOP.

      IF tab_ix < lines( i_table ).
        r_csv = r_csv && m_general_params-end_of_line.
      ENDIF.

    ENDLOOP.

    IF m_general_params-header = abap_true.
      r_csv = _get_header( ) && m_general_params-end_of_line && r_csv.
    ENDIF.
  ENDMETHOD.

  METHOD _default_config.
    header( ).
    delimiter( `;` ).
    end_of_line( cl_abap_char_utilities=>cr_lf ).
    date_format( `dd.mm.yyyy` ).
    time_format( `hh:mm:ss` ).
    alignment( cl_abap_format=>a_left ).
  ENDMETHOD.

  METHOD _get_header.
    LOOP AT m_fields_mapping INTO DATA(map).

      DATA(field_obj) = VALUE #( m_catalog[ field = map-field ]-field_obj OPTIONAL ).
      IF field_obj IS NOT BOUND.
        CONTINUE.
      ENDIF.

      DATA(column_text) = COND #(
        WHEN field_obj->m_label IS NOT INITIAL
        THEN field_obj->m_label
        ELSE map-field ).

      DATA(val) = COND string(
        WHEN m_general_params-quotechar IS INITIAL
        THEN column_text
        ELSE |{ m_general_params-quotechar }{ column_text }{ m_general_params-quotechar }| ).

      r = COND #(
        WHEN r IS INITIAL
        THEN val
        ELSE |{ r }{ m_general_params-delimiter }{ val }| ).

    ENDLOOP.
  ENDMETHOD.

  METHOD _to_string_table.
    IF    m_general_params-escapechar IS INITIAL
       OR count( val = i_csv_string
                 sub = m_general_params-escapechar && m_general_params-end_of_line ) <= 0.

      SPLIT i_csv_string AT m_general_params-end_of_line INTO TABLE r.

      RETURN.

    ENDIF.

    TRY.
        DATA(guid) = cl_system_uuid=>create_uuid_c22_static( ).
      CATCH cx_uuid_error.
        RETURN.
    ENDTRY.

    DATA(csv_string) = replace( val  = i_csv_string
                                occ  = 0
                                sub  = m_general_params-escapechar && m_general_params-end_of_line
                                with = guid ).

    SPLIT csv_string AT m_general_params-end_of_line INTO TABLE r.

    REPLACE ALL OCCURRENCES OF guid IN TABLE r WITH m_general_params-escapechar && m_general_params-end_of_line.
  ENDMETHOD.
ENDCLASS.
