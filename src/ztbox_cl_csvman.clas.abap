class ZTBOX_CL_CSVMAN definition
  public
  final
  create public .

public section.

  interfaces ZTBOX_IF_CSVMAN .

  aliases TY_INT_RANGE
    for ZTBOX_IF_CSVMAN~TY_INT_RANGE .

  types:
    BEGIN OF ty_catalog,
        table_field  TYPE name_feld,
        csv_position TYPE i,
        field_obj    TYPE REF TO ZTBOX_CL_CSVFIELD,
        inactive     TYPE flag,
      END OF ty_catalog .
  types:
    ty_catalog_t TYPE TABLE OF ty_catalog WITH KEY table_field .
  types:
    BEGIN OF ty_validation_fails,
        row          TYPE i,
        col          TYPE i,
        table_field  TYPE name_feld,
        csv_value    TYPE string,
        sap_value    TYPE string,
        check_failed TYPE seocmpname,
      END OF ty_validation_fails .
  types:
    ty_validation_fails_t TYPE TABLE OF ty_validation_fails WITH DEFAULT KEY .
  types:
    BEGIN OF ty_global_validation_fails,
             row          TYPE i,
             check_failed TYPE seocmpname,
           END OF ty_global_validation_fails .
  types:
    ty_global_validation_fails_t TYPE TABLE OF ty_global_validation_fails WITH DEFAULT KEY .
  types:
    BEGIN OF ty_field_conf,
        quotechar         TYPE char1,
        convexit          TYPE flag,
        condense          TYPE flag,
        delimiter         TYPE char1,
        alignment         TYPE i,
        keep_init         TYPE flag,
        country           TYPE land1,
        date_format       TYPE string,
        time_format       TYPE string,
        number_format     TYPE xudcpfm,
        escapechar        TYPE char1,
        doublequote       TYPE flag,
        decimals          TYPE i,
        thousand_sep      TYPE c LENGTH 1,
        decimals_sep      TYPE c LENGTH 1,
        quoting           TYPE i,
        use_number_format TYPE flag,
      END OF ty_field_conf .
  types:
    BEGIN OF ty_config.
        INCLUDE TYPE ty_field_conf.
        TYPES header TYPE flag.
    TYPES header_desc    TYPE flag.
    TYPES end_of_line    TYPE string.
    TYPES include_only   TYPE flag.
    TYPES: END OF ty_config .

  constants C_DEC_SEPARATOR_3 type XUDCPFM value 'Y' ##NO_TEXT.
  constants C_DEC_SEPARATOR_2 type XUDCPFM value 'X' ##NO_TEXT.
  constants C_DEC_SEPARATOR_1 type XUDCPFM value '' ##NO_TEXT.
  constants C_QUOTE_ALL type I value 3 ##NO_TEXT.
  constants C_QUOTE_MINIMAL type I value 1 ##NO_TEXT.
  constants C_QUOTE_NONNUMERIC type I value 2 ##NO_TEXT.
  constants C_QUOTE_NONE type I value 0 ##NO_TEXT.

  class-methods CLASS_CONSTRUCTOR .
  methods READ_CSV
    importing
      !IT_CSV type STRING_TABLE
    changing
      !CT_TABLE type TABLE
    raising
      resumable(ZCX_TBOX_CSVMAN) .
  methods CREATE_CSV
    importing
      !IT_TABLE type TABLE
    returning
      value(R_CSV) type STRING
    raising
      resumable(ZCX_TBOX_CSVMAN) .
  methods CONSTRUCTOR .
  methods TO_STRING
    importing
      !I_TAB type STRING_TABLE
    returning
      value(R_RES) type STRING .
  methods TO_TABLE_STRING
    importing
      !I_STR type STRING
    returning
      value(R_RES) type STRING_TABLE .
  methods FIELD
    importing
      !I_NAME type FELD_NAME
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVFIELD .
  methods TIME_FORMAT
    importing
      !TIME_FORMAT type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods DATE_FORMAT
    importing
      !DATE_FORMAT type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods ESCAPECHAR
    importing
      !ESCAPECHAR type CHAR1
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods QUOTECHAR
    importing
      !QUOTECHAR type CHAR1
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods END_OF_LINE
    importing
      !EOF type CLIKE
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods ALIGNMENT
    importing
      !ALIGNMENT type I
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods CONVEXIT
    importing
      !CONVEXIT type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods CONDENSE
    importing
      !CONDENSE type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods KEEP_INIT
    importing
      !KEEP_INIT type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods COUNTRY
    importing
      !COUNTRY type LAND1
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods QUOTING
    importing
      !QUOTING type I
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods DOUBLEQUOTE
    importing
      !DOUBLEQUOTE type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods NUMBER_FORMAT
    importing
      !NUMBER_FORMAT type XUDCPFM
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods DECIMALS
    importing
      !DECIMALS type I
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods DELIMITER
    importing
      !DELIMITER type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods HEADER_DESC
    importing
      !HEADER_DESC type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods HEADER
    importing
      !HEADER type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods GET_VALIDATION_FAILS
    returning
      value(R_VALIDATION_FAILS) type TY_VALIDATION_FAILS_T .
  methods ADD_VALIDATION
    importing
      !CHECK_OBJECT type ref to OBJECT optional
      !CHECK_METHOD type SEOCMPNAME .
  methods RESET_VALIDATIONS .
  methods CHECK_CSV
    importing
      !IT_CSV type STRING_TABLE
    returning
      value(R_FAILS) type TY_GLOBAL_VALIDATION_FAILS_T .
  methods IGNORE_ROWS
    importing
      !ROWS type TY_INT_RANGE .
protected section.
private section.

  aliases _CONF
    for ZTBOX_IF_CSVMAN~_CONF .
  aliases TY_CONF
    for ZTBOX_IF_CSVMAN~TY_CONF .
  aliases TY_VALIDATIONS
    for ZTBOX_IF_CSVMAN~TY_VALIDATIONS .
  aliases TY_VALIDATIONS_T
    for ZTBOX_IF_CSVMAN~TY_VALIDATIONS_T .

  types:
    BEGIN OF ty_typekind_map,
      typekind TYPE abap_typekind,
      type_cat TYPE c LENGTH 1,
    END OF ty_typekind_map .
  types:
    ty_typekind_map_t TYPE TABLE OF ty_typekind_map WITH DEFAULT KEY .
  types:
    BEGIN OF ty_values,
      raw_data TYPE string,
      name     TYPE name_feld,
    END OF ty_values .
  types:
    ty_values_t TYPE TABLE OF ty_values WITH DEFAULT KEY .

  data _TABLE_REF type ref to DATA .
  class-data _TYPES_MAP type TY_TYPEKIND_MAP_T .
  data _CATALOG type TY_CATALOG_T .
  data _VALIDATION_FAILS type TY_VALIDATION_FAILS_T .
  data _CURRENT_ROW type I .
  data _VALIDATIONS type TY_VALIDATIONS_T .
  data _FIELDS_COUNT type I .
  data _EXCLUDE_ROWS type TY_INT_RANGE .

  methods INCLUDE_ONLY
    importing
      !INCLUDE_ONLY type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods _CSV_TO_LINE
    importing
      !IV_CSV type STRING
    raising
      resumable(ZCX_TBOX_CSVMAN) .
  methods _GET_HEADER
    returning
      value(R_HEADER) type STRING .
  class-methods _SET_TYPES_MAP .
  methods _PARSE_LINE
    importing
      !I_LINE type STRING
    returning
      value(R_VALUES) type STRING_TABLE
    raising
      resumable(ZCX_TBOX_CSVMAN) .
  methods _GET_VALUES
    importing
      !I_LINE type STRING
    returning
      value(R_VALUES) type TY_VALUES_T
    raising
      resumable(ZCX_TBOX_CSVMAN) .
  methods _CREATE_CATALOG
    importing
      !IT_TABLE type ANY TABLE
    raising
      resumable(ZCX_TBOX_CSVMAN) .
  methods _GET_LINE
    importing
      !IT_TABLE type ANY TABLE
    returning
      value(R_LINE) type ref to CL_ABAP_STRUCTDESCR
    raising
      resumable(ZCX_TBOX_CSVMAN) .
  methods _PARSE_VALUES
    importing
      !I_VALUES type STRING_TABLE
    returning
      value(R_VALUES) type STRING_TABLE .
  methods _APPLY_QUOTINGS
    importing
      !I_CAT type TY_CATALOG
    changing
      !C_VALUE type STRING .
  methods _APPLY_ESCAPINGS
    changing
      !C_VALUE type STRING .
  methods _TECHNICAL_CHECKS
    raising
      resumable(ZCX_TBOX_CSVMAN) .
  methods _EMPTY_ROW
    importing
      !VALUE type STRING
    returning
      value(FAIL) type FLAG .
  methods _DELIMITER_CHECK
    importing
      !VALUE type STRING
    returning
      value(FAIL) type FLAG .
  methods _VALIDATION_CHECKS
    importing
      !CSV_ROW type STRING
    raising
      resumable(ZCX_TBOX_CSVMAN) .
ENDCLASS.



CLASS ZTBOX_CL_CSVMAN IMPLEMENTATION.


  METHOD add_validation.

    APPEND VALUE #(
      object      = COND #( WHEN check_object IS SUPPLIED AND check_object IS BOUND THEN check_object ELSE me )
      validation  = check_method ) TO _validations.

  ENDMETHOD.


  METHOD alignment.

    _conf-alignment = alignment.

    r_res = me.

  ENDMETHOD.


  METHOD check_csv.

    LOOP AT it_csv INTO DATA(csv_row).

      TRY.
          _validation_checks( csv_row ).

        CATCH zcx_tbox_csvman INTO DATA(x_check).

          APPEND VALUE #(
            row           = sy-tabix
            check_failed  = x_check->method_name ) TO r_fails.

      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD class_constructor.

    _set_types_map( ).

  ENDMETHOD.


  METHOD condense.

    _conf-condense = condense.

    r_res = me.

  ENDMETHOD.


  METHOD constructor.

    header( abap_true ).
    delimiter( |,| ).
    end_of_line( cl_abap_char_utilities=>cr_lf ).
    alignment( cl_abap_format=>a_left ).
    convexit( abap_true ).
    date_format( |dd.mm.yyyy| ).
    time_format( |hh:mm:ss| ).
    quoting( c_quote_none ).
    keep_init( abap_true ).

    add_validation(
      check_object  = me
      check_method  = |_EMPTY_ROW| ).

    add_validation(
      check_object  = me
      check_method  = |_DELIMITER_CHECK| ).

  ENDMETHOD.


  METHOD convexit.

    _conf-convexit = convexit.

    r_res = me.

  ENDMETHOD.


  METHOD COUNTRY.

    _conf-country = country.

    r_res = me.

  ENDMETHOD.


  METHOD create_csv.

    _technical_checks( ).
    _create_catalog( it_table ).

    DATA(val)       = VALUE string( ).
    DATA(tab_lines) = lines( it_table ).
    DATA(cat_lines) = lines( _catalog ).

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<line>).
      DATA(tab_ix) = sy-tabix.

      LOOP AT _catalog INTO DATA(cat).
        DATA(cat_ix) = sy-tabix.

        CLEAR val.
        ASSIGN COMPONENT cat-table_field OF STRUCTURE <line> TO FIELD-SYMBOL(<val>).

        IF <val> IS NOT INITIAL OR _conf-keep_init EQ abap_true.

          val = cat-field_obj->write( <val> ).

        ENDIF.

        _apply_escapings(
          CHANGING  c_value = val ).

        _apply_quotings(
          EXPORTING i_cat   = cat
          CHANGING  c_value = val ).

        r_csv = r_csv && val.

        IF cat_ix NE cat_lines.
          r_csv = r_csv && _conf-delimiter.
        ENDIF.

      ENDLOOP.

      IF tab_ix NE tab_lines.
        r_csv = r_csv && _conf-end_of_line.
      ENDIF.

    ENDLOOP.

    IF _conf-header EQ abap_true.
      DATA(header) = _get_header( ).
      r_csv = header && _conf-end_of_line && r_csv.
    ENDIF.

  ENDMETHOD.


  METHOD date_format.

    _conf-date_format = date_format.

    r_res = me.

  ENDMETHOD.


  METHOD DECIMALS.

    _conf-decimals = decimals.

    r_res = me.

  ENDMETHOD.


  METHOD DELIMITER.

    _conf-delimiter = delimiter.

    r_res = me.

  ENDMETHOD.


  METHOD DOUBLEQUOTE.

    _conf-doublequote = doublequote.

    r_res = me.

  ENDMETHOD.


  METHOD end_of_line.

    _conf-end_of_line = eof.

    r_res = me.

  ENDMETHOD.


  METHOD ESCAPECHAR.

    _conf-escapechar = escapechar.

    r_res = me.

  ENDMETHOD.


  METHOD field.

    r_res = VALUE #( _catalog[ table_field = i_name ]-field_obj OPTIONAL ).

    IF r_res IS INITIAL.

      r_res = NEW #( ).

      r_res->config( _conf ).

      APPEND VALUE #(
        table_field = i_name
        field_obj   = r_res ) TO _catalog.

    ENDIF.

  ENDMETHOD.


  METHOD get_validation_fails.

    r_validation_fails = _validation_fails.

  ENDMETHOD.


  METHOD header.

    _conf-header = header.

    r_res = me.

  ENDMETHOD.


  METHOD header_desc.

    _conf-header_desc = header_desc.

    r_res = me.

  ENDMETHOD.


  METHOD ignore_rows.

    _exclude_rows = rows.

  ENDMETHOD.


  METHOD include_only.

    _conf-include_only = include_only.

    r_res = me.

  ENDMETHOD.


  METHOD keep_init.

    _conf-keep_init = keep_init.

    r_res = me.

  ENDMETHOD.


  METHOD number_format.

    _conf-number_format      = number_format.
    _conf-use_number_format  = abap_true.

    r_res = me.

  ENDMETHOD.


  METHOD quotechar.

    _conf-quotechar = quotechar.

    quoting( c_quote_all ).

    r_res = me.

  ENDMETHOD.


  METHOD QUOTING.

    _conf-quoting = quoting.

    r_res = me.

  ENDMETHOD.


  METHOD read_csv.

    _technical_checks( ).

    CREATE DATA _table_ref LIKE ct_table.

    GET REFERENCE OF ct_table INTO _table_ref.

    _create_catalog( ct_table ).

    CLEAR _current_row.
    LOOP AT it_csv INTO DATA(csv).

      _current_row = sy-tabix.

      IF _exclude_rows IS NOT INITIAL.
        CHECK _current_row NOT IN _exclude_rows.
      ENDIF.

      CHECK _conf-header EQ abap_false OR sy-tabix > 1.

      _csv_to_line( csv ).

    ENDLOOP.

  ENDMETHOD.


  METHOD reset_validations.

    CLEAR _validations.

  ENDMETHOD.


  METHOD time_format.

    _conf-time_format = time_format.

    r_res = me.

  ENDMETHOD.


  METHOD to_string.

    LOOP AT i_tab INTO DATA(row).

      r_res = COND #( WHEN sy-tabix EQ 1 THEN row ELSE r_res && _conf-end_of_line && row ).

    ENDLOOP.

  ENDMETHOD.


  METHOD to_table_string.

    CLEAR r_res.
    IF _conf-escapechar IS INITIAL OR find( val = i_str sub = _conf-escapechar && _conf-end_of_line ) = -1.

      SPLIT i_str AT _conf-end_of_line INTO TABLE r_res.

    ELSE.

      TRY.
          DATA(guid) = cl_system_uuid=>create_uuid_c22_static( ).
        CATCH cx_uuid_error.
          RETURN.
      ENDTRY.

      DATA(str) = replace( val = i_str occ = 0 sub = _conf-escapechar && _conf-end_of_line with = guid ).

      SPLIT str AT _conf-end_of_line INTO TABLE r_res.

      REPLACE ALL OCCURRENCES OF guid IN TABLE r_res WITH _conf-escapechar && _conf-end_of_line.

    ENDIF.

  ENDMETHOD.


  METHOD _apply_escapings.

    IF _conf-escapechar IS INITIAL AND _conf-doublequote EQ abap_true AND _conf-quotechar IS NOT INITIAL.
      c_value = replace( val = c_value occ = 0 sub = _conf-quotechar with = _conf-quotechar && _conf-quotechar ).
    ENDIF.

    IF _conf-escapechar IS NOT INITIAL AND _conf-quotechar IS NOT INITIAL.
      c_value = replace( val = c_value occ = 0 sub = _conf-quotechar with = _conf-escapechar && _conf-quotechar ).
    ENDIF.

    IF _conf-escapechar IS NOT INITIAL AND ( _conf-quotechar IS INITIAL OR _conf-quoting EQ ztbox_cl_csvman=>c_quote_none ).
      c_value = replace( val = c_value occ = 0 sub = _conf-delimiter with = _conf-escapechar && _conf-delimiter ).
    ENDIF.

    IF _conf-escapechar IS NOT INITIAL.
      c_value = replace( val = c_value occ = 0 sub = _conf-end_of_line with = _conf-escapechar && _conf-end_of_line ).
    ENDIF.

  ENDMETHOD.


  METHOD _apply_quotings.

    CASE _conf-quoting.

      WHEN c_quote_all.
        c_value =  _conf-quotechar && c_value && _conf-quotechar.

      WHEN c_quote_minimal.

        IF      find( val = c_value sub = _conf-delimiter ) GE 0
          OR    find( val = c_value sub = _conf-end_of_line ) GE 0
          OR  ( _conf-quotechar IS NOT INITIAL AND find( val = c_value sub = _conf-quotechar ) GE 0 ).

          c_value = _conf-quotechar && c_value && _conf-quotechar.

        ENDIF.

      WHEN c_quote_nonnumeric.

        IF i_cat-field_obj IS NOT INSTANCE OF ZTBOX_CL_CSVFIELD_numb.
          c_value = _conf-quotechar && c_value && _conf-quotechar.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD _create_catalog.

    DATA field_obj TYPE REF TO ztbox_cl_csvfield.

    DATA(line)        = _get_line( it_table ).
    DATA(components)  = line->components.

    LOOP AT _catalog INTO DATA(cat).
      CHECK NOT line_exists( components[ name = cat-table_field ] ).
      DELETE _catalog INDEX sy-tabix.
    ENDLOOP.

    LOOP AT components INTO DATA(comp).

      DATA(ix) = sy-tabix.

      DATA(elem)      = CAST cl_abap_elemdescr( line->get_component_type( comp-name ) ).
      DATA(type_cat)  = VALUE #( _types_map[ typekind = elem->type_kind ]-type_cat OPTIONAL ).

      CLEAR field_obj.
      field_obj = COND #(
        WHEN type_cat EQ cl_abap_elemdescr=>typekind_date   THEN NEW ztbox_cl_csvfield_date( )
        WHEN type_cat EQ cl_abap_elemdescr=>typekind_time   THEN NEW ztbox_cl_csvfield_time( )
        WHEN type_cat EQ cl_abap_elemdescr=>typekind_packed THEN NEW ztbox_cl_csvfield_numb( )
        ELSE NEW ztbox_cl_csvfield_char( ) ).

      field_obj->assign_element( elem ).
      field_obj->config( _conf ).

      READ TABLE _catalog ASSIGNING FIELD-SYMBOL(<cat>) WITH KEY table_field = comp-name.
      IF sy-subrc EQ 0.

        field_obj->copy( <cat>-field_obj ).
        field_obj->csv_position( ix ).
        IF field_obj->_include EQ abap_true.
          include_only( abap_true ).
        ENDIF.

        <cat>-field_obj     = field_obj.
        <cat>-csv_position  = field_obj->_csv_position.

        <cat>-inactive      = COND #(
          WHEN  <cat>-field_obj->_exclude EQ abap_true OR
              ( <cat>-field_obj->_include EQ abap_false AND _conf-include_only EQ abap_true )
          THEN abap_true
          ELSE abap_false ).

      ELSE.

        APPEND VALUE #(
          table_field   = comp-name
          inactive      = COND #( WHEN _conf-include_only EQ abap_true THEN abap_true ELSE abap_false )
          field_obj     = field_obj ) TO _catalog.

      ENDIF.

    ENDLOOP.

    DELETE _catalog WHERE inactive EQ abap_true.

    DATA jx TYPE i VALUE 1.
    LOOP AT _catalog ASSIGNING <cat> WHERE csv_position IS INITIAL.
      DO.
        IF NOT line_exists( _catalog[ csv_position = jx ] ).
          EXIT.
        ELSE.
          ADD 1 TO jx.
        ENDIF.
      ENDDO.

      <cat>-csv_position = jx.
    ENDLOOP.

    SORT _catalog BY csv_position.

  ENDMETHOD.


  METHOD _csv_to_line.

    FIELD-SYMBOLS <tab>   TYPE ANY TABLE.
    ASSIGN _table_ref->*  TO <tab>.

    DATA line_ref         TYPE REF TO data.
    CREATE DATA line_ref  LIKE LINE OF <tab>.
    ASSIGN line_ref->*    TO FIELD-SYMBOL(<row>).

    DATA(values) = _get_values( iv_csv ).
    CHECK values IS NOT INITIAL.

    LOOP AT _catalog INTO DATA(cat).

      ASSIGN COMPONENT cat-table_field OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
      CHECK sy-subrc EQ 0.

      DATA(val_str) = VALUE #( values[ name = cat-table_field ]-raw_data OPTIONAL ).

      TRY.

          <val> = cat-field_obj->read( val_str ).

        CATCH zcx_tbox_csvman INTO DATA(x_field).

          APPEND VALUE #(
            row           = _current_row
            col           = cat-csv_position
            table_field   = cat-table_field
            csv_value     = val_str
            sap_value     = x_field->sap_value
            check_failed  = x_field->method_name ) TO _validation_fails.

      ENDTRY.

    ENDLOOP.

    INSERT <row> INTO TABLE <tab>.

  ENDMETHOD.


  METHOD _delimiter_check.

    DATA(csv_row) = VALUE string( ).

    csv_row = value.

    IF _conf-escapechar IS NOT INITIAL.
      csv_row = replace( val = csv_row occ = 0 sub = _conf-escapechar && _conf-escapechar with = ` ` ).
    ENDIF.

    IF _conf-escapechar IS NOT INITIAL.
      csv_row = replace( val = csv_row occ = 0 sub = _conf-escapechar && _conf-delimiter with = ` ` ).
    ENDIF.

    TRY.
        DATA(values) = _parse_line( value ).
      CATCH zcx_tbox_csvman.
        RETURN.
    ENDTRY.

    IF _fields_count IS INITIAL.
      _fields_count = lines( values ).
      RETURN.
    ENDIF.

    IF lines( values ) NE _fields_count.
      fail = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD _empty_row.

    fail = xsdbool( value IS INITIAL ).

  ENDMETHOD.


  METHOD _get_header.

    LOOP AT _catalog INTO DATA(cat).

      DATA(header_txt) = COND #(
        WHEN _conf-header_desc EQ abap_true AND cat-field_obj->_label IS NOT INITIAL
          THEN cat-field_obj->_label
        ELSE cat-table_field ).

      DATA(val) = COND string(
        WHEN _conf-quotechar IS INITIAL
          THEN header_txt
          ELSE |{ _conf-quotechar }{ header_txt }{ _conf-quotechar }| ).

      r_header = COND #(
        WHEN r_header IS INITIAL
          THEN val
        ELSE |{ r_header }{ _conf-delimiter }{ val }| ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _get_line.

    DATA(tdesc) = cl_abap_typedescr=>describe_by_data( it_table ).

    IF tdesc->kind NE cl_abap_typedescr=>kind_table.

      RAISE EXCEPTION TYPE zcx_tbox_csvman
        EXPORTING
          textid = zcx_tbox_csvman=>invalid_table.

    ENDIF.

    DATA(table_desc)  = CAST cl_abap_tabledescr( tdesc ).
    DATA(line_type)   = table_desc->get_table_line_type( ).

    IF  line_type->type_kind NE cl_abap_typedescr=>typekind_struct1 AND
        line_type->type_kind NE cl_abap_typedescr=>typekind_struct2.

      RAISE EXCEPTION TYPE zcx_tbox_csvman
        EXPORTING
          textid = zcx_tbox_csvman=>invalid_table.

    ENDIF.

    r_line = CAST cl_abap_structdescr( line_type ).

  ENDMETHOD.


  METHOD _get_values.

    DATA(values) = _parse_line( i_line ).

    LOOP AT _catalog INTO DATA(cat).

      READ TABLE values INTO DATA(value) INDEX cat-csv_position.
      CHECK sy-subrc EQ 0.

      r_values = VALUE #( BASE r_values (
        raw_data  = value
        name      = cat-table_field ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _parse_line.

    DATA(line) = VALUE string( ).
    line = i_line.

    IF _conf-escapechar IS NOT INITIAL AND find( val = line sub = _conf-escapechar && _conf-escapechar ) GE 0.

      TRY.
          DATA(guid_esc) = cl_system_uuid=>create_uuid_c22_static( ).
        CATCH cx_uuid_error.
          RETURN.
      ENDTRY.

      line = replace( val = line occ = 0 sub = _conf-escapechar && _conf-escapechar with = guid_esc ).

    ENDIF.

    IF _conf-escapechar IS NOT INITIAL AND find( val = line sub = _conf-escapechar && _conf-delimiter ) GE 0.

      TRY.
          DATA(guid_del) = cl_system_uuid=>create_uuid_c22_static( ).
        CATCH cx_uuid_error.
          RETURN.
      ENDTRY.

      line = replace( val = line occ = 0 sub = _conf-escapechar && _conf-delimiter with = guid_del ).

    ENDIF.

    SPLIT line AT _conf-delimiter INTO TABLE DATA(values).
    r_values = _parse_values( values ).

    IF guid_del IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF guid_del IN TABLE r_values WITH _conf-delimiter.
    ENDIF.

    IF guid_esc IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF guid_del IN TABLE r_values WITH _conf-escapechar.
    ENDIF.

  ENDMETHOD.


  METHOD _parse_values.

    IF lines( i_values ) EQ 1.
      r_values = i_values.
      RETURN.
    ENDIF.

    DATA tot_enc TYPE i.
    DATA(combine)       = abap_false.
    DATA(val_combined)  = VALUE string( ).
    DATA(values)        = VALUE string_table( ).

    values = i_values.

    LOOP AT values INTO DATA(value).

      IF _conf-escapechar IS NOT INITIAL.
        FIND ALL OCCURRENCES OF |{ _conf-escapechar }{ _conf-quotechar }| IN value MATCH COUNT DATA(c_esc).
      ENDIF.
      IF _conf-quotechar IS NOT INITIAL.
        FIND ALL OCCURRENCES OF _conf-quotechar IN value MATCH COUNT DATA(c_enc).
      ENDIF.
      tot_enc = tot_enc + c_enc - c_esc.

      CASE tot_enc MOD 2.

        WHEN 0.
          IF combine EQ abap_true.
            val_combined = val_combined && _conf-delimiter && value.
            value = val_combined.
          ENDIF.

          IF value IS NOT INITIAL AND _conf-quotechar IS NOT INITIAL.

            DATA(val_first) = value(1).
            DATA(val_last)  = substring( val = value off = strlen( value ) - 1 len = 1 ).

            IF val_last EQ _conf-quotechar AND val_first EQ _conf-quotechar.
              value = substring( val = value off = 1 len = strlen( value ) - 2 ).
            ENDIF.

          ENDIF.

          IF _conf-escapechar IS NOT INITIAL.
            value = replace( val = value occ = 0 sub = _conf-escapechar && _conf-quotechar with = _conf-quotechar ).
          ENDIF.

          APPEND value TO r_values.
          CLEAR: value, val_combined, combine.

        WHEN OTHERS.
          combine = abap_true.
          val_combined = COND #( WHEN val_combined IS INITIAL THEN value ELSE val_combined && _conf-delimiter && value ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_types_map.

    _types_map = VALUE #(
      ( typekind  = cl_abap_typedescr=>typekind_char
        type_cat  = cl_abap_typedescr=>typekind_char )
      ( typekind  = cl_abap_typedescr=>typekind_num
        type_cat  = cl_abap_typedescr=>typekind_char )
      ( typekind  = cl_abap_typedescr=>typekind_numeric
        type_cat  = cl_abap_typedescr=>typekind_char )
      ( typekind  = cl_abap_typedescr=>typekind_clike
        type_cat  = cl_abap_typedescr=>typekind_char )
      ( typekind  = cl_abap_typedescr=>typekind_csequence
        type_cat  = cl_abap_typedescr=>typekind_char )
      ( typekind  = cl_abap_typedescr=>typekind_simple
        type_cat  = cl_abap_typedescr=>typekind_char )
      ( typekind  = cl_abap_typedescr=>typekind_string
        type_cat  = cl_abap_typedescr=>typekind_char )
      ( typekind  = cl_abap_typedescr=>typekind_date
        type_cat  = cl_abap_typedescr=>typekind_date )
      ( typekind  = cl_abap_typedescr=>typekind_time
        type_cat  = cl_abap_typedescr=>typekind_time )
      ( typekind  = cl_abap_typedescr=>typekind_decfloat
        type_cat  = cl_abap_typedescr=>typekind_packed )
      ( typekind  = cl_abap_typedescr=>typekind_decfloat16
        type_cat  = cl_abap_typedescr=>typekind_packed )
      ( typekind  = cl_abap_typedescr=>typekind_decfloat34
        type_cat  = cl_abap_typedescr=>typekind_packed )
      ( typekind  = cl_abap_typedescr=>typekind_float
        type_cat  = cl_abap_typedescr=>typekind_packed )
      ( typekind  = cl_abap_typedescr=>typekind_packed
        type_cat  = cl_abap_typedescr=>typekind_packed )
      ( typekind  = cl_abap_typedescr=>typekind_int
        type_cat  = cl_abap_typedescr=>typekind_packed )
      ( typekind  = cl_abap_typedescr=>typekind_int1
        type_cat  = cl_abap_typedescr=>typekind_packed )
      ( typekind  = cl_abap_typedescr=>typekind_int2
        type_cat  = cl_abap_typedescr=>typekind_packed )
      ( typekind  = cl_abap_typedescr=>typekind_int8
        type_cat  = cl_abap_typedescr=>typekind_packed )
      ( typekind  = cl_abap_typedescr=>typekind_hex
        type_cat  = cl_abap_typedescr=>typekind_char )
      ( typekind  = cl_abap_typedescr=>typekind_xsequence
        type_cat  = cl_abap_typedescr=>typekind_char )
      ( typekind  = cl_abap_typedescr=>typekind_xstring
        type_cat  = cl_abap_typedescr=>typekind_char ) ).

  ENDMETHOD.


  METHOD _technical_checks.

    IF _conf-delimiter IS NOT INITIAL AND _conf-delimiter EQ _conf-quotechar.

      RAISE EXCEPTION TYPE zcx_tbox_csvman
        EXPORTING
          textid = zcx_tbox_csvman=>invalid_delim_quote.

    ENDIF.

    IF _conf-escapechar IS NOT INITIAL AND _conf-escapechar EQ _conf-delimiter.

      RAISE EXCEPTION TYPE zcx_tbox_csvman
        EXPORTING
          textid = zcx_tbox_csvman=>invalid_delim_escape.

    ENDIF.

    IF _conf-escapechar IS NOT INITIAL AND _conf-escapechar EQ _conf-quotechar.

      RAISE EXCEPTION TYPE zcx_tbox_csvman
        EXPORTING
          textid = zcx_tbox_csvman=>invalid_escape_quote.

    ENDIF.

    IF _conf-quotechar IS INITIAL AND _conf-doublequote EQ abap_true.

      RAISE EXCEPTION TYPE zcx_tbox_csvman
        EXPORTING
          textid = zcx_tbox_csvman=>quotechar_double.

    ENDIF.

    IF _conf-quotechar IS INITIAL AND _conf-quoting NE ztbox_cl_csvman=>c_quote_none.

      RAISE EXCEPTION TYPE zcx_tbox_csvman
        EXPORTING
          textid = zcx_tbox_csvman=>quotechar_quoting.

    ENDIF.

  ENDMETHOD.


  METHOD _validation_checks.

    LOOP AT _validations ASSIGNING FIELD-SYMBOL(<val>).

      CALL METHOD <val>-object->(<val>-validation) EXPORTING value = csv_row RECEIVING fail = <val>-failed.

      IF <val>-failed EQ abap_true.
        RAISE EXCEPTION TYPE zcx_tbox_csvman
          EXPORTING
            method_name = <val>-validation.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
