class ZTBOX_CL_CSVMAN definition
  public
  final
  create public

  global friends ZTBOX_CL_CSVMAN_FIELD
                 ZTBOX_CL_CSVMAN_LINE .

public section.

  types:
    BEGIN OF ty_catalog,
        table_field  TYPE name_feld,
        csv_position TYPE i,
        field_obj    TYPE REF TO ztbox_cl_fieldesc,
        priority     TYPE flag,
      END OF ty_catalog .
  types:
    ty_catalog_t TYPE TABLE OF ty_catalog WITH KEY table_field .
  types:
    BEGIN OF ty_mappings,
        csv_position TYPE i,
        table_field  TYPE name_feld,
      END OF ty_mappings .
  types:
    ty_mappings_t TYPE TABLE OF ty_mappings WITH DEFAULT KEY .
  types:
    BEGIN OF ty_validation_fails,
             row         TYPE i,
             col         TYPE i,
             table_field TYPE name_feld,
             raw_value   TYPE string,
             sap_value   TYPE string,
             method_fail TYPE seocmpname,
           END OF ty_validation_fails .
  types:
    ty_validation_fails_t TYPE TABLE OF ty_validation_fails WITH DEFAULT KEY .
  types:
    BEGIN OF ty_field_conf,
        enclosing      TYPE char1,
        convexit       TYPE flag,
        condense       TYPE flag,
        alignment      TYPE i,
        keep_initials  TYPE flag,
        country_format TYPE land1,
        date_format    TYPE string,
        time_format    TYPE string,
        escapator      TYPE char1,
        thousand_sep   TYPE c LENGTH 1,
        decimals_sep   TYPE c LENGTH 1,
      END OF ty_field_conf .
  types:
    BEGIN OF ty_config.
        INCLUDE TYPE ty_field_conf.
        TYPES header TYPE flag.
    TYPES header_desc    TYPE flag.
    TYPES separator      TYPE string.
    TYPES end_of_line    TYPE string.
    TYPES include_only   TYPE flag.
    TYPES mappings       TYPE ty_mappings_t.
    TYPES: END OF ty_config .
  types:
    BEGIN OF ty_fields_attributes.
    TYPES name TYPE name_feld.
    INCLUDE TYPE ty_field_conf.
    TYPES: END OF ty_fields_attributes .
  types:
    ty_fields_attributes_t TYPE TABLE OF ty_fields_attributes WITH DEFAULT KEY .

  class-methods CLASS_CONSTRUCTOR .
  methods READ_CSV
    importing
      !IT_CSV type STRING_TABLE
    changing
      !CT_TABLE type TABLE .
  methods CREATE_CSV
    importing
      !IT_TABLE type TABLE
    returning
      value(R_CSV) type STRING .
  methods CONSTRUCTOR .
  methods GET_FIRST_LINE
    returning
      value(R_HEADER) type STRING_TABLE .
  class-methods TO_STRING
    importing
      !I_TAB type STRING_TABLE
      !I_EOL type ANY default CL_ABAP_CHAR_UTILITIES=>CR_LF
    returning
      value(R_RES) type STRING .
  class-methods TO_TABLE_STRING
    importing
      !I_STR type STRING
      !I_EOL type ANY default CL_ABAP_CHAR_UTILITIES=>CR_LF
    returning
      value(R_RES) type STRING_TABLE .
  methods FIELD
    importing
      !I_NAME type FELD_NAME
    returning
      value(R_RES) type ref to ZTBOX_CL_FIELDESC .
  methods MAPPINGS
    importing
      !I_MAPPINGS type TY_MAPPINGS_T
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods TIME_FORMAT
    importing
      !TIME_FORMAT type STRING
      !FOR_FIELD type NAME_FELD optional
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods DATE_FORMAT
    importing
      !DATE_FORMAT type STRING
      !FOR_FIELD type NAME_FELD optional
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods ESCAPATOR
    importing
      !ESCAPATOR type CHAR1
      !FOR_FIELD type NAME_FELD optional
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods ENCLOSING
    importing
      !ENCLOSING type CHAR1
      !FOR_FIELD type NAME_FELD optional
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods END_OF_LINE
    importing
      !I_PAR type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods ALIGNMENT
    importing
      !ALIGNMENT type I
      !FOR_FIELD type NAME_FELD optional
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods THOUSAND_SEP
    importing
      !THOUSAND_SEP type CHAR1
      !FOR_FIELD type NAME_FELD optional
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods CONVEXIT
    importing
      !CONVEXIT type FLAG
      !FOR_FIELD type NAME_FELD optional
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods CONDENSE
    importing
      !CONDENSE type FLAG
      !FOR_FIELD type NAME_FELD optional
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods KEEP_INIT
    importing
      !KEEP_INIT type FLAG
      !FOR_FIELD type NAME_FELD optional
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods DECIMAL_SEP
    importing
      !DECIMAL_SEP type CHAR1
      !FOR_FIELD type NAME_FELD optional
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods COUNTRY_FORMAT
    importing
      !COUNTRY type LAND1
      value(FOR_FIELD) type NAME_FELD
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods SEPARATOR
    importing
      !I_PAR type STRING
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods HEADER_DESC
    importing
      !I_PAR type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods HEADER
    importing
      !I_PAR type FLAG
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN .
  methods GET_VALIDATION_FAILS
    returning
      value(R_VALIDATION_FAILS) type TY_VALIDATION_FAILS_T .
protected section.
private section.

  types:
    BEGIN OF ty_typekind_map,
      typekind TYPE abap_typekind,
      type_cat TYPE c LENGTH 1,
    END OF ty_typekind_map .
  types:
    ty_typekind_map_t TYPE TABLE OF ty_typekind_map WITH DEFAULT KEY .
  types:
    BEGIN OF ty_fields,
      name      TYPE name_feld,
      position  TYPE i,
      field_obj TYPE REF TO ztbox_cl_csvman_field,
    END OF ty_fields .
  types:
    ty_fields_t TYPE TABLE OF ty_fields .
  types:
    BEGIN OF ty_values,
      raw_data TYPE string,
      name     TYPE name_feld,
    END OF ty_values .
  types:
    ty_values_t TYPE TABLE OF ty_values WITH DEFAULT KEY .

  data _CONFIGURATION type TY_CONFIG .
  data _TABLE_REF type ref to DATA .
  data _MAPPINGS type STRING_TABLE .
  class-data _TYPES_MAP type TY_TYPEKIND_MAP_T .
  data _FIELDS type TY_FIELDS_T .
  data _LINE_DESC type ref to CL_ABAP_STRUCTDESCR .
  data _TAB_DESC type ref to ZTBOX_CL_TABDESC .
  data _FIELDS_ATTRIBUTES type TY_FIELDS_ATTRIBUTES_T .
  data _CATALOG type TY_CATALOG_T .
  data _VALIDATION_FAILS type TY_VALIDATION_FAILS_T .
  data _CURRENT_ROW type I .

  methods _LINE_TO_CSV
    importing
      !IS_LINE type ANY
    returning
      value(R_CSV) type STRING .
  methods _SET_TABINFO
    importing
      !IT_TABLE type TABLE .
  methods _CSV_TO_LINE
    importing
      !IV_CSV type STRING .
  methods _GET_HEADER
    returning
      value(R_HEADER) type STRING .
  class-methods _SET_TYPES_MAP .
  methods _REORDER_FIELDS .
  methods _SET_MAPPINGS .
  methods _SET_LINE_DESC
    importing
      !IT_TABLE type TABLE .
  methods _PARSE_LINE
    importing
      !I_LINE type STRING
    returning
      value(R_VALUES) type STRING_TABLE .
  methods _GET_VALUES
    importing
      !I_LINE type STRING
    returning
      value(R_VALUES) type TY_VALUES_T .
  methods _ASSIGN_ATTRIBUTE
    importing
      !I_FIELD type NAME_FELD
      !I_ATTRIBUTE type STRING
      !I_VALUE type ANY .
  methods _CONFIG_FIELDS .
  methods _CREATE_CATALOG
    importing
      !IT_TABLE type ANY TABLE .
  methods _GET_LINE
    importing
      !IT_TABLE type ANY TABLE
    returning
      value(R_LINE) type ref to CL_ABAP_STRUCTDESCR .
ENDCLASS.



CLASS ZTBOX_CL_CSVMAN IMPLEMENTATION.


  METHOD ALIGNMENT.

    _assign_attribute(
      i_attribute = |ALIGNMENT|
      i_field     = for_field
      i_value     = alignment ).

    r_res = me.

  ENDMETHOD.


  METHOD class_constructor.

    _set_types_map( ).

  ENDMETHOD.


  METHOD CONDENSE.

    _assign_attribute(
      i_attribute = |CONDENSE|
      i_field     = for_field
      i_value     = condense ).

    r_res = me.

  ENDMETHOD.


  METHOD constructor.

    header( abap_true ).
    separator( CONV #( cl_abap_char_utilities=>horizontal_tab ) ).
    alignment( cl_abap_format=>a_left ).
    convexit( abap_true ).
    end_of_line( CONV #( cl_abap_char_utilities=>cr_lf ) ).
    date_format( |gg.mm.yyyy| ).
    time_format( |hh:mm:ss| ).
    thousand_sep( |.| ).
    decimal_sep( |,| ).

  ENDMETHOD.


  METHOD CONVEXIT.

    _assign_attribute(
      i_attribute = |CONVEXIT|
      i_field     = for_field
      i_value     = convexit ).

    r_res = me.

  ENDMETHOD.


  METHOD country_format.

    _assign_attribute(
      i_attribute = |COUNTRY_FORMAT|
      i_field     = for_field
      i_value     = country ).

    r_res = me.

  ENDMETHOD.


  METHOD create_csv.

    _create_catalog( it_table ).
*    _reorder_fields( ).
*    _set_tabinfo( it_table ).
*    _reorder_fields( ).
*    _set_mappings( ).

    IF _configuration-header EQ abap_true.
      r_csv = |{ _get_header( ) }{ _configuration-end_of_line }|.
    ENDIF.

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<line>).

      DATA(eof) = COND #( WHEN sy-tabix NE lines( it_table ) THEN _configuration-end_of_line ELSE space ).

      r_csv = |{ r_csv }{ _line_to_csv( <line> ) }{ eof }|.

    ENDLOOP.

  ENDMETHOD.


  METHOD date_format.

    _assign_attribute(
      i_attribute = |DATE_FORMAT|
      i_field     = for_field
      i_value     = date_format ).

    r_res = me.

  ENDMETHOD.


  METHOD DECIMAL_SEP.

    _assign_attribute(
      i_attribute = |DECIMALS_SEP|
      i_field     = for_field
      i_value     = decimal_sep ).

    r_res = me.

  ENDMETHOD.


  METHOD enclosing.

    _assign_attribute(
      i_attribute = |ENCLOSING|
      i_field     = for_field
      i_value     = enclosing ).

    r_res = me.

  ENDMETHOD.


  METHOD END_OF_LINE.

    _configuration-end_of_line = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD escapator.

    _assign_attribute(
      i_attribute = |ESCAPATOR|
      i_field     = for_field
      i_value     = CONV #( escapator ) ).

    r_res = me.

  ENDMETHOD.


  METHOD field.

    r_res = VALUE #( _catalog[ table_field = i_name ]-field_obj OPTIONAL ).

    IF r_res IS INITIAL.

      r_res = NEW #( ).

      r_res->config( CORRESPONDING #( _configuration ) ).

      APPEND VALUE #(
        table_field = i_name
        field_obj   = r_res ) TO _catalog.

    ENDIF.

  ENDMETHOD.


  METHOD get_first_line.

    r_header = _mappings.

  ENDMETHOD.


  METHOD get_validation_fails.

    r_validation_fails = _validation_fails.

  ENDMETHOD.


  METHOD header.

    _configuration-header = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD HEADER_DESC.

    _configuration-header_desc = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD KEEP_INIT.

    _assign_attribute(
      i_attribute = |KEEP_INITIAL|
      i_field     = for_field
      i_value     = keep_init ).

    r_res = me.

  ENDMETHOD.


  METHOD mappings.

    _configuration-mappings = i_mappings.

    r_res = me.

  ENDMETHOD.


  METHOD read_csv.

    CREATE DATA _table_ref LIKE ct_table.

    GET REFERENCE OF ct_table INTO _table_ref.

    _create_catalog( ct_table ).

    LOOP AT it_csv INTO DATA(csv).

      _current_row = sy-tabix.

      CHECK _configuration-header EQ abap_false OR sy-tabix > 1.

      _csv_to_line( csv ).

    ENDLOOP.

  ENDMETHOD.


  METHOD separator.

    _configuration-separator = i_par.

    r_res = me.

  ENDMETHOD.


  METHOD THOUSAND_SEP.

    _assign_attribute(
      i_attribute = |THOUSAND_SEP|
      i_field     = for_field
      i_value     = thousand_sep ).

    r_res = me.

  ENDMETHOD.


  METHOD time_format.

    _assign_attribute(
      i_attribute = |TIME_FORMAT|
      i_field     = for_field
      i_value     = time_format ).

    r_res = me.

  ENDMETHOD.


  METHOD to_string.

    LOOP AT i_tab INTO DATA(row).

      r_res = COND #( WHEN sy-tabix EQ 1 THEN row ELSE |{ r_res }{ i_eol }{ row }| ).

    ENDLOOP.

  ENDMETHOD.


  METHOD to_table_string.

    CLEAR r_res.
    SPLIT i_str AT i_eol INTO TABLE r_res.

  ENDMETHOD.


  METHOD _assign_attribute.

    IF i_field IS INITIAL.
      ASSIGN COMPONENT i_attribute OF STRUCTURE _configuration TO FIELD-SYMBOL(<val>).
      <val> = i_value.
      RETURN.
    ENDIF.

    READ TABLE _fields_attributes ASSIGNING FIELD-SYMBOL(<field>) WITH KEY name = i_field.
    IF sy-subrc EQ 0.

      ASSIGN COMPONENT i_attribute OF STRUCTURE <field> TO <val>.
      <val> = i_value.

    ELSE.

      DATA(field_attr) = CORRESPONDING ty_fields_attributes( _configuration ).
      field_attr-name  = i_field.

      ASSIGN COMPONENT i_attribute OF STRUCTURE field_attr TO <val>.
      <val> = i_value.

      APPEND field_attr TO _fields_attributes.

    ENDIF.

  ENDMETHOD.


  METHOD _config_fields.

    DATA(fields) = _tab_desc->get_fields( ).

    LOOP AT fields INTO DATA(field).

      DATA(conf) = COND ty_field_conf(
        WHEN line_exists( _fields_attributes[ name = field-name ] )
          THEN CORRESPONDING ty_field_conf( _fields_attributes[ name = field-name ] )
        ELSE CORRESPONDING ty_field_conf( _configuration ) ).

      field-field->config( conf ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _create_catalog.

    DATA field_obj TYPE REF TO ztbox_cl_fieldesc.

    DATA(line)        = _get_line( it_table ).
    DATA(components)  = line->get_components( ).

    LOOP AT components INTO DATA(comp).

      DATA(ix) = sy-tabix.

      DATA(elem)      = CAST cl_abap_elemdescr( line->get_component_type( comp-name ) ).
      DATA(type_cat)  = VALUE #( _types_map[ typekind = elem->type_kind ]-type_cat OPTIONAL ).

      CLEAR field_obj.
      field_obj = COND #(
        WHEN type_cat EQ cl_abap_elemdescr=>typekind_date   THEN NEW ztbox_cl_fieldesc_date( )
        WHEN type_cat EQ cl_abap_elemdescr=>typekind_time   THEN NEW ztbox_cl_fieldesc_time( )
        WHEN type_cat EQ cl_abap_elemdescr=>typekind_packed THEN NEW ztbox_cl_fieldesc_numb( )
        ELSE NEW ztbox_cl_fieldesc_char( ) ).

      field_obj->assign_element( elem ).
      field_obj->config( CORRESPONDING #( _configuration ) ).

      READ TABLE _catalog ASSIGNING FIELD-SYMBOL(<cat>) WITH KEY table_field = comp-name.
      IF sy-subrc EQ 0.

        field_obj->copy( <cat>-field_obj ).
        <cat>-field_obj = field_obj.
        <cat>-field_obj->assign_element( elem ).
        <cat>-csv_position  = COND #( WHEN <cat>-field_obj->_csv_position IS NOT INITIAL THEN <cat>-field_obj->_csv_position ELSE ix ).
        <cat>-priority      = COND #( WHEN <cat>-field_obj->_csv_position IS NOT INITIAL THEN abap_true ELSE abap_false ).

      ELSE.

        APPEND VALUE #(
          table_field   = comp-name
          csv_position  = ix
          field_obj     = field_obj ) TO _catalog.

      ENDIF.

    ENDLOOP.

    CLEAR ix.
    LOOP AT _catalog ASSIGNING <cat> WHERE priority IS INITIAL.
      ADD 1 TO ix.
      <cat>-csv_position = ix.
    ENDLOOP.

    SORT _catalog BY csv_position ASCENDING priority DESCENDING.

  ENDMETHOD.


  METHOD _csv_to_line.

    DATA line_ref TYPE REF TO data.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.
    ASSIGN _table_ref->* TO <tab>.

    CREATE DATA line_ref LIKE LINE OF <tab>.
    ASSIGN line_ref->* TO FIELD-SYMBOL(<row>).

    DATA(values) = _get_values( iv_csv ).

    LOOP AT _catalog INTO DATA(cat).

      CHECK cat-field_obj->_exclude EQ abap_false.
      CHECK cat-field_obj->_include EQ abap_true OR _configuration-include_only EQ abap_false.

      ASSIGN COMPONENT cat-table_field OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
      CHECK sy-subrc EQ 0.

      DATA(val_str) = VALUE #( values[ name = cat-table_field ]-raw_data OPTIONAL ).

      TRY.
          <val> = cat-field_obj->read( val_str ).
        CATCH zcx_tbox_fieldesc INTO DATA(x_field).
          APPEND VALUE #(
            row         = _current_row
            col         = cat-csv_position
            table_field = cat-table_field
            raw_value   = val_str
            sap_value   = x_field->sap_value
            method_fail = x_field->method ) TO _validation_fails.
      ENDTRY.

    ENDLOOP.

    INSERT <row> INTO TABLE <tab>.

  ENDMETHOD.


  METHOD _get_header.

    LOOP AT _catalog INTO DATA(cat).

      CHECK cat-field_obj->_exclude EQ abap_false.
      CHECK cat-field_obj->_include EQ abap_true OR _configuration-include_only EQ abap_false.

      DATA(header_txt) = COND #(
        WHEN _configuration-header_desc EQ abap_true AND cat-field_obj->_label IS NOT INITIAL
          THEN cat-field_obj->_label
        ELSE cat-table_field ).

      DATA(val) = COND string(
        WHEN _configuration-enclosing IS INITIAL
          THEN header_txt
          ELSE |{ _configuration-enclosing }{ header_txt }{ _configuration-enclosing }| ).

      r_header = COND #(
        WHEN r_header IS INITIAL
          THEN val
        ELSE |{ r_header }{ _configuration-separator }{ val }| ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _get_line.

    DATA(tdesc) = cl_abap_typedescr=>describe_by_data( it_table ).

    CHECK tdesc->kind EQ cl_abap_typedescr=>kind_table.

    DATA(table_desc)  = CAST cl_abap_tabledescr( tdesc ).
    DATA(line_type)   = table_desc->get_table_line_type( ).

    CHECK
      line_type->type_kind EQ cl_abap_typedescr=>typekind_struct1 OR
      line_type->type_kind EQ cl_abap_typedescr=>typekind_struct2.

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


  METHOD _line_to_csv.

    LOOP AT _catalog INTO DATA(cat).

      CHECK cat-field_obj->_exclude EQ abap_false.
      CHECK cat-field_obj->_include EQ abap_true OR _configuration-include_only EQ abap_false.

      ASSIGN COMPONENT cat-table_field OF STRUCTURE is_line TO FIELD-SYMBOL(<val>).
      CHECK sy-subrc EQ 0.

      DATA(val) = cat-field_obj->write( <val> ).

      r_csv = COND #(
        WHEN r_csv IS INITIAL
          THEN val
        ELSE |{ r_csv }{ _configuration-separator }{ val }| ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _parse_line.

    DATA tot_enc TYPE i.

    DATA(line)          = i_line.
    DATA(combine)       = abap_false.
    DATA(val_combined)  = VALUE string( ).

    IF _configuration-escapator IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF _configuration-escapator IN line WITH _configuration-enclosing.
    ENDIF.

    SPLIT line AT _configuration-separator INTO TABLE DATA(values).

    IF _configuration-enclosing IS INITIAL.
      r_values = values.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <val> TYPE string.

    LOOP AT values INTO DATA(value).

      FIND ALL OCCURRENCES OF _configuration-enclosing IN value MATCH COUNT DATA(c_enc).
      tot_enc = tot_enc + c_enc.

      CASE tot_enc MOD 2.

        WHEN 0.
          IF combine EQ abap_true.
            val_combined = |{ val_combined }{ _configuration-separator }{ value }|.
            value = val_combined.
          ENDIF.

          IF _configuration-escapator IS NOT INITIAL.
            REPLACE ALL OCCURRENCES OF |{ _configuration-escapator }{ _configuration-enclosing }| IN value WITH _configuration-enclosing.
          ENDIF.

          REPLACE FIRST OCCURRENCE OF _configuration-enclosing IN value WITH space.

          IF value IS NOT INITIAL.

            DATA(val_last) = substring( val = value off = strlen( value ) - 1 len = 1 ).

            IF val_last EQ _configuration-enclosing.
              value = substring( val = value off = 0 len = strlen( value ) - 1 ).
            ENDIF.

          ENDIF.

          APPEND value TO r_values.
          CLEAR: value, val_combined, combine.

        WHEN OTHERS.
          combine = abap_true.
          val_combined = COND #( WHEN val_combined IS INITIAL THEN value ELSE |{ val_combined }{ _configuration-separator }{ value }| ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD _reorder_fields.

    LOOP AT _catalog ASSIGNING FIELD-SYMBOL(<cat>).

      <cat>-csv_position  = COND #( WHEN <cat>-field_obj->_csv_position IS NOT INITIAL THEN <cat>-field_obj->_csv_position ELSE <cat>-csv_position ).
      <cat>-priority      = COND #( WHEN <cat>-field_obj->_csv_position IS NOT INITIAL THEN abap_true ELSE abap_false ).

    ENDLOOP.

    SORT _catalog BY csv_position ASCENDING priority DESCENDING.

  ENDMETHOD.


  METHOD _set_line_desc.

    DATA(tdesc) = cl_abap_typedescr=>describe_by_data( it_table ).

    CHECK tdesc->kind EQ cl_abap_typedescr=>kind_table.

    DATA(table_desc)  = CAST cl_abap_tabledescr( tdesc ).
    DATA(line_type)   = table_desc->get_table_line_type( ).

    CHECK
      line_type->type_kind EQ cl_abap_typedescr=>typekind_struct1 OR
      line_type->type_kind EQ cl_abap_typedescr=>typekind_struct2.

    _line_desc = CAST cl_abap_structdescr( line_type ).

  ENDMETHOD.


  METHOD _set_mappings.

    CHECK _configuration-mappings IS INITIAL.

    LOOP AT _catalog INTO DATA(cat).

      _configuration-mappings = VALUE #( BASE _configuration-mappings
        (   csv_position   = sy-tabix
            table_field    = cat-table_field ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_tabinfo.

*    _set_line_desc( it_table ).
*
*    LOOP AT _line_desc->components INTO DATA(comp).
*
*      DATA(ix) = sy-tabix.
*
*      DATA(csv_field) = field( comp-name ).
*
*      csv_field->order( ix ).
*
*      DATA(elem_desc) = CAST cl_abap_elemdescr( _line_desc->get_component_type( comp-name ) ).
*      csv_field->char_reference( elem_desc ).
*      csv_field->type_category( VALUE #( _types_map[ typekind = comp-type_kind ]-type_cat OPTIONAL ) ).
*
*      csv_field->_set_transformation( ).
*
*    ENDLOOP.

  ENDMETHOD.


  METHOD _set_types_map.

    _types_map = VALUE #(
      ( typekind  = cl_abap_typedescr=>typekind_char
        type_cat  = cl_abap_typedescr=>typekind_char )
      ( typekind  = cl_abap_typedescr=>TYPEKIND_num
        type_cat  = cl_abap_typedescr=>typekind_char )
      ( typekind  = cl_abap_typedescr=>TYPEKIND_numeric
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
      ( typekind  = cl_abap_typedescr=>TYPEKIND_DECFLOAt16
        type_cat  = cl_abap_typedescr=>typekind_packed )
      ( typekind  = cl_abap_typedescr=>typekind_decfloat34
        type_cat  = cl_abap_typedescr=>typekind_packed )
      ( typekind  = cl_abap_typedescr=>typekind_float
        type_cat  = cl_abap_typedescr=>typekind_packed )
      ( typekind  = cl_abap_typedescr=>TYPEKIND_packed
        type_cat  = cl_abap_typedescr=>typekind_packed )
      ( typekind  = cl_abap_typedescr=>TYPEKIND_int
        type_cat  = cl_abap_typedescr=>TYPEKIND_int )
      ( typekind  = cl_abap_typedescr=>TYPEKIND_int1
        type_cat  = cl_abap_typedescr=>TYPEKIND_int )
      ( typekind  = cl_abap_typedescr=>TYPEKIND_int2
        type_cat  = cl_abap_typedescr=>TYPEKIND_int )
      ( typekind  = cl_abap_typedescr=>TYPEKIND_int8
        type_cat  = cl_abap_typedescr=>TYPEKIND_int )
      ( typekind  = cl_abap_typedescr=>TYPEKIND_hex
        type_cat  = cl_abap_typedescr=>TYPEKIND_xstring )
      ( typekind  = cl_abap_typedescr=>typekind_xsequence
        type_cat  = cl_abap_typedescr=>TYPEKIND_xstring )
      ( typekind  = cl_abap_typedescr=>TYPEKIND_xstring
        type_cat  = cl_abap_typedescr=>TYPEKIND_xstring ) ).

  ENDMETHOD.
ENDCLASS.
