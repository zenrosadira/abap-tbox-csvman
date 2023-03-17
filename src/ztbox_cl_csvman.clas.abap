class ZTBOX_CL_CSVMAN definition
  public
  final
  create public

  global friends ZTBOX_CL_CSVMAN_FIELD .

public section.

  types:
    BEGIN OF ty_order,
        pos   TYPE i,
        field TYPE name_feld,
      END OF ty_order .
  types:
    ty_order_t TYPE TABLE OF ty_order WITH DEFAULT KEY .
  types:
    BEGIN OF ty_core_config,
        enclosing      TYPE string,
        convexit       TYPE flag,
        alignment      TYPE i,
        keep_initials  TYPE flag,
        country_format TYPE land1,
      END OF ty_core_config .
  types:
    BEGIN OF ty_field_conf.
    TYPES field  TYPE name_feld.
    TYPES config TYPE ty_core_config.
    TYPES: END OF ty_field_conf .
  types:
    ty_field_conf_t TYPE TABLE OF ty_field_conf WITH DEFAULT KEY .
  types:
    BEGIN OF ty_config.
        INCLUDE TYPE ty_core_config.
    TYPES header TYPE flag.
    TYPES separator      TYPE string.
    TYPES escapator      TYPE string.
    TYPES mapping        TYPE flag.
    TYPES exclude_fields TYPE name_feld_tty.
    TYPES fields_order   TYPE ty_order_t.
    TYPES end_of_line    TYPE string.
    TYPES fields_config  TYPE ty_field_conf_t.
    TYPES include_only   type flag.
    TYPES: END OF ty_config .

  class-methods CLASS_CONSTRUCTOR .
  methods CONFIGURE
    importing
      !I_HEADER type FLAG optional
      !I_ENCLOSING type STRING optional
      !I_ESCAPATOR type STRING optional
      !I_SEPARATOR type STRING optional
      !I_CONVEXIT type FLAG optional
      !I_ALIGNMENT type I optional
      !I_MAPPING type FLAG optional
      !I_EXCLUDE_FIELDS type NAME_FELD_TTY optional
      !I_ORDER type TY_ORDER_T optional
      !I_KEEP_INITIALS type FLAG optional
      !I_COUNTRY_FORMAT type LAND1 optional
      !I_END_OF_LINE type STRING optional
      !I_FIELDS_CONF type TY_FIELD_CONF_T optional .
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
  class-methods TO_TABLE_STRING
    importing
      !I_STR type STRING
      !I_EOL type ANY default CL_ABAP_CHAR_UTILITIES=>CR_LF
    returning
      value(R_RES) type STRING_TABLE .
  methods GET_FIELD
    importing
      !I_NAME type FELD_NAME
    returning
      value(R_RES) type ref to ZTBOX_CL_CSVMAN_FIELD .
protected section.
private section.

  types:
    BEGIN OF ty_tabinfo,
      field     TYPE feldname,
      rollname  TYPE string,
      data_type TYPE string,
      outlen    TYPE int4,
      decimals  TYPE int4,
      convexit  TYPE string,
      type_kind TYPE c LENGTH 1,
      type_cat  TYPE c LENGTH 1,
      order     TYPE int4,
      c_ref     TYPE REF TO data,
    END OF ty_tabinfo .
  types:
    ty_tabinfo_t TYPE TABLE OF ty_tabinfo WITH KEY field .
  types:
    BEGIN OF ty_typekind_map,
      typekind TYPE abap_typekind,
      type_cat TYPE c LENGTH 1,
    END OF ty_typekind_map .
  types:
    ty_typekind_map_t TYPE TABLE OF ty_typekind_map WITH DEFAULT KEY .
  types:
    BEGIN OF ty_fields,
      name      TYPE feld_Name,
      position  TYPE i,
      field_obj TYPE REF TO ztbox_cl_csvman_field,
    END OF ty_fields .
  types:
    ty_fields_t TYPE TABLE OF ty_fields .

  data _CONFIGURATION type TY_CONFIG .
  data _TAB_INFO type TY_TABINFO_T .
  data _CSV_TO_READ type STRING_TABLE .
  data _TABLE_REF type ref to DATA .
  data _MAPPINGS type STRING_TABLE .
  class-data _TYPES_MAP type TY_TYPEKIND_MAP_T .
  data _FIELDS_CONF type TY_FIELD_CONF .
  data _FIELDS type TY_FIELDS_T .

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
  methods _MAPPING
    importing
      !I_IX type INT4
    returning
      value(R_JX) type INT4 .
  methods _DATE_FORMAT
    importing
      !I_DATE type DATUM
      !I_COUNTRY type LAND1 optional
    returning
      value(R_RES) type STRING .
  methods _NUM_FORMAT
    importing
      !I_NUM type NUMERIC
      !I_COUNTRY type LAND1 optional
    returning
      value(R_RES) type STRING .
  class-methods _SET_TYPES_MAP .
  methods _REORDER_FIELDS .
ENDCLASS.



CLASS ZTBOX_CL_CSVMAN IMPLEMENTATION.


  METHOD configure.

    _configuration-header         = COND #( WHEN i_header         IS SUPPLIED THEN i_header         ELSE _configuration-header ).
    _configuration-enclosing      = COND #( WHEN i_enclosing      IS SUPPLIED THEN i_enclosing      ELSE _configuration-enclosing ).
    _configuration-separator      = COND #( WHEN i_separator      IS SUPPLIED THEN i_separator      ELSE _configuration-separator ).
    _configuration-convexit       = COND #( WHEN i_convexit       IS SUPPLIED THEN i_convexit       ELSE _configuration-convexit ).
    _configuration-alignment      = COND #( WHEN i_alignment      IS SUPPLIED THEN i_alignment      ELSE _configuration-alignment ).
    _configuration-keep_initials  = COND #( WHEN i_keep_initials  IS SUPPLIED THEN i_keep_initials  ELSE _configuration-keep_initials ).
    _configuration-country_format = COND #( WHEN i_country_format IS SUPPLIED THEN i_country_format ELSE _configuration-country_format ).
    _configuration-mapping        = COND #( WHEN i_mapping        IS SUPPLIED THEN i_mapping        ELSE _configuration-mapping ).
    _configuration-end_of_line    = COND #( WHEN i_end_of_line    IS SUPPLIED THEN I_end_of_line    ELSE _configuration-end_of_line ).

    _configuration-exclude_fields = i_exclude_fields.
    _configuration-fields_order   = i_order.
    _configuration-fields_config  = i_fields_conf.
    _configuration-escapator      = i_escapator.

  ENDMETHOD.


  METHOD constructor.

    configure(
      i_header      = abap_true
      i_separator   = CONV #( cl_abap_char_utilities=>horizontal_tab )
      i_alignment   = cl_abap_format=>a_left
      i_convexit    = abap_true
      i_end_of_line = CONV #( cl_abap_char_utilities=>cr_lf ) ).

  ENDMETHOD.


  METHOD create_csv.

    _set_tabinfo( it_table ).
    _reorder_fields( ).

    IF _configuration-header EQ abap_true.
      r_csv = |{ _get_header( ) }{ _configuration-end_of_line }|.
    ENDIF.

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<line>).

      DATA(eof) = COND #( WHEN sy-tabix NE lines( it_table ) THEN _configuration-end_of_line ELSE space ).

      r_csv = |{ r_csv }{ _line_to_csv( <line> ) }{ eof }|.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_first_line.

    r_header = _mappings.

  ENDMETHOD.


  METHOD read_csv.

    _csv_to_read = it_csv.

    CREATE DATA _table_ref LIKE ct_table.
    GET REFERENCE OF ct_table INTO _table_ref.

    _set_tabinfo( ct_table ).

    LOOP AT it_csv INTO DATA(csv).

      IF _configuration-header EQ abap_true AND sy-tabix EQ 1.
        SPLIT csv AT _configuration-separator INTO TABLE _mappings.
      ENDIF.

      _csv_to_line( csv ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _csv_to_line.

    DATA(csv) = VALUE string( ).

    csv = iv_csv.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.

    DATA line             TYPE REF TO data.
    ASSIGN _table_ref->*  TO <tab>.
    CREATE DATA line      LIKE LINE OF <tab>.
    ASSIGN line->*        TO FIELD-SYMBOL(<row>).

    IF _configuration-enclosing IS INITIAL.
      SPLIT csv AT _configuration-separator INTO TABLE DATA(values).
    ELSE.
      FIND ALL OCCURRENCES OF _configuration-enclosing IN csv RESULTS DATA(quotes).
    ENDIF.

    LOOP AT _tab_info INTO DATA(cat).
      DATA(ix) = sy-tabix.

      ASSIGN COMPONENT cat-field OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
      CHECK sy-subrc EQ 0.

      DATA(jx) = _mapping( ix ).
      CHECK jx > 0.

      IF values IS NOT INITIAL.
        DATA(val) = VALUE #( values[ jx ] OPTIONAL ).
      ELSE.

        DATA(start)  = VALUE #( quotes[ ( jx * 2 ) - 1 ]-offset OPTIONAL ).
        DATA(end)    = VALUE #( quotes[ jx * 2 ]-offset OPTIONAL ).

        ADD 1 TO start.

        DATA(off) = end - start.

        val = csv+start(off).
      ENDIF.

      CONDENSE val.

      IF val IS NOT INITIAL.
        cl_rsan_ut_conversion_exit=>convert_to_intern(
            EXPORTING
              i_fieldinfo      = VALUE #(
                inttype   = cat-type_kind
                convexit  = cat-convexit
                decimals  = cat-decimals
                outputlen = cat-outlen
                length    = cat-outlen
                datatype  = cat-data_type )
              i_external_value  = val
          IMPORTING
              e_internal_value  = val ).
      ENDIF.

      <val> = val.

    ENDLOOP.

    INSERT <row> INTO TABLE <tab>.

  ENDMETHOD.


  METHOD _get_header.

    LOOP AT _fields INTO DATA(field).

      IF    _configuration-include_only EQ abap_true
        AND field-field_obj->_field_config-include EQ abap_false
        OR  field-field_obj->_field_config-exclude EQ abap_true.

        CONTINUE.

      ENDIF.

      DATA(val) = COND string(
        WHEN _configuration-enclosing IS INITIAL
          THEN field-name
          ELSE |{ _configuration-enclosing }{ field-name }{ _configuration-enclosing }| ).

      r_header = COND #(
        WHEN r_header IS INITIAL
          THEN val
        ELSE |{ r_header }{ _configuration-separator }{ val }| ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _line_to_csv.

    LOOP AT _fields INTO DATA(field).

      ASSIGN COMPONENT field-name OF STRUCTURE is_line TO FIELD-SYMBOL(<val>).
      CHECK sy-subrc EQ 0.

***      DATA(fields_config) = field-field_obj->_field_config.
***
***      ASSIGN fields_config-c_ref->* TO FIELD-SYMBOL(<val_c>).
***
***      DATA(val) = VALUE string( ).
***
***      IF <val> IS NOT INITIAL OR fields_config-keep_initials EQ abap_true.
***
***        CASE fields_config-type_cat.
***          WHEN cl_abap_typedescr=>typekind_packed.
***            val = _num_format( i_num = <val> i_country = fields_config-country_format ).
***
***          WHEN cl_abap_typedescr=>typekind_date.
***            val = _date_format( i_date = <val> i_country = fields_config-country_format ).
***
***          WHEN OTHERS.
***            WRITE <val> TO <val_c>.
***            val = COND #( WHEN fields_config-convexit EQ abap_true THEN <val_c> ELSE <val> ).
***
***            IF _configuration-escapator IS NOT INITIAL.
***              REPLACE ALL OCCURRENCES OF _configuration-separator IN val WITH |{ _configuration-escapator }{ _configuration-separator }|.
***            ENDIF.
***
***        ENDCASE.
***
***        DATA(align) = COND #(
***          WHEN fields_config-alignment IS NOT INITIAL
***            THEN fields_config-alignment
***          ELSE cl_abap_format=>a_left ).
***
***        CLEAR <val_c>.
***        <val_c> = |{ val WIDTH = fields_config-outlen ALIGN = (align) }|.
***
***      ENDIF.
***
***      CLEAR val.
***      val = COND #(
***        WHEN _configuration-enclosing IS NOT INITIAL
***          THEN |{ fields_config-enclosing }{ <val_c> }{ fields_config-enclosing }|
***        ELSE <val_c> ).

      DATA(val) = field-field_obj->get_value( <val> ).

      r_csv = COND #(
        WHEN r_csv IS INITIAL
          THEN val
        ELSE |{ r_csv }{ _configuration-separator }{ val }| ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _mapping.

    r_jx = i_ix.

    CHECK _configuration-mapping IS NOT INITIAL.

    DATA(lv_field) = VALUE #( _tab_info[ i_ix ]-field OPTIONAL ).

    READ TABLE _mappings WITH KEY table_line = lv_field TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      r_jx = sy-tabix.
    ELSE.
      r_jx = 0.
    ENDIF.

  ENDMETHOD.


  METHOD _set_tabinfo.

    CLEAR _tab_info.

    DATA c_ref TYPE REF TO data.

    DATA(tdesc) = cl_abap_typedescr=>describe_by_data( it_table ).

    CHECK tdesc->kind EQ cl_abap_typedescr=>kind_table.

    DATA(table_desc)  = CAST cl_abap_tabledescr( tdesc ).
    DATA(line_type)   = table_desc->get_table_line_type( ).

    CHECK
      line_type->type_kind EQ cl_abap_typedescr=>typekind_struct1 OR
      line_type->type_kind EQ cl_abap_typedescr=>typekind_struct2.

    DATA(line_desc) = CAST cl_abap_structdescr( line_type ).

    LOOP AT line_desc->components INTO DATA(comp).

      DATA(csv_field) = NEW ztbox_cl_csvman_field(
        i_name  = comp-name
        i_csv   = me ).

      csv_field->order( sy-tabix ).

      DATA(ref_components) = line_desc->get_components( ).
      DATA(elem_desc) = CAST cl_abap_elemdescr( VALUE #( ref_components[ name = comp-name ]-type OPTIONAL ) ).

      csv_field->outlen( elem_desc->output_length ).
      csv_field->char_reference( elem_desc ).
      csv_field->type_category( VALUE #( _types_map[ typekind = comp-type_kind ]-type_cat OPTIONAL ) ).

      APPEND VALUE #(
        name      = comp-name
        position  = csv_field->_field_config-order
        field_obj = csv_field ) TO _fields.

    ENDLOOP.

  ENDMETHOD.


  METHOD class_constructor.

    _set_types_map( ).

  ENDMETHOD.


  METHOD get_field.

    r_res = VALUE #( _fields[ name = i_name ]-field_obj OPTIONAL ).

  ENDMETHOD.


  METHOD to_table_string.

    CLEAR r_res.
    SPLIT i_str AT i_eol INTO TABLE r_res.

  ENDMETHOD.


  METHOD _date_format.

    r_res = |{ i_date COUNTRY = i_country }|.

  ENDMETHOD.


  METHOD _num_format.

    r_res = |{ i_num COUNTRY = i_country }|.

  ENDMETHOD.


  METHOD _reorder_fields.

    LOOP AT _fields ASSIGNING FIELD-SYMBOL(<field>).

      <field>-position = <field>-field_obj->_field_config-order.

    ENDLOOP.

    SORT _fields BY position.

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
      ( typekind  = cl_abap_typedescr=>typekind_utclong
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
