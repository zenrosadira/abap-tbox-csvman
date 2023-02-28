class ZTBOX_CL_CSVMAN definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_config,
        header      TYPE flag,
        dquotes     TYPE string,
        separator   TYPE string,
        convexit    TYPE flag,
        keep_spaces TYPE flag,
        mapping     TYPE flag,
      END OF ty_config .
  types:
    BEGIN OF ty_order,
             pos   TYPE int4,
             field TYPE feldname,
           END OF ty_order .
  types:
    ty_order_t TYPE TABLE OF ty_order WITH DEFAULT KEY .

  methods CONFIGURE
    importing
      !I_HEADER type FLAG optional
      !I_DQUOTES type STRING optional
      !I_SEPARATOR type STRING optional
      !I_CONVEXIT type FLAG optional
      !I_KEEP_SPACES type FLAG optional
      !I_MAPPING type FLAG optional
      !I_EXCLUDE_FIELDS type STRING_TABLE optional
      !I_ORDER type TY_ORDER_T optional .
  methods READ_CSV
    importing
      !IT_CSV type STRING_TABLE
    changing
      !CT_TABLE type TABLE .
  methods CREATE_CSV
    importing
      !IT_TABLE type TABLE
    returning
      value(R_CSV) type STRING_TABLE .
  methods CONSTRUCTOR .
  methods GET_FIRST_LINE
    returning
      value(R_HEADER) type STRING_TABLE .
protected section.
PRIVATE SECTION.

  TYPES:
    BEGIN OF ty_tabinfo,
      field     TYPE feldname,
      rollname  TYPE string,
      data_type TYPE string,
      outlen    TYPE int4,
      decimals  TYPE int4,
      convexit  TYPE string,
      type_kind TYPE c LENGTH 1,
      order     TYPE int4,
    END OF ty_tabinfo .
  TYPES:
    ty_tabinfo_t TYPE TABLE OF ty_tabinfo WITH KEY field .

  DATA _configuration TYPE ty_config .
  DATA _tab_info TYPE ty_tabinfo_t .
  DATA _csv_to_read TYPE string_table .
  DATA _table_ref TYPE REF TO data .
  DATA _mappings TYPE string_table .
  DATA _exclude_fields TYPE string_table .
  DATA _orders TYPE ty_order_t .

  METHODS _line_to_csv
    IMPORTING
      !is_line     TYPE any
    RETURNING
      VALUE(r_csv) TYPE string .
  METHODS _set_tabinfo
    IMPORTING
      !it_table TYPE table .
  METHODS _csv_to_line
    IMPORTING
      !iv_csv TYPE string .
  METHODS _get_header
    RETURNING
      VALUE(r_header) TYPE string .
  METHODS _mapping
    IMPORTING
      !i_ix       TYPE int4
    RETURNING
      VALUE(r_jx) TYPE int4 .
ENDCLASS.



CLASS ZTBOX_CL_CSVMAN IMPLEMENTATION.


  METHOD configure.

    IF i_header IS SUPPLIED.
      _configuration-header = i_header.
    ENDIF.

    IF i_dquotes IS SUPPLIED.
      _configuration-dquotes = i_dquotes.
    ENDIF.

    IF i_separator IS SUPPLIED.
      _configuration-separator = i_separator.
    ENDIF.

    IF i_convexit IS SUPPLIED.
      _configuration-convexit = i_convexit.
    ENDIF.

    IF i_keep_spaces IS SUPPLIED.
      _configuration-keep_spaces = i_keep_spaces.
    ENDIF.

    IF i_mapping IS SUPPLIED.
      _configuration-mapping = i_mapping.
    ENDIF.

    IF i_exclude_fields IS SUPPLIED.
      _exclude_fields = i_exclude_fields.
    ENDIF.

    IF i_order IS SUPPLIED.
      _orders = i_order.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    _configuration = VALUE #(
      header      = abap_true
      separator   = |;|
      convexit    = abap_true
      keep_spaces = abap_true
      dquotes     = || ).

  ENDMETHOD.


  METHOD create_csv.

    _set_tabinfo( it_table ).

    IF _configuration-header EQ abap_true.
      r_csv = VALUE #( ( _get_header( ) ) ).
    ENDIF.

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<fs_line>).

      r_csv = VALUE #( BASE r_csv ( _line_to_csv( <fs_line> ) ) ).

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

    LOOP AT it_csv INTO DATA(lv_csv).

      IF _configuration-header EQ abap_true.

        IF sy-tabix EQ 1.
          SPLIT lv_csv AT _configuration-separator INTO TABLE _mappings.
        ENDIF.

        CHECK sy-tabix > 1.

      ENDIF.

      _csv_to_line( lv_csv ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _csv_to_line.

    DATA lv_csv     TYPE string.
    DATA lv_val     TYPE string.
    DATA lv_ix      TYPE i.
    DATA lv_jx      TYPE i.
    DATA lv_start   TYPE i.
    DATA lv_end     TYPE i.
    DATA lv_off     TYPE i.

    lv_csv = iv_csv.

    FIELD-SYMBOLS <fs_tab> TYPE ANY TABLE.

    DATA lo_line        TYPE REF TO data.
    ASSIGN _table_ref->* TO <fs_tab>.
    CREATE DATA lo_line LIKE LINE OF <fs_tab>.
    ASSIGN lo_line->* TO FIELD-SYMBOL(<fs_row>).

    IF _configuration-dquotes IS INITIAL.
      SPLIT lv_csv AT _configuration-separator INTO TABLE DATA(lt_values).
    ELSE.
      FIND ALL OCCURRENCES OF _configuration-dquotes IN lv_csv RESULTS DATA(lt_res).
    ENDIF.

    LOOP AT _tab_info INTO DATA(ls_info).
      lv_ix = sy-tabix.

      ASSIGN COMPONENT ls_info-field OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_val>).
      CHECK sy-subrc EQ 0.

      lv_jx = _mapping( lv_ix ).
      CHECK lv_jx > 0.

      IF lt_values IS NOT INITIAL.
        lv_val = VALUE #( lt_values[ lv_jx ] OPTIONAL ).
      ELSE.
        CLEAR: lv_start, lv_end.
        lv_start  = VALUE #( lt_res[ ( lv_jx * 2 ) - 1 ]-offset OPTIONAL ).
        lv_end    = VALUE #( lt_res[ lv_jx * 2 ]-offset OPTIONAL ).

        ADD 1 TO lv_start.

        CLEAR lv_off.
        lv_off = lv_end - lv_start.

        lv_val = lv_csv+lv_start(lv_off).
      ENDIF.

      CONDENSE lv_val.

      IF lv_val IS NOT INITIAL.
        cl_rsan_ut_conversion_exit=>convert_to_intern(
            EXPORTING
              i_fieldinfo      = VALUE #(
                inttype   = ls_info-type_kind
                convexit  = ls_info-convexit
                decimals  = ls_info-decimals
                outputlen = ls_info-outlen
                length    = ls_info-outlen
                datatype  = ls_info-data_type )
              i_external_value  = lv_val
          IMPORTING
              e_internal_value  = lv_val ).
      ENDIF.

      <fs_val> = lv_val.

    ENDLOOP.

    INSERT <fs_row> INTO TABLE <fs_tab>.

  ENDMETHOD.


  METHOD _get_header.

    DATA lv_val TYPE string.

    LOOP AT _tab_info INTO DATA(ls_info).

      CLEAR lv_val.
      lv_val = COND #( WHEN _configuration-dquotes IS INITIAL THEN ls_info-field ELSE |{ _configuration-dquotes }{ ls_info-field }{ _configuration-dquotes }| ).

      r_header = COND #( WHEN r_header IS INITIAL THEN lv_val ELSE |{ r_header }{ _configuration-separator }{ lv_val }| ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _line_to_csv.

    DATA lv_val        TYPE string.
    DATA lv_val_spaces TYPE c LENGTH 3000.
    DATA lv_spaces     TYPE i.

    LOOP AT _tab_info INTO DATA(ls_info).

      ASSIGN COMPONENT ls_info-field OF STRUCTURE is_line TO FIELD-SYMBOL(<fs_val>).
      CHECK sy-subrc EQ 0.

      lv_val = <fs_val>.

      IF <fs_val> IS INITIAL.
        CLEAR lv_val.
      ENDIF.

      IF _configuration-convexit IS NOT INITIAL AND lv_val IS NOT INITIAL.
        cl_rsan_ut_conversion_exit=>convert_to_extern(
          EXPORTING
            i_fieldinfo      = VALUE #(
              inttype   = ls_info-type_kind
              convexit  = ls_info-convexit
              decimals  = ls_info-decimals
              outputlen = ls_info-outlen
              datatype  = ls_info-data_type )
            i_internal_value = lv_val
        IMPORTING
            e_external_value = lv_val ).
      ENDIF.

      CONDENSE lv_val.

      IF _configuration-keep_spaces EQ abap_true.
        CLEAR: lv_val_spaces, lv_spaces.

        lv_spaces = ls_info-outlen - strlen( lv_val ).
        lv_val_spaces+lv_spaces = lv_val.

        lv_val = lv_val_spaces.
      ENDIF.

      IF _configuration-dquotes IS NOT INITIAL.
        lv_val = |{ _configuration-dquotes }{ lv_val }{ _configuration-dquotes }|.
      ENDIF.

      r_csv = COND #( WHEN r_csv IS INITIAL THEN lv_val ELSE |{ r_csv }{ _configuration-separator }{ lv_val }| ).

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

    DATA(lo_tdesc) = cl_abap_typedescr=>describe_by_data( it_table ).
    CHECK lo_tdesc->kind EQ cl_abap_typedescr=>kind_table.

    DATA(lo_table) = CAST cl_abap_tabledescr( lo_tdesc ).

    DATA(lo_ldesc) = lo_table->get_table_line_type( ).
    CHECK lo_ldesc->type_kind EQ cl_abap_typedescr=>typekind_struct2 OR lo_ldesc->type_kind EQ cl_abap_typedescr=>typekind_struct1.

    DATA(lo_line) = CAST cl_abap_structdescr( lo_ldesc ).

    LOOP AT lo_line->components INTO DATA(ls_comp).

      CHECK NOT line_exists( _exclude_fields[ table_line = ls_comp-name ] ).

      DATA(lo_elem) = CAST cl_abap_elemdescr( lo_line->get_component_type( ls_comp-name ) ).

      _tab_info = VALUE #( BASE _tab_info (
        field     = ls_comp-name
        decimals  = ls_comp-decimals
        type_kind = ls_comp-type_kind
        order     = VALUE #( _orders[ field = ls_comp-name ]-pos OPTIONAL )
        data_type = COND #( WHEN ls_comp-type_kind EQ 'C' THEN 'CHAR'
                            WHEN ls_comp-type_kind EQ 'N' THEN 'NUMC'
                            WHEN ls_comp-type_kind EQ 'D' THEN 'DATS'
                            WHEN ls_comp-type_kind EQ 'T' THEN 'TIMS' )
        rollname  = lo_elem->help_id
        outlen    = lo_elem->output_length
        convexit  = COND #( WHEN lo_elem->edit_mask IS NOT INITIAL THEN lo_elem->edit_mask+2 ELSE space ) ) ).

    ENDLOOP.

    IF _orders IS NOT INITIAL.
      SORT _tab_info BY order.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
