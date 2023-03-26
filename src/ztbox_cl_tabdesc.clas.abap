class ZTBOX_CL_TABDESC definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_fields,
      pos   TYPE i,
      name  TYPE name_feld,
      field TYPE REF TO ztbox_cl_fieldesc,
    END OF ty_fields .
  types:
    ty_fields_t TYPE TABLE OF ty_fields WITH DEFAULT KEY .
  types:
    BEGIN OF ty_typekind_map,
      typekind TYPE abap_typekind,
      type_cat TYPE c LENGTH 1,
    END OF ty_typekind_map .
  types:
    ty_typekind_map_t TYPE TABLE OF ty_typekind_map WITH DEFAULT KEY .

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR
    importing
      !I_TABLE type ANY TABLE .
  class-methods GET_TYPE_KIND_MAP
    returning
      value(R_TYPE_KIND_MAP) type TY_TYPEKIND_MAP_T .
  methods GET_LINE_REF
    returning
      value(R_LINE) type ref to DATA .
  methods GET_FIELDS
    returning
      value(R_FIELDS) type TY_FIELDS_T .
  methods GET_FIELD
    importing
      !I_NAME type NAME_FELD
    returning
      value(R_FIELD) type ref to ZTBOX_CL_FIELDESC .
protected section.
private section.

  data _TAB_REF type ref to DATA .
  data _ROW_REF type ref to DATA .
  data:
    _fields  TYPE TABLE OF ty_fields WITH KEY pos name .
  class-data _TYPE_KIND_MAP type TY_TYPEKIND_MAP_T .

  methods _SET_TABLE
    importing
      !I_TABLE type ANY TABLE .
  class-methods _SET_TYPE_KIND_MAP .
ENDCLASS.



CLASS ZTBOX_CL_TABDESC IMPLEMENTATION.


  METHOD class_constructor.

    _set_type_kind_map( ).

  ENDMETHOD.


  METHOD constructor.

    _set_table( i_table ).

  ENDMETHOD.


  METHOD get_field.

    r_field = VALUE #( _fields[ name = i_name ]-field OPTIONAL ).

  ENDMETHOD.


  METHOD get_fields.

    r_fields = _fields.

  ENDMETHOD.


  METHOD get_line_ref.

    r_line = _row_ref.

  ENDMETHOD.


  METHOD get_type_kind_map.

    r_type_kind_map = _type_kind_map.

  ENDMETHOD.


  METHOD _set_table.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.

    DATA(tdesc) = cl_abap_typedescr=>describe_by_data( i_table ).

    CHECK tdesc->kind EQ cl_abap_typedescr=>kind_table.

    DATA(table_desc)  = CAST cl_abap_tabledescr( tdesc ).
    DATA(line_type)   = table_desc->get_table_line_type( ).

    CHECK
      line_type->type_kind EQ cl_abap_typedescr=>typekind_struct1 OR
      line_type->type_kind EQ cl_abap_typedescr=>typekind_struct2.

    DATA(line_desc) = CAST cl_abap_structdescr( line_type ).

    DATA(components) = line_desc->get_components( ).

    LOOP AT components INTO DATA(comp).

      APPEND VALUE #(
        pos   = sy-tabix
        name  = comp-name
        field = ztbox_cl_fieldesc=>create( CAST cl_abap_elemdescr( line_desc->get_component_type( comp-name ) ) ) ) TO _fields.

    ENDLOOP.

    CREATE DATA _tab_ref LIKE i_table.
    ASSIGN _tab_ref->* TO <tab>.

    <tab> = i_table.

    CREATE DATA _row_ref LIKE LINE OF <tab>.

  ENDMETHOD.


  METHOD _set_type_kind_map.

    _type_kind_map = VALUE #(
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
        type_cat  = cl_abap_typedescr=>typekind_int )
      ( typekind  = cl_abap_typedescr=>typekind_int1
        type_cat  = cl_abap_typedescr=>typekind_int )
      ( typekind  = cl_abap_typedescr=>typekind_int2
        type_cat  = cl_abap_typedescr=>typekind_int )
      ( typekind  = cl_abap_typedescr=>typekind_int8
        type_cat  = cl_abap_typedescr=>typekind_int )
      ( typekind  = cl_abap_typedescr=>typekind_hex
        type_cat  = cl_abap_typedescr=>typekind_xstring )
      ( typekind  = cl_abap_typedescr=>typekind_xsequence
        type_cat  = cl_abap_typedescr=>typekind_xstring )
      ( typekind  = cl_abap_typedescr=>typekind_xstring
        type_cat  = cl_abap_typedescr=>typekind_xstring ) ).

  ENDMETHOD.
ENDCLASS.
