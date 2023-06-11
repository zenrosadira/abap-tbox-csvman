CLASS lcl_utils DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      is_elem
        IMPORTING i_data    TYPE data                     RETURNING VALUE(r) TYPE abap_bool,
      is_table_of_elem
        IMPORTING i_data    TYPE data                     RETURNING VALUE(r) TYPE abap_bool,
      is_table_of_one_component
        IMPORTING i_data    TYPE data                     RETURNING VALUE(r) TYPE abap_bool,
      create_type_table_from_catalog
        IMPORTING i_catalog TYPE abap_component_view_tab  RETURNING VALUE(r) TYPE REF TO data.


ENDCLASS.

CLASS lcl_utils IMPLEMENTATION.

  METHOD is_elem.

    r = xsdbool( cl_abap_typedescr=>describe_by_data( i_data )->kind = cl_abap_typedescr=>kind_elem ).

  ENDMETHOD.

  METHOD is_table_of_elem.

    DATA(data_desc) = cl_abap_typedescr=>describe_by_data( i_data ).
    CHECK data_desc->kind = cl_abap_typedescr=>kind_table.

    DATA(tab_desc) = CAST cl_abap_tabledescr( data_desc ).
    CHECK tab_desc->get_table_line_type( )->kind = cl_abap_typedescr=>kind_elem.

    r = abap_true.

  ENDMETHOD.

  METHOD is_table_of_one_component.

    DATA(data_desc) = cl_abap_typedescr=>describe_by_data( i_data ).
    CHECK data_desc->kind = cl_abap_typedescr=>kind_table.

    DATA(tab_desc)  = CAST cl_abap_tabledescr( data_desc ).
    DATA(tab_line)  = tab_desc->get_table_line_type( ).
    CHECK tab_line->kind = cl_abap_typedescr=>kind_struct.

    DATA(struct_line) = CAST cl_abap_structdescr( tab_line ).
    DATA(components)  = struct_line->get_included_view( ).

    r = xsdbool( lines( components ) = 1 AND components[ 1 ]-type->kind = cl_abap_typedescr=>kind_elem ).

  ENDMETHOD.

  METHOD create_type_table_from_catalog.

    DATA(line_type) = cl_abap_structdescr=>get( CORRESPONDING #( i_catalog ) ).

    DATA(tab_type) = cl_abap_tabledescr=>get(
      p_line_type = line_type
      p_key_kind  = cl_abap_tabledescr=>keydefkind_empty ).

    CREATE DATA r TYPE HANDLE tab_type.

  ENDMETHOD.

ENDCLASS.
