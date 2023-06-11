CLASS lcl_utils DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      get_components  IMPORTING i_data  TYPE data       RETURNING VALUE(r) TYPE abap_component_view_tab,
      is_flat_table   IMPORTING i_table TYPE ANY TABLE  RETURNING VALUE(r) TYPE abap_bool.


ENDCLASS.

CLASS lcl_utils IMPLEMENTATION.

  METHOD get_components.

    DATA(data_desc) = cl_abap_typedescr=>describe_by_data( i_data ).

    CASE data_desc->kind.

      WHEN cl_abap_typedescr=>kind_table.

        DATA(tab_desc) = CAST cl_abap_tabledescr( data_desc ).
        DATA(tab_line) = tab_desc->get_table_line_type( ).

        CASE tab_line->kind.

          WHEN cl_abap_typedescr=>kind_struct.
            DATA(line_desc) = CAST cl_abap_structdescr( tab_line ).
            r = line_desc->get_included_view( ).

          WHEN OTHERS.
            r = VALUE #(
              ( name = |TABLE_LINE|
                type  = tab_line ) ).

        ENDCASE.

      WHEN cl_abap_typedescr=>kind_struct.

        DATA(struct_desc) = CAST cl_abap_structdescr( data_desc ).
        r = struct_desc->get_included_view( ).

    ENDCASE.

  ENDMETHOD.

  METHOD is_flat_table.

    DATA(data_desc)       = cl_abap_typedescr=>describe_by_data( i_table ).
    CHECK data_desc->kind = cl_abap_typedescr=>kind_table.

    DATA(tab_desc)        = CAST cl_abap_tabledescr( data_desc ).
    DATA(tab_line)        = tab_desc->get_table_line_type( ).
    CHECK tab_line->kind  = cl_abap_typedescr=>kind_struct.

    DATA(struct_line)     = CAST cl_abap_structdescr( tab_line ).
    DATA(components)      = struct_line->get_included_view( ).

    LOOP AT components INTO DATA(component).

      IF component-type->kind <> cl_abap_typedescr=>kind_elem.
        RETURN.
      ENDIF.

    ENDLOOP.

    r = abap_true.

  ENDMETHOD.

ENDCLASS.
