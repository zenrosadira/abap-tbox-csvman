CLASS zcl_tbox_csvman DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get_writer
      RETURNING VALUE(r) TYPE REF TO zcl_tbox_csv_writer.

    CLASS-METHODS get_reader
      RETURNING VALUE(r) TYPE REF TO zcl_tbox_csv_reader.

    CLASS-METHODS class_constructor.

    CLASS-METHODS get_normalized_type
      IMPORTING i_typekind TYPE clike
      RETURNING VALUE(r)   TYPE abap_typekind.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_typekind_normalization,
        typekind        TYPE c LENGTH 1,
        normalized_type TYPE c LENGTH 1,
      END OF ts_typekind_normalization.

    CLASS-DATA g_typekinds_normalization TYPE TABLE OF ts_typekind_normalization WITH KEY typekind.

    CLASS-METHODS _load_typekinds.
ENDCLASS.


CLASS zcl_tbox_csvman IMPLEMENTATION.
* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TBOX_CSVMAN=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD class_constructor.
    _load_typekinds( ).
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TBOX_CSVMAN=>GET_NORMALIZED_TYPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TYPEKIND                     TYPE        CLIKE
* | [<-()] R                              TYPE        ABAP_TYPEKIND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_normalized_type.
    r = VALUE #( g_typekinds_normalization[ typekind = i_typekind ]-normalized_type OPTIONAL ).
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TBOX_CSVMAN=>GET_READER
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_READER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_reader.
    r = NEW #( ).
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TBOX_CSVMAN=>GET_WRITER
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_WRITER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_writer.
    r = NEW #( ).
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_TBOX_CSVMAN=>_LOAD_TYPEKINDS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _load_typekinds.
    g_typekinds_normalization = VALUE #( ( typekind        = cl_abap_typedescr=>typekind_char
                                           normalized_type = cl_abap_typedescr=>typekind_char )
                                         ( typekind        = cl_abap_typedescr=>typekind_num
                                           normalized_type = cl_abap_typedescr=>typekind_char )
                                         ( typekind        = cl_abap_typedescr=>typekind_numeric
                                           normalized_type = cl_abap_typedescr=>typekind_char )
                                         ( typekind        = cl_abap_typedescr=>typekind_clike
                                           normalized_type = cl_abap_typedescr=>typekind_char )
                                         ( typekind        = cl_abap_typedescr=>typekind_csequence
                                           normalized_type = cl_abap_typedescr=>typekind_char )
                                         ( typekind        = cl_abap_typedescr=>typekind_simple
                                           normalized_type = cl_abap_typedescr=>typekind_char )
                                         ( typekind        = cl_abap_typedescr=>typekind_string
                                           normalized_type = cl_abap_typedescr=>typekind_char )
                                         ( typekind        = cl_abap_typedescr=>typekind_date
                                           normalized_type = cl_abap_typedescr=>typekind_date )
                                         ( typekind        = cl_abap_typedescr=>typekind_time
                                           normalized_type = cl_abap_typedescr=>typekind_time )
                                         ( typekind        = cl_abap_typedescr=>typekind_decfloat
                                           normalized_type = cl_abap_typedescr=>typekind_packed )
                                         ( typekind        = cl_abap_typedescr=>typekind_decfloat16
                                           normalized_type = cl_abap_typedescr=>typekind_packed )
                                         ( typekind        = cl_abap_typedescr=>typekind_decfloat34
                                           normalized_type = cl_abap_typedescr=>typekind_packed )
                                         ( typekind        = cl_abap_typedescr=>typekind_float
                                           normalized_type = cl_abap_typedescr=>typekind_packed )
                                         ( typekind        = cl_abap_typedescr=>typekind_packed
                                           normalized_type = cl_abap_typedescr=>typekind_packed )
                                         ( typekind        = cl_abap_typedescr=>typekind_int
                                           normalized_type = cl_abap_typedescr=>typekind_packed )
                                         ( typekind        = cl_abap_typedescr=>typekind_int1
                                           normalized_type = cl_abap_typedescr=>typekind_packed )
                                         ( typekind        = cl_abap_typedescr=>typekind_int2
                                           normalized_type = cl_abap_typedescr=>typekind_packed )
                                         ( typekind        = cl_abap_typedescr=>typekind_int8
                                           normalized_type = cl_abap_typedescr=>typekind_packed )
                                         ( typekind        = cl_abap_typedescr=>typekind_hex
                                           normalized_type = cl_abap_typedescr=>typekind_char )
                                         ( typekind        = cl_abap_typedescr=>typekind_xsequence
                                           normalized_type = cl_abap_typedescr=>typekind_char )
                                         ( typekind        = cl_abap_typedescr=>typekind_xstring
                                           normalized_type = cl_abap_typedescr=>typekind_char ) ).
  ENDMETHOD.
ENDCLASS.
