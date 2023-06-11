class ZCL_TBOX_CSV_COMMON definition
  public
  create protected

  global friends ZCL_TBOX_CSVFIELD .

public section.

  methods FIELD
    importing
      !I_NAME type CLIKE
    returning
      value(R_RES) type ref to ZCL_TBOX_CSVFIELD .
  methods HEADER_DESC
    importing
      !I_ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods HEADER
    importing
      !I_ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods DOUBLEQUOTE
    importing
      !I_ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods QUOTING
    importing
      !I_QUOTING type I
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods ESCAPECHAR
    importing
      !I_ESCAPECHAR type CLIKE
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods QUOTECHAR
    importing
      !I_QUOTECHAR type CLIKE
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods DELIMITER
    importing
      !I_DELIMITER type CLIKE
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods END_OF_LINE
    importing
      !I_EOF type CLIKE
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods DECIMALS
    importing
      !I_DECIMALS type I
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods NUMBER_FORMAT
    importing
      !I_NUMBER_FORMAT type CLIKE
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods TIME_FORMAT
    importing
      !I_TIME_FORMAT type CLIKE
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods DATE_FORMAT
    importing
      !I_DATE_FORMAT type CLIKE
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods COUNTRY
    importing
      !I_COUNTRY type LAND1
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods CONV_EXIT
    importing
      !I_ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods KEEP_INIT
    importing
      !I_ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods ALIGNMENT
    importing
      !I_ALIGNMENT type I
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  methods CONDENSE_VALUES
    importing
      !I_ACTIVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(R) type ref to ZCL_TBOX_CSV_COMMON .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ts_date_format,
        dd_off TYPE i,
        mm_off TYPE i,
        yy_off TYPE i,
        yy_len TYPE i,
        sep    TYPE c LENGTH 1,
      END OF ts_date_format .
    TYPES:
      BEGIN OF ts_time_format,
        hh_off TYPE i,
        mm_off TYPE i,
        ss_off TYPE i,
        sep    TYPE c LENGTH 1,
      END OF ts_time_format .
    TYPES:
      BEGIN OF ts_general_params,
        header      TYPE abap_bool,
        header_desc TYPE abap_bool,
        end_of_line TYPE string,
        quotechar   TYPE c LENGTH 1,
        delimiter   TYPE c LENGTH 1,
        escapechar  TYPE c LENGTH 1,
        doublequote TYPE abap_bool,
        quoting     TYPE i,
      END OF ts_general_params .
    TYPES:
      BEGIN OF ts_format_params,
        convexit          TYPE abap_bool,
        condense          TYPE abap_bool,
        alignment         TYPE i,
        keep_init         TYPE abap_bool,
        country           TYPE land1,
        date_format       TYPE ts_date_format,
        time_format       TYPE ts_time_format,
        number_format     TYPE c LENGTH 1,
        decimals          TYPE i,
        use_number_format TYPE abap_bool,
      END OF ts_format_params .
    TYPES:
      BEGIN OF ts_catalog,
        field     TYPE c LENGTH 30,
        field_obj TYPE REF TO zcl_tbox_csvfield,
      END OF ts_catalog .
    TYPES:
      BEGIN OF ts_fields_map,
        field    TYPE c LENGTH 30,
        position TYPE i,
      END OF ts_fields_map .

    DATA:
    m_catalog TYPE TABLE OF ts_catalog WITH KEY field .
    DATA:
    m_include_fields TYPE RANGE OF string .
    DATA:
    m_exclude_fields TYPE RANGE OF string .
    DATA:
    m_fields_mapping TYPE TABLE OF ts_fields_map WITH KEY field .
    DATA m_general_params TYPE ts_general_params .
    DATA m_format_params TYPE ts_format_params .
    DATA m_decimals_set TYPE abap_bool .

    METHODS _set_catalog
      IMPORTING
        !i_table TYPE ANY TABLE
      RAISING
        zcx_tbox_csvman .
  PRIVATE SECTION.

    METHODS _next_free_catalog_position
    RETURNING
      VALUE(r) TYPE i .
    CLASS-METHODS _create_time_format
    IMPORTING
      !i_time_format TYPE clike
    RETURNING
      VALUE(r_time_format) TYPE ts_time_format .
    CLASS-METHODS _create_date_format
    IMPORTING
      !i_date_format TYPE clike
    RETURNING
      VALUE(r_date_format) TYPE ts_date_format .
ENDCLASS.



CLASS ZCL_TBOX_CSV_COMMON IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->ALIGNMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ALIGNMENT                    TYPE        I
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD alignment.

    m_format_params-alignment = i_alignment.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->CONDENSE_VALUES
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ACTIVE                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD condense_values.

    m_format_params-condense = i_active.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->CONV_EXIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ACTIVE                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_exit.

    m_format_params-convexit = i_active.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->COUNTRY
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_COUNTRY                      TYPE        LAND1
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD country.

    m_format_params-country = i_country.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->DATE_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DATE_FORMAT                  TYPE        CLIKE
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD date_format.

    m_format_params-date_format = _create_date_format( i_date_format ).

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->DECIMALS
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DECIMALS                     TYPE        I
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD decimals.

    m_format_params-decimals  = i_decimals.
    m_decimals_set            = abap_true.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->DELIMITER
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DELIMITER                    TYPE        CLIKE
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD delimiter.

    m_general_params-delimiter = i_delimiter.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->DOUBLEQUOTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ACTIVE                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD doublequote.

    m_general_params-doublequote = i_active.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->END_OF_LINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_EOF                          TYPE        CLIKE
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD end_of_line.

    m_general_params-end_of_line = i_eof.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->ESCAPECHAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ESCAPECHAR                   TYPE        CLIKE
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD escapechar.

    m_general_params-escapechar = i_escapechar.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NAME                         TYPE        CLIKE
* | [<-()] R_RES                          TYPE REF TO ZCL_TBOX_CSVFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD field.

    IF NOT line_exists( m_catalog[ field = i_name ] ).

      INSERT VALUE #(
        field     = i_name
        field_obj = NEW #(
            i_name  = i_name
            i_csv   = me ) ) INTO TABLE m_catalog.

    ENDIF.

    r_res = m_catalog[ field = i_name ]-field_obj.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->HEADER
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ACTIVE                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD header.

    m_general_params-header = i_active.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->HEADER_DESC
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ACTIVE                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD header_desc.

    m_general_params-header_desc = i_active.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->KEEP_INIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ACTIVE                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD keep_init.

    m_format_params-keep_init = i_active.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->NUMBER_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NUMBER_FORMAT                TYPE        CLIKE
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD number_format.

    m_format_params-number_format     = i_number_format.
    m_format_params-use_number_format = abap_true.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->QUOTECHAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_QUOTECHAR                    TYPE        CLIKE
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD quotechar.

    m_general_params-quotechar = i_quotechar.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->QUOTING
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_QUOTING                      TYPE        I
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD quoting.

    m_general_params-quoting = i_quoting.

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TBOX_CSV_COMMON->TIME_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TIME_FORMAT                  TYPE        CLIKE
* | [<-()] R                              TYPE REF TO ZCL_TBOX_CSV_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD time_format.

    m_format_params-time_format = _create_time_format( i_time_format ).

    r = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_TBOX_CSV_COMMON=>_CREATE_DATE_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DATE_FORMAT                  TYPE        CLIKE
* | [<-()] R_DATE_FORMAT                  TYPE        TS_DATE_FORMAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _create_date_format.

    DATA(d2_off) = find( val    = i_date_format
                         sub    = `dd`
                         case   = abap_false ).
    DATA(m2_off) = find( val    = i_date_format
                         sub    = `mm`
                         case   = abap_false ).
    DATA(y2_off) = find( val    = i_date_format
                         sub    = `yy`
                         case   = abap_false ).
    DATA(y4_off) = find( val    = i_date_format
                         sub    = `yyyy`
                         case   = abap_false ).
    DATA(y4_cnt) = count( val   = i_date_format
                          sub   = `yyyy`
                          case  = abap_false ).

    DATA(sep_off) = nmin( val1 = d2_off
                          val2 = m2_off ) + 2.

    r_date_format = VALUE #(
      sep     = i_date_format+sep_off(1)
      dd_off  = d2_off
      mm_off  = m2_off
      yy_off  = COND #( WHEN y4_cnt > 0 THEN y4_off ELSE y2_off )
      yy_len  = COND #( WHEN y4_cnt > 0 THEN 4 ELSE 2 ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_TBOX_CSV_COMMON=>_CREATE_TIME_FORMAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TIME_FORMAT                  TYPE        CLIKE
* | [<-()] R_TIME_FORMAT                  TYPE        TS_TIME_FORMAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _create_time_format.

    DATA(h2_off) = find( val  = i_time_format
                         sub  = `hh`
                         case = abap_false ).
    DATA(m2_off) = find( val  = i_time_format
                         sub  = `mm`
                         case = abap_false ).
    DATA(s2_off) = find( val  = i_time_format
                         sub  = `ss`
                         case = abap_false ).

    DATA(p_off) = nmin( val1 = s2_off
                        val2 = m2_off ) + 2.

    r_time_format = VALUE #(
      sep     = i_time_format+p_off(1)
      hh_off  = h2_off
      mm_off  = m2_off
      ss_off  = s2_off ).


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TBOX_CSV_COMMON->_NEXT_FREE_CATALOG_POSITION
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R                              TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _next_free_catalog_position.

    DO lines( m_catalog ) TIMES.

      IF NOT line_exists( m_fields_mapping[ position = sy-index ] ).
        r = sy-index.
        EXIT.
      ENDIF.

    ENDDO.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_TBOX_CSV_COMMON->_SET_CATALOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TABLE                        TYPE        ANY TABLE
* | [!CX!] ZCX_TBOX_CSVMAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _set_catalog.

    IF NOT lcl_utils=>is_flat_table( i_table ).
      RAISE EXCEPTION TYPE zcx_tbox_csvman EXPORTING text = CONV #( `Invalid table.` ).
    ENDIF.

    DATA(components) = lcl_utils=>get_components( i_table ).

    LOOP AT components INTO DATA(component).

      IF m_include_fields IS NOT INITIAL AND component-name NOT IN m_include_fields.
        DELETE m_catalog        WHERE field = component-name.
        DELETE m_fields_mapping WHERE field = component-name.
        CONTINUE.
      ENDIF.

      IF m_exclude_fields IS NOT INITIAL.
        CHECK component-name NOT IN m_exclude_fields.
      ENDIF.

      field( component-name )->_assign_element( CAST cl_abap_elemdescr( component-type ) ).

      IF NOT line_exists( m_fields_mapping[ field = component-name ] ).
        INSERT VALUE #(
          field     = component-name
          position  = _next_free_catalog_position( ) ) INTO TABLE m_fields_mapping.
      ENDIF.

    ENDLOOP.

    SORT m_fields_mapping BY position.

  ENDMETHOD.
ENDCLASS.
