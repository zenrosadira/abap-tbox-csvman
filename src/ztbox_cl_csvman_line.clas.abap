class ZTBOX_CL_CSVMAN_LINE definition
  public
  final
  create public

  global friends ZTBOX_CL_CSVMAN .

public section.

  methods CONSTRUCTOR
    importing
      !I_INDEX type I
      !I_LINE type STRING
      !I_CSV type ref to ZTBOX_CL_CSVMAN .
  methods GET_VALUE
    importing
      !I_FIELD type NAME_FELD
    returning
      value(R_VAL) type STRING .
protected section.
private section.

  types:
    BEGIN OF ty_values,
           value TYPE string,
           field TYPE name_feld,
         END OF ty_values .
  types:
    ty_values_t TYPE TABLE OF ty_values .

  data _IX type I .
  data _LINE type STRING .
  data _REF type ref to DATA .
  data _CSV type ref to ZTBOX_CL_CSVMAN .
  data _STR_VALUES type TY_VALUES_T .

  methods GET_VALUES
    returning
      value(R_VALUES) type STRING_TABLE .
  methods SET_VALUES .
ENDCLASS.



CLASS ZTBOX_CL_CSVMAN_LINE IMPLEMENTATION.


  METHOD constructor.

    _ix   = i_index.
    _line = i_line.
    _csv  = i_csv.

    set_values( ).

  ENDMETHOD.


  METHOD get_value.

    DATA(val_str) = VALUE #( _str_values[ field = i_field ]-value OPTIONAL ).

    DATA(field) = _csv->get_field( i_field ).

    r_val = field->adapt_value( val_str ).

  ENDMETHOD.


  METHOD get_values.

    DATA tot_enc TYPE i.

    DATA(line)          = _line.
    DATA(combine)       = abap_false.
    DATA(val_combined)  = VALUE string( ).

    IF _csv->_configuration-escapator IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF _csv->_configuration-escapator IN line WITH _csv->_configuration-enclosing.
    ENDIF.

    SPLIT line AT _csv->_configuration-separator INTO TABLE DATA(values).

    IF _csv->_configuration-enclosing IS INITIAL.
      r_values = values.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <val> TYPE string.

    LOOP AT values INTO DATA(value).

      FIND ALL OCCURRENCES OF _csv->_configuration-enclosing IN value MATCH COUNT DATA(c_enc).
      tot_enc = tot_enc + c_enc.

      CASE tot_enc MOD 2.

        WHEN 0.
          IF combine EQ abap_true.
            val_combined = |{ val_combined }{ _csv->_configuration-separator }{ value }|.
            value = val_combined.
          ENDIF.

          IF _csv->_configuration-escapator IS NOT INITIAL.
            REPLACE ALL OCCURRENCES OF |{ _csv->_configuration-escapator }{ _csv->_configuration-enclosing }| IN value WITH _csv->_configuration-enclosing.
          ENDIF.

          REPLACE FIRST OCCURRENCE OF _csv->_configuration-enclosing IN value WITH space.

          IF value IS NOT INITIAL.

            DATA(val_last) = substring( val = value off = strlen( value ) - 1 len = 1 ).

            IF val_last EQ _csv->_configuration-enclosing.
              value = substring( val = value off = 0 len = strlen( value ) - 1 ).
            ENDIF.

          ENDIF.

          APPEND value TO r_values.
          CLEAR: value, val_combined, combine.

        WHEN OTHERS.
          combine = abap_true.
          val_combined = COND #( WHEN val_combined IS INITIAL THEN value ELSE |{ val_combined }{ _csv->_configuration-separator }{ value }| ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_values.

    DATA(values) = get_values( ).

    LOOP AT _csv->_configuration-mappings INTO DATA(map).

      READ TABLE values INTO DATA(value) INDEX map-src_col_pos.
      CHECK sy-subrc EQ 0.

      _str_values = VALUE #( BASE _str_values (
        value = value
        field = map-trg_col_name ) ).

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
