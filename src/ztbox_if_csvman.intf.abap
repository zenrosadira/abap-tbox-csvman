interface ZTBOX_IF_CSVMAN
  public .


  types:
    BEGIN OF ty_field_config,
           enclosing      TYPE string,
           convexit       TYPE flag,
           alignment      TYPE i,
           keep_initials  TYPE flag,
           country_format TYPE land1,
         END OF ty_field_config .

  methods COUNTRY_FORMAT
    importing
      !I_CTR type LAND1
    returning
      value(R_RES) type ref to ZTBOX_IF_CSVMAN .
  methods KEEP_INIT
    importing
      !I_KIN type FLAG
    returning
      value(R_RES) type ref to ZTBOX_IF_CSVMAN .
  methods CONVEXIT
    importing
      !I_CEX type FLAG
    returning
      value(R_RES) type ref to ZTBOX_IF_CSVMAN .
  methods ENCLOSING
    importing
      !I_ENC type STRING
    returning
      value(R_RES) type ref to ZTBOX_IF_CSVMAN .
endinterface.
