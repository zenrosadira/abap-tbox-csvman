interface ZTBOX_IF_CSVMAN
  public .


  types:
    BEGIN OF ty_conf,
           header            TYPE flag,
           header_desc       TYPE flag,
           end_of_line       TYPE string,
           include_only      TYPE flag,
           quotechar         TYPE char1,
           convexit          TYPE flag,
           condense          TYPE flag,
           delimiter         TYPE char1,
           alignment         TYPE i,
           keep_init         TYPE flag,
           country           TYPE land1,
           date_format       TYPE string,
           time_format       TYPE string,
           number_format     TYPE xudcpfm,
           escapechar        TYPE char1,
           doublequote       TYPE flag,
           decimals          TYPE i,
           quoting           TYPE i,
           use_number_format TYPE flag,
         END OF ty_conf .
  types:
    BEGIN OF ty_transformations,
      order  TYPE i,
      object TYPE REF TO object,
      transf TYPE seocmpname,
    END OF ty_transformations .
  types:
    ty_transformations_t TYPE TABLE OF ty_transformations WITH DEFAULT KEY .
  types:
    BEGIN OF ty_validations,
      object     TYPE REF TO object,
      validation TYPE seocmpname,
      failed     TYPE flag,
    END OF ty_validations .
  types:
    ty_validations_t TYPE TABLE OF ty_validations WITH DEFAULT KEY .
  types:
    ty_int_range TYPE RANGE OF i .

  data _CONF type TY_CONF .
endinterface.
