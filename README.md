# ABAP CSV Manager

![Example](https://i.ibb.co/9N6MN7J/CSV-Example.png)

## Quick Start

```abap
* First, create an instance
DATA(csv_man) = NEW ztbox_cl_csvman( ).

* Now you can transform an internal table into a CSV string
DATA(csv_str) = csv_man->create_csv( t_table_data ).

* csv_str is a string, you can transform it into a table of strings
DATA(csv_tab) = csv_man->to_table_string( csv_str ).

* To read a CSV file (imported as a table of strings):
csv_man->read_csv( EXPORTING it_csv = csv_file CHANGING ct_table = t_table_data ).
```

## General Configuration
:office_worker: **Can I decide which character to use as delimiter, quotechar, and line terminator? And what about escaping special characters?**

:mage: Once instantiated you can configure the csv management object:
- ``csv_man->delimiter( `;` ).``  *fields delimiter, default is comma:* `,`*.*
- ``csv_man->quotechar( `"` ).``  *to quote fields, default is none.*
- ``csv_man->end_of_line( `|` ).`` *line-terminator char, default is Carriage Return and Line Feed* `%_CR_LF` *.*
- ``csv_man->escapechar( `/` ).`` *to escape special characters, both in read and write mode.*
- ``csv_man->doublequote( abap_true ).`` *to escape a quotechar character with a quotechar character.*
- `csv_man->quoting( ztbox_cl_csvman=>c_quote_minimal ).` *to restrict quoting application, with these options:*
  - `ztbox_cl_csvman=>c_quote_all` *to apply quotechar character to all fields (this is default behaviour if a quotechar is set);*
  - `ztbox_cl_csvman=>c_quote_minimal` *to apply quotechar character only to fields containing special characters;*
  - `ztbox_cl_csvman=>c_quote_nonnumeric` *to apply quotechar character only to non-numeric fields;*
  - `ztbox_cl_csvman=>c_quote_none` *to never quote fields (this is the default behaviour if no quotechar is set).*
- `csv_man->header( abap_true ).` *to write/expect an header line in write/read mode.*
- `csv_man->header_desc( abap_true ).` *to use long label description (from data element, in the log-on language) as header text field. If the field is not typed with a dictionary data element its name is still used as description.*

## Output Format
:office_worker: **Nice, but I want also control fields output format, especially for date/time/numeric fields.**

:mage: Sure, you can use these configurations:
- ``csv_man->date_format( `yyyy/dd/mm` ).`` *to decide output format for date fields in write mode, or to declare expected format for date fields in read mode. You can use any format containing "dd", "mm", "yy", "yyyy" and an optional separator. Default is* `dd.mm.yyyy`.
- ``csv_man->time_format( `hh:mm:ss` ).`` *same as above, but for time fields. Default is* `hh:mm:ss`.
- `csv_man->number_format( ).` *three options here:*
  - ` ` *(blank) to output numbers like* `1.234.567,89`
  - `X` *to output numbers like* `1,234,567.89`
  - `Y` *to output numbers like* `1 234 567,89`
- ``csv_man->country( `US` ).`` *to output date, time and numbers according to a country rules (less specific than previous methods).*
- `csv_man->decimals( 3 ).` *to write numerical fields with the specified decimals precision.*
- `csv_man->convexit( abap_true ).` *to apply domain conversion exit, internal-to-external in write mode, external-to-internal in read mode. Default is* `abap_true`.
- `csv_man->condense( abap_true ).` *to remove leading and trailing spaces. Default is* `abap_false`.
- `csv_man->keep_init( abap_true ).` *to maintain initial values: if set to* `abap_false` *a numerical field containing only 0, as well as an initial date or initial time, became blank in write mode. Default is* `abap_true`.
- `csv_man->alignment( cl_abap_format=>a_right ).` *to align fields content according to the following options:*
  - `cl_abap_format=>a_left` *to justify text on the left (default option);*
  - `cl_abap_format=>a_right` *to justify text on the right.*

## Fields Properties
:office_worker: **Ok cool. These are global configuration, valid for all the fields, aren't they? What if I want to set some format property to one field and a different property to another?**

:mage: You can restrict the application of the previous methods to a single field by calling `field( )` method first, also chaining other methods, e.g.:

```abap
csv_man->field( `AMOUNT_USD` )->number_format( `X` ).
csv_man->field( `AMOUNT_EUR` )->country( `IT` )->decimals( 2 ).
```
You can also exclude some fields from the CSV generation/reading process using `exclude( abap_true )` method:

```abap
csv_man->field( `MANDT` )->exclude( abap_true ).
```

Viceversa, if you work with a table having too many fields, you can generate or reading a CSV considering only a small subset of fields using `include( abap_true )` method. Once you have called `include` for a field, only fields for which `include` has been called will be considered.

```abap
csv_man->field( `MATNR` )->include( abap_true ).
csv_man->field( `WERKS` )->include( abap_true ).
```

If the order of the fields in the table does not match the columns in the CSV to generate or read, you can map each field with the corresponding csv-column position:

```abap
csv_man->header_desc( abap_true ).
csv_man->field( `MATNR` )->csv_position( 2 )->label( `Material!!!` ).
csv_man->field( `WERKS` )->csv_position( 1 )->label( `THE Plant` ).
```
In this way, the following table:
| MATNR  | WERKS |
| ------- | ------ |
| AAAA01  | US01  |
| BBBB02  | US02  |

gives this CSV as output:
```csv
THE Plant,Material!!!
US01,AAAA01
US02,BBBB02
```

`label( )` method set a custom header text when `header_desc( )` is used.

## Validations
:office_worker: **Regarding reading the csv, is there an automatism to help identify errors in the data?**

:mage: We have it. When a CSV is read, some validation checks are performed according to the data type of the target fields: date fields (if not blank) must be a valid and plausible date; time fields (if not blank) must contain a valid and plausible time, numerical fields (if not blank) must contain a valid number. Whenever any of these check fail, the contents are not transferred. You get a detailed report for the validation fails by calling `get_validations_fails( )`.

Suppose you read this CSV:

```csv
31/02/2023,10:45:19,"1900,20"
28/02/2023,25:00:00,"-894,23"
31/12/2022,00:00:01,"12A4,43"
```

into a table with structure `DATE [D(8)] | TIME [T(6)] | AMOUNT [P(7) DEC(2)]`. The output will be
| DATE  | TIME | AMOUNT |
| ------- | ------ | ------ |
| 00000000 | 104519 | 1900.20 |
| 20230228 | 000000 | -894.00 |
| 20221231 | 000001 | 0.00 |

And the `get_validation_fails( )` output is this table:
| ROW  | COL | TABLE_FIELD | CSV_VALUE | SAP_VALUE | CHECK_FAILED |
| ------- | ------- | ------- | ------- | ------- | ------- |
| 1 | 1 | DATE | 31/02/2023 | 20230231 | DATE_PLAUSIBILITY |
| 2 | 2 | TIME | 25:00:00 | 250000 | TIME_PLAUSIBILITY |
| 3 | 3 | AMOUNT | 12A4,43 | 12A4.43 | VALID_NUMB |

You can add also custom validation checks: it must be an instance method with the following signature

```abap
METHODS sample_check IMPORTING value TYPE string RETURNING VALUE(fail) TYPE flag.
```
If, e.g., an object `sample_object` implements method `sample_check`, you can add this check to a field:

```abap
csv_man->field( `FIELD_NAME` )->add_post_validation(
  check_object  = sample_object
  check_method  = `SAMPLE_CHECK` ).
```

in two ways: as a *pre validation* by calling method `add_pre_validation( )`, and as a *post validation* by calling method `add_post_validation( )`.
- Pre-validation checks are applied to raw data: parameter `value` is a string containing exactly the content of the field in the CSV;
- Post-validation checks are applied to transformed data: parameter `value` is a string containing the field value interally formatted for the SAP data type field target, according to the format options configured.

`fail = abap_true` means the check has not been passed.

## Installation
Install this project using [abapGit](https://abapgit.org/) ![abapGit](https://docs.abapgit.org/img/favicon.png)
