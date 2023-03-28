# ABAP CSV Manager

![Example](CSV_Example.png)

:office_worker: **Can I decide which character to use as delimiter, quotechar, and line terminator? And what about escaping special characters?**

:mage: Once instantiated you can configure the csv management object:
- `csv_man->delimiter( |;| ).`  *Fields delimiter, default is comma:* `,`
- `csv_man->quotechar( |"| ).`  *To quote fields, default is none*
- `csv_man->end_of_line( '|' ).` *Line-terminator char, default is Carriage Return and Line Feed* `%_CR_LF`
- `csv_man->escapechar( |/| ).` *To escape special characters, both in read and write mode*
- `csv_man->doublequote( abap_true ).` *To escape a quotechar character with a quotechar character*
- `csv_man->quoting( ztbox_cl_csvman=>c_quote_minimal ).` *To restrict quoting application, with these options:*
  - `ztbox_cl_csvman=>c_quote_all` *to apply quotechar character to all fields (this is default behaviour if a quotechar is set)*
  - `ztbox_cl_csvman=>c_quote_minimal` *to apply quotechar character only to fields containing special characters*
  - `ztbox_cl_csvman=>c_quote_nonnumeric` *to apply quotechar character only to non-numeric fields*
  - `ztbox_cl_csvman=>c_quote_none` *to never quotes fields (this is the default behaviour if no quotechar is set)*

:office_worker: **Nice, but I want also control fields output format, especially for date/time/number fields**

:mage: Sure, you can use these configurations:
- `csv_man->date_format( |yyyy/dd/mm| ).` *To decide output format for date fields in write mode, or to declare expected format for date fields in read mode. You can use any format containing "dd", "mm", "yy", "yyyy" and an optional separator.*
- `csv_man->time_format( |hh:mm:ss| ).` *Same as above, but for time fields.*
- `csv_man->number_format( ).` *Three options here:*
  - ` ` *(blank) to output numbers like* `1.234.567,89`
  - `X` *to output numbers like* `1,234,567.89`
  - `Y` *to output numbers like* `1 234 567,89`
- `csv_man->country( 'US' ).` *To output date, time and numbers according to a country rules (less specific than previous methods).*
- `csv_man->convexit( abap_true ).` *To apply data element conversion exit, internal-to-external in write mode, external-to-internal in read mode
- `csv_man->condense( abap_true ).` *To remove leading and trailing spaces*
- `csv_man->keep_init( abap_true ).` *To maintain initial values: if set to* `abap_false` *a number field containing only 0, as well as an initial date, became blank in write mode. Default option is* `abap_true`
- `csv_man->alignment( cl_abap_format=>a_right ).` *To align fields content according to the following options:*
  - `cl_abap_format=>a_left` *To justify text on the left (default option)*
  - `cl_abap_format=>a_right` *To justify text on the right*
