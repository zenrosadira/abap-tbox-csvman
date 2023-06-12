CLASS zcx_tbox_csvman DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS zcx_tbox_csvman TYPE c LENGTH 32 VALUE '00155DB5F7D41EDE81F31844E11B0321'.

    DATA text TYPE string.

    METHODS constructor
      IMPORTING textid    LIKE textid   OPTIONAL
                !previous LIKE previous OPTIONAL
                !text     TYPE string   OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_tbox_csvman IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( textid   = textid
                        previous = previous ).
    IF textid IS INITIAL.
      me->textid = zcx_tbox_csvman.
    ENDIF.
    me->text = text.
  ENDMETHOD.
ENDCLASS.
