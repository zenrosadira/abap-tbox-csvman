CLASS zcl_tbox_csvman_apack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_apack_manifest.

    METHODS constructor.
ENDCLASS.


CLASS zcl_tbox_csvman_apack IMPLEMENTATION.
  METHOD constructor.
    if_apack_manifest~descriptor = VALUE #( group_id    = 'ztbox'
                                             artifact_id = 'abap-tbox-csvman'
                                             version     = '1.0'
                                             git_url     = 'https://github.com/zenrosadira/abap-tbox-csvman.git' ).
  ENDMETHOD.
ENDCLASS.
