*&---------------------------------------------------------------------*
*& Report ZOT_29_P_ALV_BAPI
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsy_005_p_alv_bapi.

INCLUDE zsy_005_i_alv_bapi_top.
INCLUDE zsy_005_i_alv_bapi_selection.
INCLUDE zsy_005_i_alv_bapi_class.
INCLUDE zsy_005_i_alv_bapi_pbo.
INCLUDE zsy_005_i_alv_bapi_pai.

INITIALIZATION.
  CREATE OBJECT go_class.

START-OF-SELECTION.

  go_class->start_of_selection( ).

  CALL SCREEN 100.
