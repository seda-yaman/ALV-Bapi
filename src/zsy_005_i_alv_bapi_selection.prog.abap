*&---------------------------------------------------------------------*
*& Include          ZOT_29_I_ALV_BAPI_SELECTION
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_vbeln FOR vbrk-vbeln ,
                  s_fkdat FOR vbrk-fkdat ,
                  s_fkart FOR vbrk-fkart ,
                  s_kunrg FOR vbrk-kunrg .

SELECTION-SCREEN END OF BLOCK b1.
