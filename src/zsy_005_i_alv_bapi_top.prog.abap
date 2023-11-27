*&---------------------------------------------------------------------*
*& Include          ZOT_29_I_ALV_BAPI_TOP
*&---------------------------------------------------------------------*

TABLES: vbrk, vbrp, ekko.

CLASS lcl_alv_bapi DEFINITION DEFERRED. "top kısmı classın altında kaldığı için
DATA: go_class TYPE REF TO lcl_alv_bapi. "classı refere alan obje

DATA: go_alv  TYPE REF TO cl_gui_alv_grid,
      go_alv2 TYPE REF TO cl_gui_alv_grid.

DATA: go_cont TYPE REF TO cl_gui_custom_container.

DATA: gt_fcat_header TYPE lvc_t_fcat,
      gs_fcat_header TYPE lvc_s_fcat.

DATA: gt_fcat_item TYPE lvc_t_fcat,
      gs_fcat_item TYPE lvc_s_fcat.

DATA: gs_layout_header  TYPE lvc_s_layo.

DATA: gs_layout_item  TYPE lvc_s_layo.

DATA: go_splitter TYPE REF TO cl_gui_splitter_container,
      go_gui1     TYPE REF TO cl_gui_container,
      go_gui2     TYPE REF TO cl_gui_container.

DATA: gt_header TYPE TABLE OF zot_29_s_alv_bapi_header,
      gs_header TYPE zot_29_s_alv_bapi_header.

DATA: gt_item TYPE TABLE OF zot_29_s_alv_bapi_item,
      gs_item TYPE zot_29_s_alv_bapi_item.

DATA: gt_cellstyle TYPE TABLE OF lvc_t_styl,
      gs_cellstyle TYPE lvc_s_styl.

DATA: gt_rowindex TYPE lvc_t_row,
      gs_rowindex LIKE LINE OF gt_rowindex.

DATA : gt_sas           TYPE STANDARD TABLE OF sval,
       gs_sas           TYPE sval,
       gv_returncode(1) TYPE c.
