*&---------------------------------------------------------------------*
*& Include          ZOT_29_I_ALV_BAPI_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_alv_bapi DEFINITION.
  PUBLIC SECTION.

    METHODS: display_alv,
      get_data_header,
      get_data_item,
      start_of_selection,
      set_fcat_header,
      set_fcat_item,
      set_layout_header,
      set_layout_item,
      save,
      sas,
      bapi_create,
      handler_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING
          e_row
          e_column
          es_row_no,
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive,
      handle_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm,
      handler_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          e_row_id
          e_column_id
          es_row_no,
      handler_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no.

  PRIVATE SECTION.
    DATA: mt_data TYPE TABLE OF zot_29_s_alv_bapi_item.

ENDCLASS.

CLASS lcl_alv_bapi IMPLEMENTATION.

  METHOD display_alv.

    CREATE OBJECT go_cont
      EXPORTING
        container_name = 'CC_ALV'.

    CREATE OBJECT go_splitter
      EXPORTING
        parent  = go_cont "hangi objeyi split edeceksem onu vermem gereken alan
        rows    = 2   "objeyi kaça bölsün kaç satır
        columns = 1. "objeyi kaça bölsün kaç kolon

    CALL METHOD go_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = go_gui1.

    CREATE OBJECT go_alv
      EXPORTING
        i_parent = go_gui1.

    SET HANDLER handler_double_click FOR go_alv.
    SET HANDLER handler_hotspot_click FOR go_alv.

    CALL METHOD go_alv->set_table_for_first_display
      EXPORTING
*       i_structure_name = 'GT_ALV'
        is_layout       = gs_layout_header
      CHANGING
        it_outtab       = gt_header
        it_fieldcatalog = gt_fcat_header.

* İkinci alv için kodlar
    CALL METHOD go_splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = go_gui2.

    CREATE OBJECT go_alv2
      EXPORTING
        i_parent = go_gui2.

    SET HANDLER handle_toolbar FOR go_alv2.
    SET HANDLER handle_ucomm FOR go_alv2.
    SET HANDLER handler_hotspot_click FOR go_alv2.
    SET HANDLER handler_button_click FOR go_alv2.

    CALL METHOD go_alv2->register_edit_event  "edit özelliğini algılaması için
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
*  EXCEPTIONS
*       error      = 1
*       others     = 2
      .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL METHOD go_alv2->set_table_for_first_display
      EXPORTING
*       i_structure_name = 'GT_ALV'
        is_layout       = gs_layout_item
      CHANGING
        it_outtab       = mt_data
        it_fieldcatalog = gt_fcat_item.

  ENDMETHOD.

  METHOD get_data_header.

    SELECT vbrk~vbeln,
      vbrk~bukrs,
      vbrk~fkdat,
      vbrk~fkart,
      vbrk~kunrg,
      vbrk~fktyp,
      vbrk~vkorg,
      vbrk~vtweg,
      vbrk~netwr,
      vbrk~waerk
      FROM vbrk
      INNER JOIN vbrp ON vbrp~vbeln = vbrk~vbeln
        WHERE   vbrk~vbeln IN @s_vbeln
            AND vbrk~fkdat IN @s_fkdat
            AND vbrk~fkart IN @s_fkart
            AND vbrk~kunrg IN @s_kunrg
      INTO CORRESPONDING FIELDS OF TABLE @gt_header.

  ENDMETHOD.

  METHOD get_data_item.

    SELECT vbrp~vbeln,
     vbrp~posnr,
     vbrp~vrkme,
     vbrp~meins,
     vbrk~waerk,
     vbrp~ntgew,
     vbrp~brgew,
     vbrp~netwr,
     vbrp~matnr,
     vbrp~arktx,
     vbrp~fkimg
     FROM vbrp
      INNER JOIN vbrk ON vbrk~vbeln = vbrp~vbeln
      INTO CORRESPONDING FIELDS OF TABLE @gt_item.

  ENDMETHOD.

  METHOD start_of_selection.

    go_class->get_data_header( ).
    go_class->set_fcat_header( ).
    go_class->set_layout_header( ).

    go_class->set_fcat_item( ).
    go_class->set_layout_item( ).

  ENDMETHOD.

  METHOD set_fcat_header.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZOT_29_S_ALV_BAPI_HEADER' "z'li structure oluşturdum
*       I_INTERNAL_TABNAME     =  "internal table name
      CHANGING
        ct_fieldcat            = gt_fcat_header "fieldcatalog verilir.
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT  gt_fcat_header ASSIGNING FIELD-SYMBOL(<lfs_fcat_h>).
      CASE <lfs_fcat_h>-fieldname.
        WHEN 'VBELN'.
          <lfs_fcat_h>-hotspot = abap_true.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_fcat_item.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZOT_29_S_ALV_BAPI_ITEM' "z'li structure oluşturdum
*       I_INTERNAL_TABNAME     =  "internal table name
      CHANGING
        ct_fieldcat            = gt_fcat_item "fieldcatalog verilir.
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT  gt_fcat_item ASSIGNING FIELD-SYMBOL(<lfs_fcat_i>).
      CASE <lfs_fcat_i>-fieldname.
        WHEN 'NETPR'.
          <lfs_fcat_i>-coltext = 'Net Fiyat'.
        WHEN 'NET_GUNCEL_FIYAT'.
          <lfs_fcat_i>-coltext = 'Net Güncel Fiyat'.
          <lfs_fcat_i>-edit = abap_true.
        WHEN 'VBELN'.
          <lfs_fcat_i>-hotspot = abap_true.
        WHEN 'LOG'.
          <lfs_fcat_i>-coltext = 'Log Görüntüle'.
          <lfs_fcat_i>-style = cl_gui_alv_grid=>mc_style_button. "hücreyi buton yapmak için
          <lfs_fcat_i>-icon = abap_true.
        WHEN 'EBELN'.
          <lfs_fcat_i>-coltext = 'EBELN'.
          <lfs_fcat_i>-hotspot = abap_true.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD set_layout_header.

    CLEAR: gs_layout_header.
    gs_layout_header-cwidth_opt = abap_true. "kolon optimizasyon
    gs_layout_header-zebra      = abap_true. "bir koyu renk bir açık renk
    gs_layout_header-col_opt    = abap_true.
    gs_layout_header-sel_mode   = 'A'. "tüm verileri seçmek tüm ya da satır bazında

  ENDMETHOD.

  METHOD set_layout_item.

    CLEAR: gs_layout_item.
    gs_layout_item-cwidth_opt = abap_true. "kolon optimizasyon
    gs_layout_item-zebra      = abap_true. "bir koyu renk bir açık renk
    gs_layout_item-col_opt    = abap_true.
    gs_layout_item-sel_mode   = 'A'. "tüm verileri seçmek tüm ya da satır bazında
    gs_layout_item-stylefname = 'CELLSTYLE'.
*    gs_layout_item-edit = abap_true.

  ENDMETHOD.


  METHOD handler_double_click.

    FREE mt_data. "her kayıtta üst üste kayıt eklemesin diye,
    go_class->get_data_item( ).

    SELECT vbeln,
      posnr,
      net_guncel_fiyat
      FROM zot_29_t_log
      INTO TABLE @DATA(lt_log).

    READ TABLE gt_header INTO gs_header INDEX e_row-index.
    IF sy-subrc EQ 0.
      LOOP AT gt_item ASSIGNING FIELD-SYMBOL(<fs_header>) WHERE vbeln = gs_header-vbeln. "vblen eşitse fs_headera atacak.

        DATA: lv_netf TYPE netpr.

*          LOOP AT gt_item ASSIGNING FIELD-SYMBOL(<fs_item>).
        READ TABLE lt_log INTO DATA(ls_log_read) WITH KEY vbeln = <fs_header>-vbeln
                                                           posnr = <fs_header>-posnr .

        CLEAR: lv_netf.
        <fs_header>-log = '@96@'. "log görüntüle buton
        lv_netf = <fs_header>-netwr / <fs_header>-fkimg.    "net fiyat için hesaplama
        <fs_header>-netpr = lv_netf.

        <fs_header>-net_guncel_fiyat = ls_log_read-net_guncel_fiyat.  "güncel fiyat kolonu için edite açma
        IF <fs_header>-net_guncel_fiyat NE 0.
          CLEAR: gs_cellstyle.
          gs_cellstyle-fieldname = 'NET_GUNCEL_FIYAT'.
          gs_cellstyle-style = '00000067'.
          APPEND gs_cellstyle TO <fs_header>-cellstyle.
        ENDIF.

*          ENDLOOP.

        APPEND <fs_header> TO mt_data.

      ENDLOOP.
    ENDIF.

    CALL METHOD go_alv2->refresh_table_display.

  ENDMETHOD.

  METHOD handle_toolbar.

    DATA ls_toolbar TYPE stb_button.
    CLEAR ls_toolbar.

    ls_toolbar-function   = 'KAYDET'.       "fonksiyon ismi sy-ucomm ile tetiklemek için
    ls_toolbar-text       = 'FİYAT KAYDET'. "buton texti
    ls_toolbar-icon       = '@2L@'.         "buton ikon
    ls_toolbar-quickinfo  = 'Fiyat Kaydet'. "buton üzerine gelindiğinde yazacak isim
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function   = 'SAS'.
    ls_toolbar-text       = 'SAS OLUŞTUR'.
    ls_toolbar-icon       = '@0N@'.
    ls_toolbar-quickinfo  = 'Sas Oluştur'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_ucomm.

    CASE e_ucomm.

      WHEN 'KAYDET'.
        save( ).

      WHEN 'SAS'.
        sas( ).

    ENDCASE.

  ENDMETHOD.

  METHOD save.

    DATA: gt_log TYPE TABLE OF zot_29_t_log,
          gs_log TYPE zot_29_t_log.

    go_alv2->get_selected_rows(
           IMPORTING
             et_index_rows = gt_rowindex  ).

    LOOP AT gt_rowindex INTO gs_rowindex .
      READ TABLE mt_data INDEX gs_rowindex-index INTO DATA(ls_data).

      IF sy-subrc EQ 0.
        gs_log-vbeln = ls_data-vbeln. "CORRESPONDING #( gs_item ).
        gs_log-posnr = ls_data-posnr.
        gs_log-netpr = ls_data-netpr.
        gs_log-net_guncel_fiyat = ls_data-net_guncel_fiyat.
        gs_log-log_datum = sy-datum.
        gs_log-log_uzeit = sy-uzeit.
        gs_log-log_uname = sy-uname.

        APPEND gs_log TO gt_log.
      ENDIF.

    ENDLOOP.

    MODIFY zot_29_t_log FROM TABLE gt_log.

    COMMIT WORK AND WAIT.

    MESSAGE i004(zot_29) .

  ENDMETHOD.

  METHOD handler_hotspot_click.

    IF e_column_id-fieldname = 'VBELN'.
      READ TABLE gt_header INTO gs_header INDEX e_row_id-index.
      SET PARAMETER ID 'VBELN' FIELD gs_header-vbeln.
      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.

      READ TABLE gt_item INTO gs_item INDEX e_row_id-index.
      SET PARAMETER ID 'VBELN' FIELD gs_item-vbeln.
      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.

    ELSEIF e_column_id-fieldname = 'EBELN'.
      READ TABLE gt_item INTO gs_item INDEX e_row_id-index.
      SET PARAMETER ID 'EBELN' FIELD gs_item-ebeln.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

    ENDIF.

  ENDMETHOD.

  METHOD sas.

    DATA: lt_rowindex TYPE lvc_t_row.
    go_alv2->get_selected_rows(
      IMPORTING
        et_index_rows = lt_rowindex
*      et_row_no      =
    ).

    IF lt_rowindex IS NOT INITIAL.

      CLEAR gs_sas.
      gs_sas-tabname   = 'EKKO' .
      gs_sas-fieldname = 'EKORG' .
      gs_sas-field_obl = 'X' .
      APPEND gs_sas TO gt_sas.

      CLEAR gs_sas.
      gs_sas-tabname   = 'EKKO' .
      gs_sas-fieldname = 'EKGRP' .
      gs_sas-field_obl = 'X' .
      APPEND gs_sas TO gt_sas.

      CLEAR gs_sas.
      gs_sas-tabname   = 'VBRP' .
      gs_sas-fieldname = 'WERKS' .
      gs_sas-field_obl = 'X' .
      APPEND gs_sas TO gt_sas.

      CLEAR gs_sas.
      gs_sas-tabname   = 'VBRP' .
      gs_sas-fieldname = 'LGORT' .
      gs_sas-field_obl = 'X' .
      APPEND gs_sas TO gt_sas.

      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
          no_value_check  = ''
          popup_title     = 'SAS OLUŞTUR'
          start_column    = '10'
          start_row       = '10'
        IMPORTING
          returncode      = gv_returncode
        TABLES
          fields          = gt_sas
        EXCEPTIONS
          error_in_fields = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      LOOP AT gt_sas INTO gs_sas.
        WRITE :/ 'Alan ismi:'  ,gs_sas-fieldname,
                 'Alan değeri:', gs_sas-value.
      ENDLOOP.

    ELSE.
      MESSAGE i008(zot_29).
    ENDIF.

    IF gv_returncode = ' '.

      go_class->bapi_create( ).

    ENDIF.

  ENDMETHOD.

  METHOD handler_button_click.

    DATA : lv_titel TYPE string,
           lv_text1 TYPE string,
           lv_text2 TYPE string,
           lv_text3 TYPE string.

    READ TABLE me->mt_data INTO DATA(ls_log_oku) INDEX es_row_no-row_id.

    IF sy-subrc EQ 0.

      lv_titel = 'Log Görüntüleme'.

      DATA(lv_fatura) = CONV string( ls_log_oku-vbeln ).
      DATA(lv_kalem)  = CONV string( ls_log_oku-posnr ).
      DATA(lv_gf)     = CONV string( ls_log_oku-net_guncel_fiyat ).

      lv_text1 = | { 'Fatura No: ' } | & | { ls_log_oku-vbeln } |.
      lv_text2 = | { 'Kalem No: ' } | & | { ls_log_oku-posnr } |.
      lv_text3 = | { 'Net Güncel Fiyat: ' } | & | { ls_log_oku-net_guncel_fiyat } |.

      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = lv_titel
          txt1  = lv_text1
          txt2  = lv_text2
          txt3  = lv_text3
*         TXT4  = ' '
        .
    ENDIF.

  ENDMETHOD.

  METHOD bapi_create.     "bapide verilen değerlerin karşılığı  xlenir

    DATA: ls_poheader   TYPE bapimepoheader,
          ls_poheaderx  TYPE bapimepoheaderx,
          lt_poitem     TYPE TABLE OF bapimepoitem,
          ls_poitem     TYPE bapimepoitem,
          lt_poitemx    TYPE TABLE OF bapimepoitemx,
          ls_poitemx    TYPE bapimepoitemx,
          lt_poaccount  TYPE TABLE OF bapimepoaccount,
          ls_poaccount  TYPE bapimepoaccount,
          lt_poaccountx TYPE TABLE OF bapimepoaccountx,
          ls_poaccountx TYPE bapimepoaccountx.
    DATA: ex_po_number TYPE bapimepoheader-po_number,
          exp_head     TYPE bapimepoheader.

    DATA: lt_return TYPE TABLE OF bapiret2,
          ls_return TYPE bapiret2.

    TYPE-POOLS: esp1.

    DATA: lt_tab   TYPE esp1_message_tab_type,
          ls_tab   TYPE esp1_message_wa_type,
          lv_numup TYPE int4.  "Count yapılıyor

    DATA: lt_rowindex2 TYPE lvc_t_row.
    go_alv2->get_selected_rows(
      IMPORTING
        et_index_rows = lt_rowindex2 " Indexes of Selected Rows
*      et_row_no      =              " Numeric IDs of Selected Rows
    ).

    READ TABLE lt_rowindex2 INTO DATA(ls_read2) INDEX 1.

    IF sy-subrc = 0.
      READ TABLE mt_data INTO DATA(ls_data2) INDEX ls_read2-index.
      IF sy-subrc = 0.
        SELECT kunag, waerk FROM vbrk INTO TABLE @DATA(lt_poheader_data)
          WHERE vbeln = @ls_data2-vbeln.

        SELECT matnr, arktx, fkimg, vrkme FROM vbrp INTO TABLE @DATA(lt_poitem_data)
          WHERE vbeln = @ls_data2-vbeln.

      ENDIF.
    ENDIF.

    READ TABLE lt_poheader_data INTO DATA(ls_poheader_data) INDEX 1.

    IF sy-subrc = 0.

      ls_poheader-comp_code = ls_poheader_data-kunag.
      ls_poheader-doc_type  = 'NB'.
      READ TABLE gt_sas INTO DATA(ls_ekorg) WITH KEY fieldname = 'EKORG'.
      READ TABLE gt_sas INTO DATA(ls_ekgrp) WITH KEY fieldname = 'EKGRP' .
      ls_poheader-purch_org = ls_ekorg-value.
      ls_poheader-pur_group = ls_ekgrp-value.
      ls_poheader-currency  = ls_poheader_data-waerk.

      ls_poheaderx-comp_code = abap_true.
      ls_poheaderx-doc_type  = abap_true.
      ls_poheaderx-purch_org = abap_true.
      ls_poheaderx-pur_group = abap_true.
      ls_poheaderx-currency  = abap_true.

      READ TABLE lt_poitem_data INTO DATA(ls_poitem_data) INDEX 1.

      ls_poitem-po_item    += 10.
      ls_poitem-material   = ls_poitem_data-matnr.
      ls_poitem-short_text = ls_poitem_data-arktx.
      READ TABLE gt_sas INTO DATA(ls_werks) WITH KEY fieldname = 'WERKS'.
      ls_poitem-plant      = ls_werks-value.
      ls_poitem-stge_loc   = ls_werks-value.
      ls_poitem-matl_group = 'X'.
      ls_poitem-quantity   = ls_poitem_data-fkimg.
      ls_poitem-po_unit    = ls_poitem_data-vrkme.
      ls_poitem-info_upd   = 'X'.
      ls_poitem-pckg_no    = '01'.

      ls_poitemx-po_item    = abap_true.
      ls_poitemx-material   = abap_true.
      ls_poitemx-short_text = abap_true.
      ls_poitemx-plant      = abap_true.
      ls_poitemx-stge_loc   = abap_true.
      ls_poitemx-matl_group = abap_true.
      ls_poitemx-quantity   = abap_true.
      ls_poitemx-po_unit    = abap_true.
      ls_poitemx-info_upd   = abap_true.
      ls_poitemx-pckg_no    = abap_true.

      APPEND ls_poitem TO lt_poitem.
      APPEND ls_poitemx TO lt_poitemx.

      ls_poaccount-po_item += 10.
      ls_poaccount-serial_no = '01'.

      ls_poaccountx-po_item   = abap_true.
      ls_poaccountx-serial_no = abap_true.

      APPEND ls_poaccount TO lt_poaccount.
      APPEND ls_poaccountx TO lt_poaccountx.

    ENDIF.

    CALL FUNCTION 'BAPI_PO_CREATE1'
      EXPORTING
        poheader         = ls_poheader
        poheaderx        = ls_poheaderx
      IMPORTING
        exppurchaseorder = ex_po_number "çıkan sonuç buna gelecek
        expheader        = exp_head
      TABLES
        return           = lt_return
        poitem           = lt_poitem
        poitemx          = lt_poitemx
        poaccount        = lt_poaccount
        poaccountx       = lt_poaccountx.

*    LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'E' . "E- error demek (E_W) "içinde e varsa loopten çık
*      EXIT.
*    ENDLOOP.

    LOOP AT lt_return INTO DATA(ls_bapi_return).  " BAPI den dönen mesjları alıyorum, popup olarak ekrana basıyor
      lv_numup += 1.
      IF ls_bapi_return-type EQ 'E'.
        ls_tab-msgid  = ls_bapi_return-number.
        ls_tab-msgno  = ls_bapi_return-log_msg_no.
        ls_tab-msgty  = 'E'.
        ls_tab-msgv1  = ls_bapi_return-message.
        ls_tab-lineno = lv_numup.
        APPEND ls_tab TO lt_tab. CLEAR ls_tab.
      ELSEIF ls_bapi_return-type EQ 'W'.
        ls_tab-msgid  = ls_bapi_return-number.
        ls_tab-msgno  = ls_bapi_return-log_msg_no.
        ls_tab-msgty  = 'W'.
        ls_tab-msgv1  = ls_bapi_return-message.
        ls_tab-lineno = lv_numup.
        APPEND ls_tab TO lt_tab.CLEAR ls_tab.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = lt_tab.                 " Additional Messages

    READ TABLE mt_data ASSIGNING FIELD-SYMBOL(<lfs_dta>) INDEX ls_read2-index. "bapi çalışmadığı için sabit değer ataması yapıldı
    IF sy-subrc = 0.
      <lfs_dta>-ebeln = '3000130400'.
    ENDIF.

    IF sy-subrc = 0.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' "hata varsa hataları geri alıyor - veri tutarlığını korumak amacı
        IMPORTING
          return = ls_return.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'   "işlemler başarılıysa
        EXPORTING
          wait = abap_true.
    ENDIF.

    CALL METHOD go_alv2->refresh_table_display.

  ENDMETHOD.

ENDCLASS.
