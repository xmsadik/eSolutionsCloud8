CLASS zcl_etr_ledger_create DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.


*  TYPES: BEGIN OF lty_cash_sel,
*           comp_code      TYPE tcj_positions-comp_code,
*           cajo_number    TYPE tcj_positions-cajo_number,
*           posting_number TYPE tcj_positions-posting_number,
*         END OF lty_cash_sel.
*

*    TYPES: BEGIN OF ty_blart,
*             blart   TYPE string, "/itetr/edf_hbbtr-blart,
*             hkon1   TYPE string, "/itetr/edf_hbbtr-hkon1,
*             hflg1   TYPE c LENGTH 1,
*             hkon2   TYPE  string, "/itetr/edf_hbbtr-hkon2,
*             hflg2   TYPE c LENGTH 1,
*             hkon3   TYPE  string, "/itetr/edf_hbbtr-hkon3,
*             hflg3   TYPE c LENGTH 1,
*             hkon4   TYPE  string, "/itetr/edf_hbbtr-hkon4,
*             hflg4   TYPE c LENGTH 1,
*             blart_t TYPE  string, "/itetr/edf_defky-blart_t,
*             gbtur   TYPE  string, "/itetr/edf_defky-gbtur,
*             oturu   TYPE  string, "/itetr/edf_defky-oturu,
*             chcok   TYPE c LENGTH 1,
*           END OF ty_blart.

    TYPES : BEGIN OF ty_skb1,
              altkt TYPE saknr,
              saknr TYPE c LENGTH 10,
            END OF ty_skb1.

    TYPES: BEGIN OF ty_colitem,
             bukrs TYPE bukrs,
             belnr TYPE belnr_d,
             gjahr TYPE gjahr,
             buzei TYPE buzei,
             docln TYPE c LENGTH 6, "docln6, #Change
             cldoc TYPE zetr_e_cldoc,
           END OF ty_colitem.

    TYPES : BEGIN OF ty_bkpf ,
              bukrs TYPE bukrs,
              belnr TYPE belnr_d,
              gjahr TYPE gjahr,
              blart TYPE blart,
              awtyp TYPE c LENGTH 5,
              awkey TYPE c LENGTH 20,
              stblg TYPE belnr_d,
              xblnr TYPE xblnr1,
              bstat TYPE zetr_e_bstat,
              tcode TYPE tcode,
              bktxt TYPE bktxt,
              budat TYPE budat,
              bldat TYPE bldat,
              rldnr TYPE fins_ledger,

            END OF ty_bkpf.


    TYPES : BEGIN OF ty_bseg ,
              bukrs TYPE bukrs,
              hkont TYPE hkont,
              gjahr TYPE gjahr,
              buzei TYPE buzei,
              gsber TYPE gsber,
              lokkt TYPE c LENGTH 10,
              belnr TYPE belnr_d,
              docln TYPE c LENGTH 6,
              budat TYPE budat,
              bldat TYPE bldat,
              rhcur TYPE waers,
              blart TYPE blart,
              drcrk TYPE shkzg,
              mwskz TYPE mwskz,
              hsl   TYPE zetr_e_edf_dmbtr,
              lifnr TYPE lifnr,
              kunnr TYPE lifnr,
              koart TYPE koart,
              sgtxt TYPE sgtxt,
              shkzg TYPE c LENGTH 1,
              dmbtr TYPE zetr_e_edf_dmbtr,
              posn2 TYPE n LENGTH 6,
              xcpdd TYPE c LENGTH 1,
              wrbtr TYPE zetr_e_edf_dmbtr,
              dmbe3 TYPE zetr_e_edf_dmbtr,
              dmbe2 TYPE zetr_e_edf_dmbtr,
              waers TYPE zetr_e_waers,
            END OF ty_bseg.


    TYPES : BEGIN OF ty_bsec,
              bukrs TYPE bukrs,
              belnr TYPE belnr_d,
              gjahr TYPE gjahr,
              buzei TYPE buzei,
              name1 TYPE name1_gp,
              name2 TYPE name1_gp,
              name3 TYPE name1_gp,
              name4 TYPE name1_gp,
            END OF ty_bsec.

    TYPES : BEGIN OF ty_bsed,
              bukrs TYPE bukrs,
              belnr TYPE belnr_d,
              gjahr TYPE gjahr,
              buzei TYPE buzei,
              boeno TYPE c LENGTH 10,

            END OF ty_bsed.

    TYPES : BEGIN OF ty_kna1 ,
              kunnr TYPE lifnr,
              name1 TYPE name1_gp,
              name2 TYPE name1_gp,
            END OF ty_kna1.


    TYPES : BEGIN OF ty_lfa1 ,
              lifnr TYPE lifnr,
              name1 TYPE name1_gp,
              name2 TYPE name1_gp,
            END OF ty_lfa1.


    TYPES: BEGIN OF ty_gsber,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE gsber,
             high   TYPE gsber,
           END OF ty_gsber.

    TYPES: BEGIN OF ty_hkont,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE hkont,
             high   TYPE hkont,
           END OF ty_hkont.

    TYPES: BEGIN OF ty_dybel,
             bukrs TYPE bukrs,
             belnr TYPE belnr_d,
             gjahr TYPE gjahr,
           END OF ty_dybel.

    TYPES: BEGIN OF ty_blryb,
             bukrs TYPE bukrs,
             blart TYPE blart,
           END OF ty_blryb.


    TYPES : BEGIN OF ty_defky,
              bukrs     TYPE bukrs,
              budat     TYPE budat,
              belnr     TYPE belnr_d,
              gjahr     TYPE gjahr,
              buzei     TYPE buzei,
              docln     TYPE zetr_e_docln6,
              bcode     TYPE zetr_e_bcode,
              monat     TYPE monat,
              tsfyd     TYPE zetr_e_ledger_in_purge,
              partn     TYPE zetr_e_edf_part_no,
              dfbuz     TYPE zetr_e_edf_defter_klmno,
              linen     TYPE zetr_e_edf_sira_no,
              yevno     TYPE zetr_e_edf_yvmy_no,
              blart     TYPE blart,
              gbtur     TYPE zetr_e_gib_doc_type,
              blart_t   TYPE zetr_e_edf_ledger_on_defin,
              oturu     TYPE zetr_e_payment_term,
              bldat     TYPE bldat,
              cpudt     TYPE zetr_e_cpudt,
              usnam     TYPE usnam,
              tcode     TYPE tcode,
              xblnr     TYPE zetr_e_xblnr,
              stblg     TYPE zetr_e_stblg,
              stjah     TYPE gjahr,
              bktxt     TYPE zetr_e_edf_bktxt,
              waers     TYPE waers,
              bstat     TYPE zetr_e_bstat,
              awtyp     TYPE zetr_e_awtyp,
              awkey     TYPE c LENGTH 20,
              xblnr_alt TYPE c LENGTH 26,
              bschl     TYPE bschl,
              koart     TYPE koart,
              umskz     TYPE c LENGTH 1,
              shkzg     TYPE shkzg,
              shkzg_srt TYPE zetr_e_edf_shkzg_srt,
              gsber     TYPE gsber,
              mwskz     TYPE mwskz,
              dmbtr_def TYPE zetr_e_edf_defter_ynsyn_tut,
              dmbtr     TYPE zetr_e_edf_dmbtr,
              ktosl     TYPE c LENGTH 3,
              zuonr     TYPE dzuonr,
              sgtxt     TYPE sgtxt,
              altkt     TYPE c LENGTH 10,
              kostl     TYPE kostl,
              pernr     TYPE n LENGTH 8,
              saknr     TYPE c LENGTH 10,
              hkont     TYPE hkont,
              hkont_3   TYPE zetr_e_edf_ana_hesap,
              xcpdd     TYPE abap_boolean,
              kunnr     TYPE c LENGTH 10,
              lifnr     TYPE lifnr,
              kname     TYPE zetr_e_edf_musteri_ad,
              lname     TYPE zetr_e_edf_satici_ad,
              zlsch     TYPE c LENGTH 1,
              xnegp     TYPE abap_boolean,
              dif45     TYPE abap_boolean,
              cldoc     TYPE c LENGTH 255,
              clitm     TYPE abap_boolean,
              orbuk     TYPE c LENGTH 4,
            END OF ty_defky.

    TYPES: BEGIN OF ty_faglflexa,
             buzei  TYPE buzei,
             rbukrs TYPE bukrs,
             docln  TYPE zetr_e_docln6,
             hsl    TYPE zetr_e_edf_dmbtr,
             ksl    TYPE zetr_e_edf_dmbtr,
             osl    TYPE zetr_e_edf_dmbtr,
             tsl    TYPE zetr_e_edf_dmbtr,
             drcrk  TYPE shkzg,
             docnr  TYPE belnr_d,
             ryear  TYPE gjahr,
             racct  TYPE saknr,
           END OF ty_faglflexa.

    TYPES: BEGIN OF ty_blart,
             blart   TYPE blart,
             blart_t TYPE zetr_e_edf_ledger_on_defin,
             gbtur   TYPE zetr_e_gib_doc_type,
             oturu   TYPE zetr_e_payment_term,
             gibbta  TYPE zetr_e_gibbta,
             ocblg   TYPE zetr_e_ocblg,
           END OF ty_blart.


    TYPES: tt_hkont TYPE TABLE OF ty_hkont.
    TYPES: tt_gsber TYPE TABLE OF ty_gsber.
    TYPES: tt_bkpf  TYPE TABLE OF ty_bkpf .
    TYPES: tt_dybel TYPE TABLE OF ty_dybel.
    TYPES: tt_blryb TYPE TABLE OF ty_blryb .
    TYPES: tt_defky TYPE TABLE OF ty_defky .
    TYPES: tt_skb1 TYPE TABLE OF ty_skb1 .
    TYPES: tt_colitem TYPE SORTED TABLE OF ty_colitem WITH UNIQUE KEY bukrs belnr gjahr buzei.
    TYPES : tt_blart TYPE TABLE OF ty_blart.



    CLASS-METHODS create
      IMPORTING
        !is_params      TYPE zetr_t_dopvr
        !is_bukrs       TYPE zetr_t_srkdb
        !iv_bukrs       TYPE bukrs
        iv_gjahr        TYPE gjahr
*        IV_BCODE
        !iv_monat       TYPE monat
        !iv_alternative TYPE zetr_e_edf_alternative
        !iv_f51_blart   TYPE blart
        !iv_f51_tcode   TYPE tcode
        !iv_tasfiye     TYPE zetr_e_edf_alternative
        !iv_ledger      TYPE zetr_e_edf_alternative
      CHANGING
        t_bkpf          TYPE tt_bkpf
        t_ex_docs       TYPE tt_dybel
*        T_WRONG_TYPES type zetr_t_BTHBL
        t_copy_belnr    TYPE tt_blryb
*        T_CASH
        tr_hkont        TYPE tt_hkont
*        tr_gsber        TYPE tt_gsber "Kullanılmıyor @YiğitcanÖ.
*        t_ledger        TYPE tt_defky "Kullanılmıyor @YiğitcanÖ.
        t_skb1          TYPE tt_skb1
        t_blart         TYPE tt_blart

      .

