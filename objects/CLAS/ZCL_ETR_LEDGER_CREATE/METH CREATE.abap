  METHOD create.

    DATA: "t_cash_sel     TYPE TABLE OF lty_cash_sel WITH HEADER LINE,
      "t_cash_data    TYPE SORTED TABLE OF tcj_positions WITH UNIQUE KEY comp_code cajo_number posting_number transact_number WITH HEADER LINE,
      "t_cash_post    TYPE SORTED TABLE OF tcj_positions WITH NON-UNIQUE KEY comp_code cajo_number posting_number WITH HEADER LINE,
      t_bkpf_add     TYPE TABLE OF ty_bkpf, "OCCURS 0 WITH HEADER LINE,
      lt_bseg        TYPE SORTED TABLE OF ty_bseg WITH NON-UNIQUE KEY bukrs belnr gjahr buzei hkont lokkt gsber kunnr lifnr, "WITH HEADER LINE,
      lt_new_bseg    TYPE SORTED TABLE OF ty_bseg WITH NON-UNIQUE KEY bukrs belnr gjahr buzei hkont lokkt gsber kunnr lifnr, "WITH HEADER LINE,
      ls_bseg        LIKE LINE OF lt_bseg,
      lt_bseg_tmp    TYPE TABLE OF ty_bseg, "bseg OCCURS 0 WITH HEADER LINE,
      ls_bseg_tmp    TYPE  ty_bseg,
      lt_bseg_dat    TYPE TABLE OF ty_bseg, "OCCURS 0 WITH HEADER LINE,
      lt_colitm      TYPE SORTED TABLE OF ty_colitem WITH UNIQUE KEY bukrs belnr gjahr buzei docln , "WITH HEADER LINE,
*      lt_colitm_bseg TYPE SORTED TABLE OF ty_colitem WITH UNIQUE KEY bukrs belnr gjahr , "WITH HEADER LINE,
      lt_colitm_bseg TYPE tt_colitem,
      ls_colitm_bseg TYPE ty_colitem,
      ls_colitm      LIKE LINE OF lt_colitm,
      lt_bsec        TYPE SORTED TABLE OF ty_bsec WITH UNIQUE KEY bukrs belnr gjahr buzei , "WITH HEADER LINE,
      lt_bsed        TYPE SORTED TABLE OF ty_bsed WITH UNIQUE KEY bukrs belnr gjahr buzei , "WITH HEADER LINE,
*      lt_payr        TYPE SORTED TABLE OF payr WITH NON-UNIQUE KEY zbukr vblnr gjahr ,"WITH HEADER LINE,
      lt_bsec_sel    TYPE TABLE OF ty_bseg,  "OCCURS    0 WITH HEADER LINE,
      ls_bsec_sel    TYPE ty_bseg,
      lt_faglflexa   TYPE SORTED TABLE OF ty_faglflexa WITH NON-UNIQUE KEY rbukrs docnr ryear buzei docln , "WITH HEADER LINE,
      lt_ledger      TYPE TABLE OF zetr_t_defky , "OCCURS 0 WITH HEADER LINE,
      ls_ledger      LIKE LINE OF lt_ledger,
      lc_root        TYPE REF TO cx_root,
      lt_kna1_tmp    TYPE TABLE OF ty_kna1, " kna1 OCCURS 0 WITH HEADER LINE,
      ls_kna1_tmp    TYPE ty_kna1,
      lt_lfa1_tmp    TYPE TABLE OF ty_lfa1, "kna1 OCCURS 0 WITH HEADER LINE,
      ls_lfa1_tmp    TYPE ty_lfa1,
      lt_kna1        TYPE SORTED TABLE OF ty_kna1 WITH NON-UNIQUE KEY kunnr , "WITH HEADER LINE,
      lt_lfa1        TYPE SORTED TABLE OF ty_lfa1 WITH NON-UNIQUE KEY lifnr, " WITH HEADER LINE,
      lv_stblg       TYPE belnr_d, "rbkp-stblg,
      lv_stjah       TYPE gjahr, "rbkp-stjah,
      lv_awkey       TYPE c LENGTH 20, "bkpf-awkey,
      lv_satici      TYPE c LENGTH 1,
      lv_anahesap    TYPE c LENGTH 1,
      lv_count       TYPE i,
*"      lt_blart       TYPE SORTED TABLE OF /itetr/edf_hbbtr WITH NON-UNIQUE KEY blart," WITH HEADER LINE,
*"      lt_blart_tmp   TYPE SORTED TABLE OF ty_blart WITH NON-UNIQUE KEY blart ,"WITH HEADER LINE,
      lv_count_th    TYPE i,
      lv_count_fh    TYPE i,
      lv_skip        TYPE c LENGTH 1,
      lv_hkont       TYPE hkont,
      lv_bktxt       TYPE zetr_e_edf_bktxt,
      lv_45day       TYPE i,
      lv_char        TYPE c LENGTH 1000,
      lv_changed     TYPE c LENGTH 1,
      lv_subrc       TYPE sy-subrc,
      lv_count_send  TYPE i,
      lv_alternative TYPE c LENGTH 1.

    LOOP AT t_bkpf INTO DATA(ls_bkpf).
      READ TABLE t_ex_docs INTO DATA(ls_ex_docs) WITH KEY belnr = ls_bkpf-belnr
                                                          gjahr = ls_bkpf-gjahr.
      IF sy-subrc EQ 0.
        DELETE t_bkpf INDEX sy-tabix.
        CONTINUE.
      ENDIF.

*      READ TABLE t_wrong_types WITH KEY bukrs = t_bkpf-bukrs
*                                         belnr = t_bkpf-belnr
*                                         gjahr = t_bkpf-gjahr.
*      IF sy-subrc EQ 0.
*        t_bkpf-blart = t_wro!ng_types-blart.
*        MODIFY t_bkpf TRANSPORTING blart.
*      ENDIF.

      "Miro Faturaları Ters Kayıt Kontrolü
      IF ls_bkpf-awtyp EQ 'RMRP'.
        CLEAR : lv_stblg,lv_stjah,lv_awkey.

        SELECT SINGLE reversedocument AS stblg,
                      reversedocumentfiscalyear AS stjah
        FROM i_supplierinvoiceapi01
        WHERE supplierinvoice = @ls_bkpf-awkey(10)
            AND fiscalyear    = @ls_bkpf-awkey+10(4)
            AND companycode   = @ls_bkpf-bukrs
        INTO (@lv_stblg, @lv_stjah).


        IF lv_stblg IS NOT INITIAL.
          CONCATENATE lv_stblg lv_stjah INTO lv_awkey.

          SELECT SINGLE accountingdocument AS belnr
            FROM i_journalentry
           WHERE companycode EQ @ls_bkpf-bukrs
             AND fiscalyear   EQ @lv_stjah
             AND referencedocumenttype   EQ 'RMRP'
             AND originalreferencedocument   EQ @lv_awkey
             INTO @ls_bkpf-stblg.
          IF sy-subrc EQ 0.
            MODIFY t_bkpf FROM ls_bkpf."TRANSPORTING stblg.
          ENDIF.
        ENDIF.

      ENDIF.

      READ TABLE t_copy_belnr INTO DATA(ls_copy_belnr) WITH KEY blart = ls_bkpf-blart.
      IF sy-subrc EQ 0 AND ls_bkpf-xblnr IS INITIAL.
        ls_bkpf-xblnr = ls_bkpf-belnr.
        MODIFY t_bkpf FROM ls_bkpf ."TRANSPORTING xblnr.
      ENDIF.

*      IF is_params-xhana IS NOT INITIAL.
      SELECT companycode AS bukrs,
             glaccount  AS hkont,
             alternativeglaccount  AS lokkt,
             fiscalyear  AS gjahr,
             accountingdocument  AS belnr,
             ledgergllineitem  AS docln,
             postingdate  AS budat,
             documentdate  AS bldat,
             transactioncurrency  AS waers,
             accountingdocumenttype  AS blart,
             taxcode  AS mwskz,
             amountincompanycodecurrency    AS dmbtr,
             supplier  AS lifnr,
             customer  AS kunnr,
             financialaccounttype  AS koart,
             documentitemtext  AS sgtxt,
             debitcreditcode AS shkzg,
             amountinglobalcurrency AS dmbe2,
             amountinglobalcurrency AS dmbe3,
             amountintransactioncurrency AS wrbtr
      FROM i_journalentryitem "acdoca
       WHERE ledger EQ @is_bukrs-rldnr
         AND companycode EQ @ls_bkpf-bukrs
         AND accountingdocument EQ @ls_bkpf-belnr
         AND fiscalyear EQ @ls_bkpf-gjahr
       APPENDING CORRESPONDING FIELDS OF TABLE @lt_bseg.

*      ELSE.
*
*        SELECT *
*          FROM bseg
*          APPENDING TABLE lt_bseg
*         WHERE bukrs EQ t_bkpf-bukrs
*           AND belnr EQ t_bkpf-belnr
*           AND gjahr EQ t_bkpf-gjahr.
*
*      ENDIF.

      IF is_params-dfvhs IS NOT INITIAL.
        lv_subrc = 4.
        LOOP AT lt_bseg TRANSPORTING NO FIELDS
                        WHERE bukrs = ls_bkpf-bukrs
                           AND belnr = ls_bkpf-belnr
                           AND gjahr = ls_bkpf-gjahr
                           AND shkzg = 'S'
                           AND dmbtr < 0.
          lv_subrc = 0.
          EXIT.
        ENDLOOP.

        LOOP AT lt_bseg TRANSPORTING NO FIELDS
                        WHERE bukrs = ls_bkpf-bukrs
                          AND belnr = ls_bkpf-belnr
                          AND gjahr = ls_bkpf-gjahr
                          AND shkzg = 'H'
                          AND dmbtr > 0.
          lv_subrc = 0.
          EXIT.
        ENDLOOP.

        IF lv_subrc EQ 0.
          SELECT SINGLE COUNT(*)
            FROM i_journalentryitem
           WHERE companycode EQ @ls_bkpf-bukrs
             AND accountingdocument EQ @ls_bkpf-belnr
             AND fiscalyear EQ @ls_bkpf-gjahr.
          IF sy-subrc EQ 0.
            DELETE lt_bseg WHERE bukrs EQ ls_bkpf-bukrs
                             AND belnr EQ ls_bkpf-belnr
                             AND gjahr EQ ls_bkpf-gjahr.

*            SELECT *
*              FROM bseg
*              APPENDING TABLE lt_bseg
*             WHERE bukrs EQ t_bkpf-bukrs
*               AND belnr EQ t_bkpf-belnr
*               AND gjahr EQ t_bkpf-gjahr.

*Gerek var mı test ederken bak @YiğitcanÖ.

**            SELECT companycode AS bukrs,
**                       glaccount  AS hkont,
**                       alternativeglaccount  AS lokkt,
**                       fiscalyear  AS gjahr,
**                       accountingdocument  AS belnr,
**                       ledgergllineitem  AS docln,
**                       postingdate  AS budat,
**                       documentdate  AS bldat,
**                       transactioncurrency  AS waers,
**                       accountingdocumenttype  AS blart,
**                       taxcode  AS mwskz,
**                       amountincompanycodecurrency    AS dmbtr,
**                       supplier  AS lifnr,
**                       customer  AS kunnr,
**                       financialaccounttype  AS koart,
**                       documentitemtext  AS sgtxt,
**                       debitcreditcode AS shkzg,
**                       amountinglobalcurrency AS dmbe2,
**                       amountinglobalcurrency AS dmbe3,
**                       amountintransactioncurrency AS wrbtr
**            FROM i_journalentryitem
**            WHERE companycode        = @ls_bkpf-bukrs
**              AND accountingdocument = @ls_bkpf-belnr
**              AND fiscalyear         = @ls_bkpf-gjahr
**              INTO CORRESPONDING FIELDS OF TABLE @lt_new_bseg.
**
**            INSERT LINES OF lt_new_bseg INTO TABLE lt_bseg.

*Gerek var mı test ederken bak @YiğitcanÖ.

            CLEAR ls_colitm.
            MOVE-CORRESPONDING ls_bkpf TO ls_colitm.
            INSERT ls_colitm INTO TABLE lt_colitm_bseg.
          ENDIF.
        ENDIF.
      ENDIF.

      IF is_params-rflop IS NOT INITIAL.
        SELECT accountingdocumentitem AS buzei,
               companycode AS rbukrs,
               ledgergllineitem AS docln,
               amountincompanycodecurrency AS hsl,
               amountinglobalcurrency AS ksl,
               amountintransactioncurrency AS  osl,
               amountinglobalcurrency AS tsl,
               debitcreditcode AS drcrk,
               accountingdocument AS docnr,
               fiscalyear AS ryear,
               glaccount AS racct
          FROM i_glaccountlineitem
         WHERE fiscalyear  EQ @ls_bkpf-gjahr
           AND accountingdocument  EQ @ls_bkpf-belnr
           AND ledger  EQ @ls_bkpf-rldnr
           AND companycode EQ @ls_bkpf-bukrs
           APPENDING CORRESPONDING FIELDS OF TABLE @lt_faglflexa.
      ENDIF.

      IF ls_bkpf-bstat EQ 'L'.
        APPEND ls_bkpf TO t_bkpf_add.
      ENDIF.

*      READ TABLE t_cash WITH KEY blart = t_bkpf-blart.
*      IF sy-subrc EQ 0.
*        CLEAR t_cash_sel.
*        t_cash_sel-comp_code      = t_bkpf-bukrs.
*        t_cash_sel-posting_number = t_bkpf-awkey+0(10).
*        t_cash_sel-cajo_number    = t_bkpf-awkey+10(4).
*        APPEND t_cash_sel.CLEAR t_cash_sel.
*      ENDIF.
    ENDLOOP.

    IF t_bkpf_add[] IS NOT INITIAL.
      SELECT  companycode AS rbukrs,
              glaccount AS racct,
              alternativeglaccount AS lokkt,
              fiscalyear AS gjahr,
              accountingdocument AS belnr,
*                ledgergllineitem AS docln,
*                PostingDate AS budat,
*                documentdate AS bldat,
              companycodecurrency AS rhcur,
*                accountingdocumenttype AS blart,
              debitcreditcode AS drcrk,
              taxcode AS mwskz,
              amountincompanycodecurrency AS hsl,
*                supplier AS lifnr,
*                customer AS kunnr,
              financialaccounttype AS koart,
              documentitemtext AS sgtxt
        FROM i_addlledgeroplacctgdocitem
         FOR ALL ENTRIES IN @t_bkpf_add
       WHERE companycode EQ @t_bkpf_add-bukrs
         AND accountingdocument EQ @t_bkpf_add-belnr
         AND fiscalyear EQ @t_bkpf_add-gjahr
          APPENDING CORRESPONDING FIELDS OF TABLE @lt_bseg.
    ENDIF.


*      CALL FUNCTION '/ITETR/EDF_EXIT_006'
*    EXPORTING
*      i_bukrs   = iv_bukrs
*      i_bcode   = iv_bcode
*      i_gjahr   = iv_gjahr
*      i_monat   = iv_monat
*    IMPORTING
*      c_changed = lv_changed
*    TABLES
*      t_bseg    = lt_bseg_tmp.

    IF lv_changed IS NOT INITIAL.
      lt_bseg[] = lt_bseg_tmp[].
    ENDIF.

    CLEAR:lt_bseg_tmp,lt_bseg_tmp[].

    IF iv_alternative IS NOT INITIAL.
      DELETE lt_bseg WHERE lokkt NOT IN tr_hkont.
    ELSE.
      DELETE lt_bseg WHERE hkont NOT IN tr_hkont.
    ENDIF.
*    DELETE lt_bseg WHERE gsber NOT IN tr_gsber. "Kullanılmıyor @Yiğitcan
    IF lt_bseg[] IS INITIAL.
      EXIT.
    ENDIF.

    IF is_params-rfagl IS NOT INITIAL.
      IF is_params-rflop IS INITIAL.
        SELECT accountingdocumentitem AS buzei,
                 companycode AS rbukrs,
                 ledgergllineitem AS docln,
                 amountincompanycodecurrency AS hsl,
                 amountinglobalcurrency AS ksl,
                 amountintransactioncurrency AS  osl,
                 amountinglobalcurrency AS tsl,
                 debitcreditcode AS drcrk,
                 accountingdocument AS docnr,
                 fiscalyear AS ryear,
                 glaccount AS racct
            FROM i_glaccountlineitem
            FOR ALL ENTRIES IN @t_bkpf
         WHERE fiscalyear  EQ @t_bkpf-gjahr
           AND accountingdocument EQ @t_bkpf-belnr
           AND ledger  EQ @is_bukrs-rldnr
           AND companycode EQ @t_bkpf-bukrs
           AND glaccount IN @tr_hkont
           INTO CORRESPONDING FIELDS OF TABLE @lt_faglflexa .
      ENDIF.

      IF is_params-nbseg IS INITIAL.
        DELETE lt_faglflexa WHERE buzei IS INITIAL.
      ENDIF.

      LOOP AT lt_bseg INTO DATA(ls_bseg2) .

        LOOP AT lt_faglflexa INTO DATA(ls_faglflexa) WHERE rbukrs = ls_bseg2-bukrs
                               AND docnr  = ls_bseg2-belnr
                               AND ryear  = ls_bseg2-gjahr
                               AND buzei  = ls_bseg2-buzei.
          CLEAR ls_bseg_tmp.
          ls_bseg_tmp = ls_bseg2.

          ls_bseg_tmp-posn2 = ls_faglflexa-docln. "posn2 alanını docln olarak kullanıyorum
          ls_bseg_tmp-dmbtr = abs( ls_faglflexa-hsl ).
          ls_bseg_tmp-dmbe2 = abs( ls_faglflexa-ksl ).
          ls_bseg_tmp-dmbe2 = abs( ls_faglflexa-osl ).
          ls_bseg_tmp-wrbtr = abs( ls_faglflexa-tsl ).
          ls_bseg_tmp-shkzg = ls_faglflexa-drcrk.
          APPEND ls_bseg_tmp TO lt_bseg_tmp.CLEAR lt_bseg_tmp.

          DELETE lt_faglflexa WHERE rbukrs = ls_bseg2-bukrs
                               AND docnr  = ls_bseg2-belnr
                               AND ryear  = ls_bseg2-gjahr
                               AND buzei  = ls_bseg2-buzei.
          CONTINUE.
        ENDLOOP.
        IF sy-subrc EQ 0.
          DELETE lt_bseg.CONTINUE.
        ENDIF.
      ENDLOOP.

      IF is_params-nbseg IS NOT INITIAL.
        "Kalem Numarası 0 olup yinede yansıması gereken kayıtlar
        LOOP AT lt_faglflexa INTO ls_faglflexa WHERE buzei = '000'.
*        CLEAR : lt_bseg_tmp.
          MOVE-CORRESPONDING ls_faglflexa TO ls_bseg_tmp.
          ls_bseg_tmp-bukrs = ls_faglflexa-rbukrs.
          ls_bseg_tmp-belnr = ls_faglflexa-docnr.
          ls_bseg_tmp-gjahr = ls_faglflexa-ryear.
          ls_bseg_tmp-buzei = ls_faglflexa-buzei.
          ls_bseg_tmp-posn2 = ls_faglflexa-docln. "posn2 alanını docln olarak kullanıyorum
          ls_bseg_tmp-hkont = ls_faglflexa-racct.
          READ TABLE t_skb1 INTO DATA(ls_skb1) WITH KEY saknr = ls_faglflexa-racct.
          IF sy-subrc EQ 0.
            ls_bseg_tmp-lokkt = ls_skb1-altkt.
          ENDIF.
          ls_bseg_tmp-dmbtr = abs( ls_faglflexa-hsl ).
          ls_bseg_tmp-dmbe2 = abs( ls_faglflexa-ksl ).
          ls_bseg_tmp-dmbe3 = abs( ls_faglflexa-osl ).
          ls_bseg_tmp-wrbtr = abs( ls_faglflexa-tsl ).
          ls_bseg_tmp-shkzg = ls_faglflexa-drcrk.
          APPEND ls_bseg_tmp TO lt_bseg_tmp.CLEAR ls_bseg_tmp.
        ENDLOOP.
      ENDIF.

      IF lt_bseg_tmp[] IS NOT INITIAL.
        CLEAR:lt_bseg,lt_bseg[].
        lt_bseg[] = lt_bseg_tmp[].
      ENDIF.
    ENDIF.



    CLEAR:lt_kna1_tmp,lt_kna1_tmp[],lt_lfa1_tmp,lt_lfa1_tmp[],
           lt_kna1,lt_kna1[],lt_lfa1,lt_lfa1[].

    LOOP AT lt_bseg ASSIGNING FIELD-SYMBOL(<fs_bseg>).

      IF <fs_bseg>-buzei IS INITIAL AND <fs_bseg>-docln IS NOT INITIAL.
        <fs_bseg>-posn2 = <fs_bseg>-docln.
      ENDIF.

      IF <fs_bseg>-kunnr NE space OR <fs_bseg>-lifnr NE space.
        IF <fs_bseg>-kunnr IS NOT INITIAL.
          ls_kna1_tmp-kunnr = <fs_bseg>-kunnr.
          COLLECT ls_kna1_tmp INTO lt_kna1_tmp.
        ELSEIF <fs_bseg>-lifnr IS NOT INITIAL.
          ls_lfa1_tmp-lifnr = <fs_bseg>-lifnr.
          COLLECT ls_lfa1_tmp INTO lt_lfa1_tmp.
        ENDIF.

        IF <fs_bseg>-xcpdd IS NOT INITIAL.
          ls_bsec_sel = <fs_bseg>.
          APPEND ls_bsec_sel TO lt_bsec_sel.
          CLEAR ls_bsec_sel.
        ENDIF.
      ENDIF.

    ENDLOOP.


    IF lt_kna1_tmp[] IS NOT INITIAL.
      SELECT customer AS kunnr,
             customername AS name1
        FROM i_customer
         FOR ALL ENTRIES IN @lt_kna1_tmp
       WHERE customer = @lt_kna1_tmp-kunnr
        INTO CORRESPONDING FIELDS OF TABLE @lt_kna1.
    ENDIF.

    IF lt_lfa1_tmp[] IS NOT INITIAL.
      SELECT supplier AS lifnr,
             suppliername AS name1
        FROM i_supplier
         FOR ALL ENTRIES IN @lt_lfa1_tmp
       WHERE supplier = @lt_lfa1_tmp-lifnr
        INTO CORRESPONDING FIELDS OF TABLE @lt_lfa1.
    ENDIF.


    IF lt_bsec_sel[] IS NOT INITIAL.
      SELECT companycode AS bukrs,
             accountingdocument AS belnr,
             fiscalyear AS gjahr,
             accountingdocumentitem AS buzei,
             businesspartnername1 AS name1,
             businesspartnername2 AS name2,
             businesspartnername3 AS name3,
             businesspartnername4 AS name4
        FROM i_journalentryitemonetimedata
         FOR ALL ENTRIES IN @lt_bsec_sel
       WHERE companycode = @lt_bsec_sel-bukrs
         AND accountingdocument = @lt_bsec_sel-belnr
         AND fiscalyear = @lt_bsec_sel-gjahr
         AND accountingdocumentitem = @lt_bsec_sel-buzei
         INTO TABLE @lt_bsec.
    ENDIF.

    IF lt_bseg[] IS NOT INITIAL.
      SELECT companycode AS bukrs,
             accountingdocument AS belnr,
             fiscalyear AS gjahr ,
             accountingdocumentitem AS buzei,
             cheque AS boeno
        FROM i_billofexchange
         FOR ALL ENTRIES IN @lt_bseg
       WHERE companycode = @lt_bseg-bukrs
         AND accountingdocument = @lt_bseg-belnr
         AND fiscalyear = @lt_bseg-gjahr
         AND accountingdocumentitem = @lt_bseg-buzei
         INTO TABLE @lt_bsed.
    ENDIF.

*      IF t_bkpf[] IS NOT INITIAL.
*    SELECT zbukr vblnr gjahr chect
*      FROM payr
*      INTO CORRESPONDING FIELDS OF TABLE lt_payr
*       FOR ALL ENTRIES IN t_bkpf
*     WHERE zbukr EQ t_bkpf-bukrs
*       AND vblnr EQ t_bkpf-belnr
*       AND gjahr EQ t_bkpf-gjahr
*       AND chect NE space.
*  ENDIF.


*  IF t_cash_sel[] IS NOT INITIAL.
*    REFRESH: t_cash_post, t_cash_data.
*    SELECT comp_code cajo_number posting_number transact_number
*      FROM tcj_positions
*      INTO CORRESPONDING FIELDS OF TABLE t_cash_post
*       FOR ALL ENTRIES IN t_cash_sel
*     WHERE comp_code EQ t_cash_sel-comp_code
*       AND cajo_number EQ t_cash_sel-cajo_number
*       AND posting_number EQ t_cash_sel-posting_number.
*
*    LOOP AT t_cash_post.
*
*      CONDENSE t_cash_post-transact_number NO-GAPS.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input  = t_cash_post-transact_number
*        IMPORTING
*          output = t_cash_post-transact_number.
*
*      MODIFY t_cash_post.
*
*    ENDLOOP.
*
*    t_cash_data[] = t_cash_post[].
*
*  ENDIF.

    LOOP AT t_bkpf INTO ls_bkpf.
      CLEAR : ls_ledger,"lt_ledger,
              lv_satici,lv_anahesap,
              lt_bseg_dat,lt_bseg_dat[].

*      LOOP AT t_hbbtr WHERE blart = t_bkpf-blart.
*        MOVE-CORRESPONDING t_hbbtr TO lt_blart_tmp.
*        INSERT TABLE lt_blart_tmp.
*      ENDLOOP.

      IF is_params-dfvhs IS NOT INITIAL.
        "BSEG negatif kayıt tutmuyor
        "BSEG ile çekilen hatalı kayıtlar için bu işlemi yapmasın
        READ TABLE lt_colitm_bseg INTO ls_colitm_bseg WITH KEY bukrs = ls_colitm_bseg-bukrs
                                                                     belnr = ls_colitm_bseg-belnr
                                                                     gjahr = ls_colitm_bseg-gjahr.
        IF sy-subrc NE 0.
          LOOP AT lt_bseg INTO ls_bseg WHERE bukrs = ls_bkpf-bukrs
                                         AND belnr = ls_bkpf-belnr
                                         AND gjahr = ls_bkpf-gjahr
                                         AND shkzg = 'S'
                                         AND dmbtr < 0.

            ls_bseg-dmbtr = abs( ls_bseg-dmbtr ).

            LOOP AT lt_bseg INTO DATA(ls_bseg_inner) WHERE bukrs = ls_bkpf-bukrs
                                                       AND belnr = ls_bkpf-belnr
                                                       AND gjahr = ls_bkpf-gjahr
                                                       AND shkzg = 'S'
                                                       AND dmbtr > ls_bseg-dmbtr.
              IF ls_bseg_inner-buzei EQ ls_bseg-buzei AND
                 ls_bseg_inner-docln EQ ls_bseg-docln.
                CONTINUE.
              ENDIF.

              LOOP AT lt_colitm INTO ls_colitm WHERE bukrs = ls_bseg_inner-bukrs
                                                 AND belnr = ls_bseg_inner-belnr
                                                 AND gjahr = ls_bseg_inner-gjahr
                                                 AND buzei = ls_bseg_inner-buzei
                                                 AND docln = ls_bseg_inner-docln.
                IF ls_bseg-buzei IS NOT INITIAL AND ls_bseg-docln IS NOT INITIAL.
                  CONCATENATE ls_bseg-buzei ',' ls_bseg-docln INTO lv_char.
                ELSEIF ls_bseg-docln IS NOT INITIAL.
                  lv_char = ls_bseg-docln.
                ELSEIF ls_bseg-buzei IS NOT INITIAL.
                  lv_char = ls_bseg-buzei.
                ENDIF.

                CONCATENATE ls_colitm-cldoc '/' lv_char INTO ls_colitm-cldoc.
                MODIFY lt_colitm FROM ls_colitm.
              ENDLOOP.
              IF sy-subrc NE 0.
                CLEAR ls_colitm.

                IF ls_bseg-buzei IS NOT INITIAL AND ls_bseg-docln IS NOT INITIAL.
                  CONCATENATE ls_bseg-buzei ',' ls_bseg-docln INTO lv_char.
                ELSEIF ls_bseg-docln IS NOT INITIAL.
                  lv_char = ls_bseg-docln.
                ELSEIF ls_bseg-buzei IS NOT INITIAL.
                  lv_char = ls_bseg-buzei.
                ENDIF.

                ls_colitm-bukrs = ls_bseg_inner-bukrs.
                ls_colitm-belnr = ls_bseg_inner-belnr.
                ls_colitm-gjahr = ls_bseg_inner-gjahr.
                ls_colitm-buzei = ls_bseg_inner-buzei.
                ls_colitm-docln = ls_bseg_inner-docln.
                ls_colitm-cldoc = lv_char.
                INSERT ls_colitm INTO TABLE lt_colitm.
              ENDIF.

              ls_bseg_inner-dmbtr = ls_bseg_inner-dmbtr - ls_bseg-dmbtr.
              MODIFY lt_bseg FROM ls_bseg_inner. CLEAR ls_bseg_inner.
              EXIT.
            ENDLOOP.

            DELETE lt_bseg WHERE bukrs = ls_bseg-bukrs "DIKKAT
                             AND belnr = ls_bseg-belnr
                             AND gjahr = ls_bseg-gjahr
                             AND buzei = ls_bseg-buzei
                             AND docln = ls_bseg-docln.
            CONTINUE.
          ENDLOOP.

          LOOP AT lt_bseg INTO ls_bseg WHERE bukrs = ls_bkpf-bukrs
                                         AND belnr = ls_bkpf-belnr
                                         AND gjahr = ls_bkpf-gjahr
                                         AND shkzg = 'H'
                                         AND dmbtr > 0.

            ls_bseg-dmbtr = abs( ls_bseg-dmbtr ).

            LOOP AT lt_bseg INTO DATA(ls_bseg_inner2) WHERE bukrs = ls_bkpf-bukrs
                                                        AND belnr = ls_bkpf-belnr
                                                        AND gjahr = ls_bkpf-gjahr
                                                        AND shkzg = 'H'
                                                        AND dmbtr < ls_bseg-dmbtr.

              IF ls_bseg_inner2-buzei EQ ls_bseg-buzei AND
                 ls_bseg_inner2-docln EQ ls_bseg-docln.
                CONTINUE.
              ENDIF.

              LOOP AT lt_colitm INTO ls_colitm WHERE bukrs = ls_bseg_inner2-bukrs
                                                 AND belnr = ls_bseg_inner2-belnr
                                                 AND gjahr = ls_bseg_inner2-gjahr
                                                 AND buzei = ls_bseg_inner2-buzei
                                                 AND docln = ls_bseg_inner2-docln.
                IF ls_bseg-buzei IS NOT INITIAL AND ls_bseg-docln IS NOT INITIAL.
                  CONCATENATE ls_bseg-buzei ',' ls_bseg-docln INTO lv_char.
                ELSEIF ls_bseg-docln IS NOT INITIAL.
                  lv_char = ls_bseg-docln.
                ELSEIF ls_bseg-buzei IS NOT INITIAL.
                  lv_char = ls_bseg-buzei.
                ENDIF.

                CONCATENATE ls_colitm-cldoc '/' lv_char INTO ls_colitm-cldoc.
                MODIFY lt_colitm FROM ls_colitm.
              ENDLOOP.
              IF sy-subrc NE 0.
                CLEAR ls_colitm.

                IF ls_bseg-buzei IS NOT INITIAL AND ls_bseg-docln IS NOT INITIAL.
                  CONCATENATE ls_bseg-buzei ',' ls_bseg-docln INTO lv_char.
                ELSEIF ls_bseg-docln IS NOT INITIAL.
                  lv_char = ls_bseg-docln.
                ELSEIF ls_bseg-buzei IS NOT INITIAL.
                  lv_char = ls_bseg-buzei.
                ENDIF.

                ls_colitm-bukrs = ls_bseg_inner2-bukrs.
                ls_colitm-belnr = ls_bseg_inner2-belnr.
                ls_colitm-gjahr = ls_bseg_inner2-gjahr.
                ls_colitm-buzei = ls_bseg_inner2-buzei.
                ls_colitm-docln = ls_bseg_inner2-docln.
                ls_colitm-cldoc = lv_char.
                INSERT ls_colitm INTO TABLE lt_colitm.
              ENDIF.

              ls_bseg_inner2-dmbtr = ls_bseg_inner2-dmbtr + ls_bseg-dmbtr.
              MODIFY lt_bseg FROM ls_bseg_inner2. CLEAR ls_bseg_inner2.
              EXIT.
            ENDLOOP.

            DELETE lt_bseg WHERE bukrs = ls_bseg-bukrs "DIKKAT
                  AND belnr = ls_bseg-belnr
                  AND gjahr = ls_bseg-gjahr
                  AND buzei = ls_bseg-buzei
                  AND docln = ls_bseg-docln.
            CONTINUE.
          ENDLOOP.
        ENDIF.
      ENDIF.

      LOOP AT lt_bseg INTO DATA(ls_bseg_inner3) WHERE bukrs = ls_bkpf-bukrs
                                            AND belnr = ls_bkpf-belnr
                                            AND gjahr = ls_bkpf-gjahr.

        APPEND ls_bseg_inner3 TO lt_bseg_dat.
*        LOOP AT lt_blart_tmp.
*          CLEAR:lv_count_th,lv_count_fh.
*
*          IF lt_blart_tmp-hkon1 IS NOT INITIAL.
*            FREE:lr_hkont,lr_hkont[].
*            IF lt_blart_tmp-hkon1 CA '+*'.
*              lr_hkont = 'ICP'.
*            ELSE.
*              lr_hkont = 'IEQ'.
*            ENDIF.
*            lr_hkont-low = lt_blart_tmp-hkon1.
*            APPEND lr_hkont.CLEAR lr_hkont.
*
*            IF lt_bseg-hkont IN lr_hkont.
*              lt_blart_tmp-hflg1 = 'X'.
*              ADD 1 TO lv_count_fh.
*            ENDIF.
*            ADD 1 TO lv_count_th.
*          ENDIF.
*
*          IF lt_blart_tmp-hkon2 IS NOT INITIAL.
*            FREE:lr_hkont,lr_hkont[].
*            IF lt_blart_tmp-hkon2 CA '+*'.
*              lr_hkont = 'ICP'.
*            ELSE.
*              lr_hkont = 'IEQ'.
*            ENDIF.
*            lr_hkont-low = lt_blart_tmp-hkon2.
*            APPEND lr_hkont.CLEAR lr_hkont.
*
*            IF lt_bseg-hkont IN lr_hkont.
*              lt_blart_tmp-hflg2 = 'X'.
*              ADD 1 TO lv_count_fh.
*            ENDIF.
*            ADD 1 TO lv_count_th.
*          ENDIF.
*
*          IF lt_blart_tmp-hkon3 IS NOT INITIAL.
*            FREE:lr_hkont,lr_hkont[].
*            IF lt_blart_tmp-hkon3 CA '+*'.
*              lr_hkont = 'ICP'.
*            ELSE.
*              lr_hkont = 'IEQ'.
*            ENDIF.
*            lr_hkont-low = lt_blart_tmp-hkon3.
*            APPEND lr_hkont.CLEAR lr_hkont.
*
*            IF lt_bseg-hkont IN lr_hkont.
*              lt_blart_tmp-hflg3 = 'X'.
*              ADD 1 TO lv_count_fh.
*            ENDIF.
*            ADD 1 TO lv_count_th.
*          ENDIF.
*
*          IF lt_blart_tmp-hkon4 IS NOT INITIAL.
*            FREE:lr_hkont,lr_hkont[].
*            IF lt_blart_tmp-hkon4 CA '+*'.
*              lr_hkont = 'ICP'.
*            ELSE.
*              lr_hkont = 'IEQ'.
*            ENDIF.
*            lr_hkont-low = lt_blart_tmp-hkon4.
*            APPEND lr_hkont.CLEAR lr_hkont.
*
*            IF lt_bseg-hkont IN lr_hkont.
*              lt_blart_tmp-hflg4 = 'X'.
*              ADD 1 TO lv_count_fh.
*            ENDIF.
*            ADD 1 TO lv_count_th.
*          ENDIF.
*
*          IF lv_count_th EQ lv_count_fh.
*            lt_blart_tmp-chcok = 'X'.
*            MODIFY lt_blart_tmp.
*          ENDIF.
*        ENDLOOP.
      ENDLOOP.


      IF iv_f51_blart EQ ls_bkpf-blart AND
         iv_f51_tcode EQ ls_bkpf-tcode.
        LOOP AT lt_bseg INTO DATA(ls_bseg_inner4) WHERE bukrs = ls_bkpf-bukrs
                                                    AND belnr = ls_bkpf-belnr
                                                    AND gjahr = ls_bkpf-gjahr.
          IF ls_bseg_inner4-koart EQ 'K'.
            lv_satici = 'X'.
          ELSEIF ls_bseg_inner4-koart EQ 'S'.
            lv_anahesap = 'X'.
          ENDIF.
        ENDLOOP.
      ENDIF.


*      READ TABLE t_blart WITH KEY blart = t_bkpf-blart.
*
*      LOOP AT t_cash WHERE blart = t_bkpf-blart.
*        READ TABLE t_cash_data WITH KEY comp_code = t_bkpf-bukrs
*                                       cajo_number = t_bkpf-awkey+10(4)
*                                    posting_number = t_bkpf-awkey+0(10)
*                                    transact_number = t_cash-tisno.
*        IF sy-subrc EQ 0.
*          CLEAR:t_blart-gbtur,t_blart-blart_t,t_blart-oturu.
*          t_blart-gbtur   = t_cash-gbtur.
*          IF t_blart-gbtur = 'other'.
*            t_blart-blart_t = t_cash-blart_t.
*          ENDIF.
*          t_blart-oturu   = t_cash-oturu.
*          EXIT.
*        ENDIF.
*      ENDLOOP.


      READ TABLE t_blart ASSIGNING FIELD-SYMBOL(<fs_blart>) WITH KEY blart = ls_bkpf-blart.

      IF <fs_blart>-gbtur EQ 'check' OR <fs_blart>-gbtur EQ 'voucher'.
        CLEAR lv_count.
        LOOP AT lt_bsed INTO DATA(LS_BSED) WHERE bukrs EQ ls_bkpf-bukrs
                                              AND belnr EQ ls_bkpf-belnr
                                              AND gjahr EQ ls_bkpf-gjahr
                                              AND boeno NE space.
          ADD 1 TO lv_count.
        ENDLOOP.

        IF lv_count EQ 1.
          ls_bkpf-xblnr = LS_BSED-boeno.
        ELSEIF lv_count GT 1.
          IF <fs_blart>-gbtur EQ 'check'.
            <fs_blart>-blart_t = 'Çek Bordrosu'.
          ELSE.
            <fs_blart>-blart_t = 'Senet Bordrosu'.
          ENDIF.

          <fs_blart>-gbtur = 'other'.
        ELSE.
          CLEAR lv_count.
*          LOOP AT lt_payr WHERE zbukr = t_bkpf-bukrs
*                            AND vblnr = t_bkpf-belnr
*                            AND gjahr = t_bkpf-gjahr.
*            ADD 1 TO lv_count.
*          ENDLOOP.

          IF lv_count EQ 1.
*            t_bkpf-xblnr = lt_payr-chect.
          ELSEIF lv_count > 1.
*            t_bkpf-xblnr = lt_payr-vblnr.

            IF <fs_blart>-gbtur EQ 'check'.
              <fs_blart>-blart_t = 'Çek Bordrosu'.
            ELSE.
              <fs_blart>-blart_t = 'Senet Bordrosu'.
            ENDIF.

            <fs_blart>-gbtur = 'other'.
          ENDIF.
        ENDIF.
      ENDIF.

*      IF iv_f51_blart EQ t_bkpf-blart AND "İletilmedi @YiğitcanÖ.
*         iv_f51_tcode EQ t_bkpf-tcode.
*
*        IF lv_satici EQ 'X' AND lv_anahesap EQ 'X'.
*
*          IF t_blart-gbtur EQ 'check'.
*            t_blart-gbtur   = 'check'.
*            t_blart-blart_t = space.
*            t_blart-oturu   = 'Çek'.
*          ELSEIF t_blart-gbtur EQ 'voucher'.
*            t_blart-gbtur   = 'voucher'.
*            t_blart-blart_t = space.
*            t_blart-oturu   = 'Senet'.
*          ENDIF.
*
*        ENDIF.
*

*      ENDIF.
*
*      LOOP AT lt_blart_tmp WHERE chcok IS NOT INITIAL.
*        EXIT.
*      ENDLOOP.
*      IF sy-subrc EQ 0.
*        t_blart-blart_t = lt_blart_tmp-blart_t.
*        t_blart-gbtur   = lt_blart_tmp-gbtur.
*        t_blart-oturu   = lt_blart_tmp-oturu.
*      ENDIF.

*      CLEAR lv_skip.
*      lv_bktxt = t_bkpf-bktxt.
*      CALL FUNCTION '/ITETR/EDF_EXIT_001'
*        EXPORTING
*          is_bkpf   = t_bkpf
*        TABLES
*          t_bseg    = lt_bseg_dat
*        CHANGING
*          c_blart_t = t_blart-blart_t
*          c_gbtur   = t_blart-gbtur
*          c_oturu   = t_blart-oturu
*          c_xblnr   = t_bkpf-xblnr
*          c_bktxt   = lv_bktxt
*          c_skip    = lv_skip.
*      IF lv_skip IS NOT INITIAL.
*        CONTINUE.
*      ENDIF.

      LOOP AT lt_bseg INTO DATA(ls_bseg_inner5) WHERE bukrs = ls_bkpf-bukrs
                                                  AND belnr = ls_bkpf-belnr
                                                  AND gjahr = ls_bkpf-gjahr.

        IF iv_alternative IS INITIAL.
*          WRITE ls_bseg_inner5-hkont TO lv_hkont NO-ZERO.
*          CONDENSE lv_hkont.
          lv_hkont = ls_bseg_inner5-hkont.
          CONDENSE lv_hkont NO-GAPS.
        ELSE.
*          WRITE lt_bseg-lokkt TO lv_hkont NO-ZERO.
*          CONDENSE lv_hkont.
          lv_hkont = ls_bseg_inner5-lokkt.
          CONDENSE lv_hkont NO-GAPS.
          ls_bseg_inner5-hkont = ls_bseg_inner5-lokkt.
          MODIFY lt_bseg FROM ls_bseg_inner5.
        ENDIF.

        CLEAR lt_ledger.
        MOVE-CORRESPONDING ls_bkpf TO ls_ledger.
        MOVE-CORRESPONDING ls_bseg_inner5 TO ls_ledger.
        ls_ledger-bktxt   = lv_bktxt.
        ls_ledger-docln   = ls_bseg_inner5-posn2.
*        ls_ledger-bcode   = iv_bcode.
        ls_ledger-tsfyd   = iv_tasfiye.
*        ls_ledger-gbtur   = t_blart-gbtur.
*        IF lt_ledger-gbtur EQ 'other'.
*          lt_ledger-blart_t = t_blart-blart_t.
*        ENDIF.
*        lt_ledger-oturu   = t_blart-oturu.

        ls_ledger-hkont_3 = lv_hkont+0(3).

        IF is_params-natpb EQ 'DMBTR'.
          ls_ledger-dmbtr_def = abs( ls_bseg_inner5-dmbtr ).
        ELSEIF is_params-natpb EQ 'WRBTR'.
          ls_ledger-dmbtr_def = abs( ls_bseg_inner5-wrbtr ).
        ELSEIF is_params-natpb EQ 'DMBE2'.
          ls_ledger-dmbtr_def = abs( ls_bseg_inner5-dmbe2 ).
        ELSEIF is_params-natpb EQ 'DMBE3'.
          ls_ledger-dmbtr_def = abs( ls_bseg_inner5-dmbe3 ).
        ELSE.
          ls_ledger-dmbtr_def = abs( ls_bseg_inner5-dmbtr ).
        ENDIF.

        ls_ledger-shkzg_srt = ls_ledger-shkzg.
        IF ls_ledger-shkzg EQ 'S'.
          ls_ledger-shkzg_srt = 'B'.
        ENDIF.

        IF ls_bseg_inner5-kunnr IS NOT INITIAL OR ls_bseg_inner5-lifnr IS NOT INITIAL.
          IF ls_bseg_inner5-xcpdd IS NOT INITIAL.
            READ TABLE lt_bsec INTO DATA(ls_bsec) WITH KEY bukrs = ls_bseg_inner5-bukrs
                                                           belnr = ls_bseg_inner5-belnr
                                                           gjahr = ls_bseg_inner5-gjahr
                                                           buzei = ls_bseg_inner5-buzei.
            IF sy-subrc EQ 0.
              IF ls_bseg_inner5-kunnr IS NOT INITIAL.
                CONCATENATE ls_bsec-name1 ls_bsec-name2
                            ls_bsec-name3 ls_bsec-name4
                       INTO ls_ledger-kname SEPARATED BY space.
              ELSEIF ls_bseg_inner5-lifnr IS NOT INITIAL.
                CONCATENATE ls_bsec-name1 ls_bsec-name2
                            ls_bsec-name3 ls_bsec-name4
                       INTO ls_ledger-lname SEPARATED BY space.
              ENDIF.
            ENDIF.
          ELSE.
          ENDIF.
        ENDIF.

        IF ls_ledger-kunnr IS NOT INITIAL AND ls_ledger-kname IS INITIAL.
          READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = ls_ledger-kunnr.
          IF sy-subrc EQ 0.
            CONCATENATE ls_kna1-name1 ls_kna1-name2
                   INTO ls_ledger-kname SEPARATED BY space.
          ENDIF.
        ELSEIF ls_ledger-lifnr IS NOT INITIAL AND ls_ledger-lname IS INITIAL.
          READ TABLE lt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = ls_ledger-lifnr.
          IF sy-subrc EQ 0.
            CONCATENATE ls_lfa1-name1 ls_lfa1-name2
                   INTO ls_ledger-lname SEPARATED BY space.
          ENDIF.
        ENDIF.

        lv_45day = abs( ls_bkpf-budat - ls_bkpf-bldat ).
        IF lv_45day > 45 AND is_bukrs-days45 IS NOT INITIAL.
          ls_ledger-dif45 = 'X'.
        ENDIF.

        READ TABLE lt_colitm INTO ls_colitm WITH KEY bukrs = ls_bseg_inner5-bukrs
                                                     belnr = ls_bseg_inner5-belnr
                                                     gjahr = ls_bseg_inner5-gjahr
                                                     buzei = ls_bseg_inner5-buzei
                                                     docln = ls_bseg_inner5-docln.
        IF sy-subrc EQ 0.
          ls_ledger-clitm = 'X'.
          ls_ledger-cldoc = ls_colitm-cldoc.
        ENDIF.

        REPLACE ALL OCCURRENCES OF REGEX '[^(0-9)(a-z)(A-Z)( )]' IN ls_ledger-bktxt WITH ''.
        REPLACE ALL OCCURRENCES OF REGEX '[^(0-9)(a-z)(A-Z)( )]' IN ls_ledger-sgtxt WITH ''.
        REPLACE ALL OCCURRENCES OF REGEX '[^(0-9)(a-z)(A-Z)( )]' IN ls_ledger-kname WITH ''.
        REPLACE ALL OCCURRENCES OF REGEX '[^(0-9)(a-z)(A-Z)( )]' IN ls_ledger-lname WITH ''.



        IF is_params-colac IS NOT INITIAL.
          READ TABLE lt_ledger ASSIGNING FIELD-SYMBOL(<lfs_ledger>) WITH KEY belnr = ls_ledger-belnr
                                                                             hkont = ls_ledger-hkont
                                                                             shkzg = ls_ledger-shkzg. " AS 26.06.2023
          IF sy-subrc EQ 0.
            <lfs_ledger>-dmbtr_def = <lfs_ledger>-dmbtr_def + ls_ledger-dmbtr_def.
            CONTINUE.
          ENDIF.
        ENDIF.

        APPEND ls_ledger TO lt_ledger.CLEAR ls_ledger.
      ENDLOOP.
    ENDLOOP.

    DELETE lt_ledger WHERE dmbtr_def = 0.

    TRY.
        IF iv_ledger IS INITIAL.
*        INSERT /itetr/edf_defky FROM TABLE lt_ledger[].
          MODIFY zetr_t_defky FROM TABLE @lt_ledger.
          COMMIT WORK AND WAIT.
        ELSE.
*          APPEND LINES OF lt_ledger TO t_ledger.
        ENDIF.
      CATCH cx_root INTO lc_root.
    ENDTRY.

  ENDMETHOD.