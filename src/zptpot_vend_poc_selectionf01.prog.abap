*----------------------------------------------------------------------*
***INCLUDE ZPTPOT_VEND_EXTR_SELECTIONF01.
*----------------------------------------------------------------------*
* CHANGE HISTORY                                                       *
*  DATE      CHANGE BY  TRANSPORT      PROJECT/Change Details          *
* 27.04.2022 PREMKPRA   GD2K9J0IQ2     CHG0159211 Initial Developemnt  *
*                       GD2K9J0ITP,GD2K9J0J0S,GD2K9J0J96               *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection .

  DATA      : lx_prog      TYPE zvt_progruns,
              lx_vendot    TYPE zpts_vendor_ot,
              li_vendot    TYPE TABLE OF zpts_vendor_ot,
              lx_status    TYPE  zpts_vendor_ret,
              lx_vendor_ot TYPE  zptp_vendor_ot,
              li_vendor_ot TYPE TABLE OF zptp_vendor_ot,
              lr_lifnr     TYPE RANGE OF lifnr.
  CONSTANTS : lc_prog  TYPE repid VALUE 'ZPTPOT_VEND_EXTR',
              lc_kred  TYPE cdobjectcl VALUE 'KRED',
              lc_u     TYPE char01 VALUE 'U',
              lc_i     TYPE char01 VALUE 'I',
              lc_lfa1  TYPE tabname VALUE 'LFA1',
              lc_lfb1  TYPE tabname VALUE 'LFB1',
              lc_name1 TYPE fieldname VALUE 'NAME1',
              lc_kraus TYPE fieldname VALUE 'KRAUS',
              lc_stras TYPE fieldname VALUE 'STRAS',
              lc_ort01 TYPE fieldname VALUE 'ORT01',
              lc_regio TYPE fieldname VALUE 'REGIO',
              lc_pstlz TYPE fieldname VALUE 'PSTLZ',
              lc_land1 TYPE fieldname VALUE 'LAND1',
              lc_loevm TYPE fieldname VALUE 'LOEVM',
              lc_key   TYPE fieldname VALUE 'KEY',
              lc_ms    TYPE zcon_key1 VALUE 'MS',
              lc_dash  TYPE char01 VALUE '-',
              lc_e     TYPE char01 VALUE 'E',
              lc_r     TYPE char01 VALUE 'R',
              lc_space TYPE char01 VALUE ' ',
              lc_bapi  TYPE zcon_key2 VALUE 'BAPI'.
*              lc_rfcdest TYPE char30 VALUE 'RFC_MULESOFT_BAPI_GT2'.
*----------------------------------------------------------------------*
  "Fetch rfc destination from t9con
  SELECT SINGLE descript FROM t9con
    INTO @DATA(lv_rfcdest)
    WHERE key1 = @lc_ms
     AND  key2 = @lc_bapi
     AND  key3 = @sy-sysid.
  "Check the ZVT_PROGRUNS,to get the last execution of Program
  "And update with current execution date & time of program .
  SELECT SINGLE * FROM zvt_progruns
    INTO lx_prog
    WHERE repid = lc_prog.
  IF sy-subrc EQ 0.
    DATA(lv_dat) = lx_prog-datum.
    DATA(lv_tim) = lx_prog-uzeit.
    lx_prog-datum = sy-datum.
    lx_prog-uzeit = sy-uzeit.
    MODIFY zvt_progruns FROM lx_prog.
  ENDIF.


  "Fetch changes header from CDHDR for vendor.
  SELECT objectid , changenr
    FROM cdhdr
    INTO TABLE @DATA(li_cdhdr)
    WHERE objectclas = @lc_kred
    AND objectid IN @s_lifnr
    AND change_ind  IN ( @lc_u , @lc_i )
    AND udate GE @lv_dat
    AND utime GE @lv_tim.
  IF sy-subrc EQ 0."cdhdr

    "Fetch CDPOS table by passing the CHANGENR
    SELECT objectid ,chngind, changenr , tabname , tabkey ,fname
      FROM cdpos
      INTO TABLE @DATA(li_cdpos)
      FOR ALL ENTRIES IN @li_cdhdr
      WHERE objectid = @li_cdhdr-objectid
      AND tabname IN ( @lc_lfa1 , @lc_lfb1 )
      AND fname IN  ( @lc_name1, @lc_kraus, @lc_stras, @lc_ort01,
                      @lc_regio, @lc_pstlz, @lc_land1, @lc_loevm
                      , @lc_key )
      AND changenr = @li_cdhdr-changenr.

    IF sy-subrc EQ 0.
      LOOP AT li_cdpos ASSIGNING FIELD-SYMBOL(<lfs_cd>) WHERE tabname = lc_lfb1.
        <lfs_cd>-chngind = lc_e.
      ENDLOOP.

      "Sort for using AT End
      SORT li_cdpos BY objectid chngind .

      "Fetch vendor master data
      lr_lifnr = VALUE #( FOR lx_cdp IN li_cdpos ( sign = 'I'
                         option = 'EQ' low = lx_cdp-objectid ) ).
      DELETE ADJACENT DUPLICATES FROM lr_lifnr.
      SELECT lifnr , name1, kraus, stras, ort01,
             regio, pstlz, land1, loevm,gbort, stcd5, pson1
        FROM lfa1
        INTO TABLE @DATA(li_lfa1)
       WHERE lifnr IN @lr_lifnr
         AND ktokk IN @s_ktokk.
      IF sy-subrc EQ 0.
        SORT li_lfa1 BY lifnr.
        DELETE li_lfa1 WHERE gbort IS INITIAL
                         AND stcd5 IS INITIAL.
        IF li_lfa1 IS NOT INITIAL.

          "Fetch company codes for the vendors.
          SELECT lifnr ,bukrs
            FROM lfb1
            INTO TABLE @DATA(li_lfb1)
            FOR ALL ENTRIES IN @li_lfa1
            WHERE lifnr = @li_lfa1-lifnr.
          IF sy-subrc IS INITIAL.

            "Fetch Company code description
            SELECT bukrs , butxt
              FROM t001
              INTO TABLE @DATA(li_t001)
              FOR ALL ENTRIES IN @li_lfb1
              WHERE bukrs = @li_lfb1-bukrs.
            IF sy-subrc IS INITIAL.
              SORT li_t001 BY bukrs.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    "No changed entry
    MESSAGE TEXT-002 TYPE 'E'.
  ENDIF."cdhdr
  LOOP AT li_cdpos INTO DATA(lx_cdpos).

    "Check change indicator I
    DATA(lx_lfa1) = VALUE #( li_lfa1[ lifnr = lx_cdpos-objectid ]
                    OPTIONAL ).
    IF lx_lfa1 IS NOT INITIAL"lfa1
    AND lx_lfa1-gbort IS NOT INITIAL.
*    AND lx_lfa1-stcd5 IS NOT INITIAL."GD2K9J0J96
      "SCENE I- Updation
      IF lx_cdpos-chngind = lc_u. "chngind

        "Pass values to lx_vendot
        lx_vendot-gbort = condense( lx_lfa1-gbort ).        "GD2K9J0IQ2
        lx_vendot-stcd5 = condense( lx_lfa1-stcd5 ).        "GD2K9J0IQ2
        TRANSLATE lx_vendot-stcd5  TO LOWER CASE."GD2K9J0J0S
        lx_vendot-change_ind = lx_cdpos-chngind.
        CASE lx_cdpos-fname.
          WHEN lc_name1.
            lx_vendot-name1 = lx_lfa1-name1.
          WHEN lc_kraus.
            lx_vendot-kraus = lx_lfa1-kraus.
          WHEN lc_stras.
            lx_vendot-stras = lx_lfa1-stras.
          WHEN lc_ort01.
            lx_vendot-ort01 = lx_lfa1-ort01.
          WHEN lc_regio.
            lx_vendot-regio = lx_lfa1-regio.
          WHEN lc_pstlz.
            lx_vendot-pstlz = lx_lfa1-pstlz.
          WHEN lc_land1.
            lx_vendot-land1 = lx_lfa1-land1.
          WHEN lc_loevm.
            lx_vendot-loevm = lx_lfa1-loevm.
            "BOC by PREMKPRA<GD2K9J0ITP>
            IF lx_vendot-loevm IS INITIAL.
              lx_vendot-loevm = lc_r.
            ENDIF.
            "EOC By PREMKPRA<GD2K9J0ITP>
          WHEN OTHERS.
        ENDCASE.

        AT END OF chngind.
          APPEND lx_vendot TO li_vendot.
          IF lx_vendot-gbort IS NOT INITIAL."gbort
*          AND lx_vendot-stcd5 IS NOT INITIAL."stcd5"GD2K9J0J96

            "RFC call to pass data to mulesoft
            DELETE ADJACENT DUPLICATES FROM li_vendot
                                 COMPARING ALL FIELDS.

            CALL FUNCTION 'ZPTPRFC_OUT_OT' DESTINATION lv_rfcdest
              TABLES
                imt_vendor = li_vendot.

            IF sy-subrc EQ 0. "rfc out

              "Updating table zptp_vendor_ot except status and message
              "Status and message gets updated in RFC ZPTPRFC_IN_OT
              "Post confirmation from one trust.
              li_vendor_ot = VALUE #( FOR lx_ot IN li_vendot ( mandt = sy-mandt
                                      lifnr = lx_cdpos-objectid
                                       pson1 = lx_lfa1-pson1
                                       gbort = lx_ot-gbort
                                       stcd5 = lx_ot-stcd5
                                       change_ind = lc_u
                                       aedat = sy-datum
                                       uzeit = sy-uzeit ) ).
              MODIFY zptp_vendor_ot FROM TABLE li_vendor_ot.
              CLEAR : li_vendot,lx_vendot.

            ENDIF.       "rfc out

          ENDIF."gbort
        ENDAT.
        "SCENE II - Insertion
      ELSEIF lx_cdpos-chngind = lc_i."chngind
        lx_vendot-gbort = condense( lx_lfa1-gbort ).        "GD2K9J0IQ2
        lx_vendot-stcd5 = condense( lx_lfa1-stcd5 ).        "GD2K9J0IQ2
        TRANSLATE lx_vendot-stcd5  to LOWER CASE."GD2K9J0J0S
        lx_vendot-change_ind = lx_cdpos-chngind.
        lx_vendot-lifnr = lx_cdpos-objectid.
        APPEND lx_vendot TO li_vendot.


        AT END OF chngind.
          IF lx_vendot-gbort IS NOT INITIAL."gbort
*          AND lx_vendot-stcd5 IS NOT INITIAL."stcd5"GD2K9J0J96

            CALL FUNCTION 'ZPTPRFC_OUT_OT' DESTINATION lv_rfcdest
              TABLES
                imt_vendor = li_vendot.

            IF sy-subrc EQ 0. "rfc out
              "Update table entry
              li_vendor_ot = VALUE #( FOR lx_ot IN li_vendot ( mandt = sy-mandt
                                      lifnr = lx_ot-lifnr
                                      pson1 = lx_lfa1-pson1
                                      gbort = lx_ot-gbort
                                      stcd5 = lx_ot-stcd5
                                      change_ind = lc_i
                                       aedat = sy-datum
                                       uzeit = sy-uzeit ) ).
              MODIFY zptp_vendor_ot FROM TABLE li_vendor_ot.
              CLEAR : li_vendot,lx_vendot.
            ENDIF.       "rfc out

          ENDIF."gbort
        ENDAT.
        "Scene III -Extension
      ELSEIF lx_cdpos-chngind = lc_e."chngind

        lx_vendot-gbort = condense( lx_lfa1-gbort ).        "GD2K9J0IQ2
        lx_vendot-stcd5 = condense( lx_lfa1-stcd5 ).        "GD2K9J0IQ2
        TRANSLATE lx_vendot-stcd5  to LOWER CASE."GD2K9J0J0S
        lx_vendot-change_ind = lx_cdpos-chngind.
        lx_vendot-lifnr = lx_cdpos-objectid.
        AT END OF chngind.
          LOOP AT li_lfb1 INTO DATA(lx_lfb1) WHERE lifnr = lx_vendot-lifnr.
            READ TABLE li_t001 INTO DATA(lx_t001) WITH KEY bukrs = lx_lfb1-bukrs.
            IF sy-subrc IS INITIAL.
              DATA(lv_butxt) = lx_t001-butxt.
              REPLACE ALL OCCURRENCES OF ',' IN lv_butxt WITH ' '.
              CONDENSE lv_butxt.
            ENDIF.
            lx_vendot-bukrs = |{ lx_lfb1-bukrs }| & | | & |{ lc_dash }| &
                                  | | & |{ lv_butxt }|.
            APPEND lx_vendot TO li_vendot.
          ENDLOOP.
          IF lx_vendot-gbort IS NOT INITIAL."gbort
*          AND lx_vendot-stcd5 IS NOT INITIAL."stcd5"GD2K9J0J96

            CALL FUNCTION 'ZPTPRFC_OUT_OT' DESTINATION lv_rfcdest
              TABLES
                imt_vendor = li_vendot.
            IF sy-subrc EQ 0. "rfc out
              "Update table entry
              li_vendor_ot = VALUE #( FOR lx_ot IN li_vendot ( mandt = sy-mandt
                                      lifnr = lx_ot-lifnr
                                      pson1 = lx_lfa1-pson1
                                      gbort = lx_ot-gbort
                                      stcd5 = lx_ot-stcd5
                                      change_ind = lc_e
                                       aedat = sy-datum
                                       uzeit = sy-uzeit ) ).
              MODIFY zptp_vendor_ot FROM TABLE li_vendor_ot.
              CLEAR : li_vendot,lx_vendot.
            ENDIF.       "rfc out

          ENDIF."gbort
        ENDAT.
      ENDIF."chgind
*    ELSE."--GD2K9J0J96
*      MESSAGE TEXT-001 TYPE 'E'. "--GD2K9J0J96
    ENDIF."lfa1
  ENDLOOP.


ENDFORM.
