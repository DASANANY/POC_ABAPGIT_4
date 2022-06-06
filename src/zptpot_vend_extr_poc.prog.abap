*&---------------------------------------------------------------------*
*& Report ZPTPOT_VEND_EXTR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
* TITLE       : CHG0159211_SAP to One Trust_Vendor Master_Interface    *
* AUTHOR      : Praveena Prem                DATE: 15 Mar 2022         *
* FD Designer : Ramamoorthy J                                          *
* DESCRIPTION : This program is used to send the vendor master changes *
*               to One trust Via mulesoft and gets the status via RFC  *
*----------------------------------------------------------------------*
* CHANGE HISTORY                                                       *
*  DATE      CHANGE BY  TRANSPORT      PROJECT/Change Details          *
* 15.03.2022 PREMKPRA   GD2K9J0HPR     CHG0159211 Initial Developemnt  *
*----------------------------------------------------------------------*
REPORT zptpot_vend_extr_poc.
INCLUDE ZPTPOT_VEND_EXT_POC_TOP.
*INCLUDE zptpot_vend_ext_top.
INCLUDE ZPTPOT_VEND_EXT_POC_SEL.
*INCLUDE zptpot_vend_ext_sel.
INCLUDE ZPTPOT_VEND_POC_SELECTIONF01.
*INCLUDE zptpot_vend_extr_selectionf01.

*=======================================================================
* START OF SELECTION
*=======================================================================

START-OF-SELECTION.
  PERFORM selection.
