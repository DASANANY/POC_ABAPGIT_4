*&---------------------------------------------------------------------*
*&  Include           ZPTPOT_VEND_EXT_SEL
*&---------------------------------------------------------------------*
*=======================================================================
* Selection Screen
*=======================================================================
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-s01.

SELECT-OPTIONS:
 s_lifnr  FOR v_lifnr ,
 s_ktokk  FOR v_ktokk OBLIGATORY,
 s_bukrs  FOR v_bukrs .
SELECTION-SCREEN END OF BLOCK a.
*=======================================================================
* INITIALIZATION
*=======================================================================
INITIALIZATION.
  APPEND VALUE #( sign = c_i option = c_eq low = c_norm ) TO s_ktokk.
  APPEND VALUE #( sign = c_i option = c_eq low = c_nis ) TO s_ktokk.
  APPEND VALUE #( sign = c_i option = c_eq low = c_fact ) TO s_ktokk.
