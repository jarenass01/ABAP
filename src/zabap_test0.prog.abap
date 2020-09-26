*&---------------------------------------------------------------------*
*& Report ZABAP_TEST0
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_test0.


DATA: ls_rm03q  TYPE rm03q,
      lo_bdc    TYPE REF TO zcl_bdc_bi_gen,
      ls_header TYPE balhdri,
      lt_return TYPE bapiret2_t,
      lv_handle TYPE balloghndl,
      lv_obj    TYPE balobj_d.
*      lv_subobj        TYPE balsubobj.

* Programa para desplazar Periodos contables

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.

PARAMETERS: pa_buk TYPE bukrs OBLIGATORY.

SELECT-OPTIONS: so_npe  FOR ls_rm03q-nperi NO-EXTENSION , "Periodo
                so_lfg  FOR ls_rm03q-lfgja NO-EXTENSION . "Año

SELECTION-SCREEN END OF BLOCK b1.


AT SELECTION-SCREEN ON pa_buk.

  SELECT SINGLE *
    INTO @DATA(ls_marv)
    FROM marv
    WHERE bukrs EQ @pa_buk.

  so_npe = VALUE #(
    low     = ls_marv-lfmon + 1
    high    = 12
    option  = 'BT'
    sign    = 'I'
   ).

  so_lfg = VALUE #(
    low     = ls_marv-lfgja
    high    = sy-datum(4)
    option  = 'BT'
    sign    = 'I'
  ).

START-OF-SELECTION.

  DATA(lv_min_year) = so_lfg-low.
  DATA(lv_max_year) = so_lfg-high.

  DATA(lv_min_peri) = so_npe-low.
  DATA(lv_max_peri) = so_npe-high.

  lo_bdc = NEW zcl_bdc_bi_gen(
      dismode = 'A'
*      updmode = ''
*      defsize = ''
*      nobinpt = ''
  ).


  WHILE lv_min_year <= lv_max_year.

    IF lv_min_year = sy-datum(4).
      lv_max_peri = sy-datum+4(2).
    ENDIF.

    WHILE lv_min_peri <= lv_max_peri.


      CALL METHOD lo_bdc->init_dynpro
        EXPORTING:
          ip1 = 'RMMMPERI' ip2 = '1000' ip3 = 'X' ip4 = '' ip5 = '',
          ip1 = '' ip2 = '' ip3 = '' ip4 = 'BDC_CURSOR' ip5 = 'I_LFGJA',
          ip1 = '' ip2 = '' ip3 = '' ip4 = 'BDC_OKCODE' ip5 = '=ONLI',
          ip1 = '' ip2 = '' ip3 = '' ip4 = 'I_VBUKR' ip5 = pa_buk,
          ip1 = '' ip2 = '' ip3 = '' ip4 = 'I_BBUKR' ip5 = pa_buk,
          ip1 = '' ip2 = '' ip3 = '' ip4 = 'I_LFMON' ip5 = lv_min_peri,
          ip1 = '' ip2 = '' ip3 = '' ip4 = 'I_LFGJA' ip5 = lv_min_year,
          ip1 = '' ip2 = '' ip3 = '' ip4 = 'I_XCOMP' ip5 = 'X',
          ip1 = 'RMMMPERI' ip2 = '1000' ip3 = 'X' ip4 = '' ip5 = '',
          ip1 = '' ip2 = '' ip3 = '' ip4 = 'BDC_OKCODE' ip5 = '/EE',
          ip1 = '' ip2 = '' ip3 = '' ip4 = 'BDC_CURSOR' ip5 = 'I_VBUKR'
        .


      DATA(lt_result) =  lo_bdc->execute( 'mmpv' ).

      LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<fs_res>).

        APPEND INITIAL LINE TO lt_return
        ASSIGNING FIELD-SYMBOL(<fs_return>).

        <fs_return>-id          = <fs_res>-msgid.
        <fs_return>-number      = <fs_res>-msgnr.
        <fs_return>-type        = <fs_res>-msgtyp.

        <fs_return>-message_v1  = <fs_res>-msgv1.
        <fs_return>-message_v2  = <fs_res>-msgv2.
        <fs_return>-message_v3  = <fs_res>-msgv3.
        <fs_return>-message_v4  = <fs_res>-msgv4.

      ENDLOOP.


      ADD 1 TO lv_min_peri.
    ENDWHILE.
    lv_min_peri = 1.

    ADD 1 TO lv_min_year.
  ENDWHILE.


* *--------------------------------------------------------------------*
* LOGS
* *--------------------------------------------------------------------*

  lv_obj = 'ZDEV'.

* Header information for the log
  ls_header-object     = lv_obj.
  ls_header-subobject  = ||.
  ls_header-aldate     = sy-datum.
  ls_header-altime     = sy-uzeit.
  ls_header-aluser     = sy-uname.
  ls_header-aldate_del = sy-datum + 1.

* Get the Log handle using the header
  CALL FUNCTION 'APPL_LOG_WRITE_HEADER'
    EXPORTING
      header              = ls_header
    IMPORTING
      e_log_handle        = lv_handle
    EXCEPTIONS
      object_not_found    = 1
      subobject_not_found = 2
      error               = 3
      OTHERS              = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


* Write the Log mesages to the memory
  CALL FUNCTION 'APPL_LOG_WRITE_MESSAGES'
    EXPORTING
      object              = lv_obj
*     subobject           = 'VENDOR_CN'
      log_handle          = lv_handle
    TABLES
      messages            = lt_return
    EXCEPTIONS
      object_not_found    = 1
      subobject_not_found = 2
      OTHERS              = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'.
