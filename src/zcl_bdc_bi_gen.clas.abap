class ZCL_BDC_BI_GEN definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF gty_tran_opts ,
             dismode  TYPE ctu_params-dismode  , "Modo de procesamiento para CALL TRANSACTION USING...
             updmode  TYPE ctu_params-updmode  , "L LOCAL, S  Sincrónica, A Asincrónico
             cattmode TYPE ctu_params-cattmode ,          "Modo CATT para CALL TRANSACTION USING...
*            	Sin CATT
*              N  CATT sin control imágenes indiv.
*              A  CATT con control imágenes indiv.
             defsize  TYPE ctu_params-defsize,
             racommit TYPE ctu_params-racommit, "COMMIT WORK no es un fin en CALL TRANSACTION USING...
             nobinpt  TYPE ctu_params-nobinpt,
             nobiend  TYPE ctu_params-nobiend,
           END OF gty_tran_opts .

  methods CONSTRUCTOR
    importing
      !DISMODE type CTU_MODE default 'N'
      !UPDMODE type CTU_UPDATE default 'A'
      !DEFSIZE type CTU_DEFSZE default 'X'
      !NOBINPT type CTU_NOBIM default 'X' .
  methods INIT_DYNPRO
    importing
      value(IP1) type BDC_PROG
      value(IP2) type CHAR4
      value(IP3) type BDC_START
      value(IP4) type FNAM_____4
      value(IP5) type ANY .
  methods EXECUTE
    importing
      !IV_TCODE type TCODE
    returning
      value(ET_RETURN) type WDKMSG_TTY .
protected section.
private section.

  data BDC_DATA type CK_T_BDCDATA .
  data TRAN_OPTS type GTY_TRAN_OPTS .
  data RETURN type WDKMSG_TTY .
ENDCLASS.



CLASS ZCL_BDC_BI_GEN IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->tran_opts-dismode = dismode.
    me->tran_opts-updmode = updmode.
    me->tran_opts-defsize = defsize.
    me->tran_opts-nobinpt = nobinpt.

    REFRESH: me->bdc_data[].

  ENDMETHOD.


  METHOD execute.

    CHECK NOT iv_tcode IS INITIAL.

    DATA(lv_tcode) = iv_tcode.

    TRANSLATE lv_tcode TO UPPER CASE.

    CALL TRANSACTION lv_tcode
             USING me->bdc_data
      OPTIONS FROM me->tran_opts
     MESSAGES INTO me->return.

    REFRESH et_return.

    et_return = me->return.


  ENDMETHOD.


  METHOD init_dynpro.

    DATA: ls_bdc TYPE bdcdata.
    CLEAR ls_bdc.

    ls_bdc-program   = ip1.
    ls_bdc-dynpro    = CONV #( ip2 ).
    ls_bdc-dynbegin  = ip3.
    ls_bdc-fnam      = ip4.
    ls_bdc-fval      = CONV #( ip5 ).

    CONDENSE:
           ls_bdc-program   NO-GAPS,
           ls_bdc-dynpro    NO-GAPS,
           ls_bdc-dynbegin  NO-GAPS,
           ls_bdc-fnam      NO-GAPS.


    APPEND ls_bdc TO me->bdc_data.

  ENDMETHOD.
ENDCLASS.
