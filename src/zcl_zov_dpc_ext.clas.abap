class ZCL_ZOV_DPC_EXT definition
  public
  inheriting from ZCL_ZOV_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.

  methods CHECK_SUBSCRIPTION_AUTHORITY
    redefinition .
  methods OV_HEADERSET_CREATE_ENTITY
    redefinition .
  methods OV_HEADERSET_DELETE_ENTITY
    redefinition .
  methods OV_HEADERSET_GET_ENTITY
    redefinition .
  methods OV_HEADERSET_GET_ENTITYSET
    redefinition .
  methods OV_HEADERSET_UPDATE_ENTITY
    redefinition .
  methods OV_ITEMSET_CREATE_ENTITY
    redefinition .
  methods OV_ITEMSET_DELETE_ENTITY
    redefinition .
  methods OV_ITEMSET_GET_ENTITY
    redefinition .
  methods OV_ITEMSET_GET_ENTITYSET
    redefinition .
  methods OV_ITEMSET_UPDATE_ENTITY
    redefinition .
  methods OV_MESSAGESET_CREATE_ENTITY
    redefinition .
  methods OV_MESSAGESET_DELETE_ENTITY
    redefinition .
  methods OV_MESSAGESET_GET_ENTITY
    redefinition .
  methods OV_MESSAGESET_GET_ENTITYSET
    redefinition .
  methods OV_MESSAGESET_UPDATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZOV_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
*  EXPORTING
**    iv_entity_name          =
**    iv_entity_set_name      =
**    iv_source_name          =
*    IO_DATA_PROVIDER        =
**    it_key_tab              =
**    it_navigation_path      =
*    IO_EXPAND               =
**    io_tech_request_context =
**  IMPORTING
**    er_deep_entity          =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA : ls_deep_entity  TYPE zcl_zov_mpc_ext=>ty_ordem_item.
    DATA : ls_deep_item    TYPE zcl_zov_mpc_ext=>ts_ov_item.

    DATA : ls_cab          TYPE ztovheader.
    DATA : lt_item         TYPE STANDARD TABLE OF ztovitem.
    DATA : ls_item         TYPE ztovitem.
    DATA : ld_updkz        TYPE char1.
    DATA : ld_datahora(14) TYPE c.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    CALL METHOD io_data_provider->read_entry_data
      IMPORTING
        es_data = ls_deep_entity.

    " cabeçalho
    IF ls_deep_entity-ordemid = 0.
      ld_updkz = 'I'.

      MOVE-CORRESPONDING ls_deep_entity TO ls_cab.

      "ls_cab-criacao_data    = sy-datum.
      "ls_cab-criacao_hora    = sy-uzeit.
      "ls_cab-criacao_usuario = sy-uname.

      ld_datahora         = ls_deep_entity-datacriacao.
      ls_cab-dtcriacao    = ld_datahora(8).
      ls_cab-hrcriacao    = ld_datahora+8(6).

      SELECT SINGLE MAX( ordemid )
        INTO ls_cab-ordemid
        FROM ztovheader.

      ls_cab-ordemid = ls_cab-ordemid + 1.
    ELSE.
      ld_updkz = 'U'.

      " carregando dados atuais
      SELECT SINGLE *
        INTO ls_cab
        FROM ztovheader
       WHERE ordemid = ls_deep_entity-ordemid.

      ls_cab-clienteid  = ls_deep_entity-clienteid.
      ls_cab-status     = ls_deep_entity-status.
      ls_cab-totalitens = ls_deep_entity-totalitens.
      ls_cab-totalfrete = ls_deep_entity-totalfrete.
      ls_cab-totalordem = ls_cab-totalitens + ls_cab-totalfrete.
    ENDIF.

    " item
    LOOP AT ls_deep_entity-toitem INTO ls_deep_item.
      MOVE-CORRESPONDING ls_deep_item TO ls_item.

      ls_item-ordemid = ls_cab-ordemid.
      APPEND ls_item TO lt_item.
    ENDLOOP.

    " persistência cabeçalho
    IF ld_updkz = 'I'.
      INSERT ztovheader FROM ls_cab.
      IF sy-subrc <> 0.
        ROLLBACK WORK.

        lo_msg->add_message_text_only(
          EXPORTING
            iv_msg_type = 'E'
            iv_msg_text = 'Erro ao inserir ordem'
        ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_msg.
      ENDIF.
    ELSE.
      MODIFY ztovheader FROM ls_cab.
      IF sy-subrc <> 0.
        ROLLBACK WORK.

        lo_msg->add_message_text_only(
          EXPORTING
            iv_msg_type = 'E'
            iv_msg_text = 'Erro ao atualizar ordem'
        ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_msg.
      ENDIF.
    ENDIF.

    " persistência itens
    DELETE FROM ztovitem WHERE ordemid = ls_cab-ordemid.
    IF lines( lt_item ) > 0.
      INSERT ztovitem FROM TABLE lt_item.
      IF sy-subrc <> 0.
        ROLLBACK WORK.

        lo_msg->add_message_text_only(
          EXPORTING
            iv_msg_type = 'E'
            iv_msg_text = 'Erro ao inserir itens'
        ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_msg.
      ENDIF.
    ENDIF.

    COMMIT WORK AND WAIT.

    " atualizando deep entity de retorno

    " cabeçalho
    ls_deep_entity-ordemid = ls_cab-ordemid.
    CONVERT DATE ls_cab-dtcriacao
            TIME ls_cab-hrcriacao
            INTO TIME STAMP ls_deep_entity-datacriacao
            TIME ZONE 'UTC'. "sy-zonlo.

    " item
    LOOP AT ls_deep_entity-toitem ASSIGNING FIELD-SYMBOL(<ls_deep_item>).
      <ls_deep_item>-ordemid = ls_cab-ordemid.
    ENDLOOP.

    CALL METHOD me->copy_data_to_ref
      EXPORTING
        is_data = ls_deep_entity
      CHANGING
        cr_data = er_deep_entity.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
**  EXPORTING
**    iv_action_name          =
**    it_parameter            =
**    io_tech_request_context =
**  IMPORTING
**    er_data                 =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA: ld_ordemid  TYPE ztovheader-ordemid.
    DATA: ld_status   TYPE ztovheader-status.
    DATA: lt_bapiret2 TYPE STANDARD TABLE OF zcl_zov_mpc_ext=>zct_message.
    DATA: ls_bapiret2 TYPE zcl_zov_mpc_ext=>zct_message.

    IF iv_action_name = 'ZFI_UPDATE_STATUS'.
      ld_ordemid = it_parameter[ name = 'OrdemID' ]-value.
      ld_status  = it_parameter[ name = 'Status' ]-value.

      UPDATE ztovheader
         SET status = ld_status
       WHERE ordemid = ld_ordemid.

      IF sy-subrc = 0.
        CLEAR ls_bapiret2.
        ls_bapiret2-tipo     = 'S'.
        ls_bapiret2-mensagem = |Status da ordem { ld_ordemid } atualizado|.
        APPEND ls_bapiret2 TO lt_bapiret2.
      ELSE.
        CLEAR ls_bapiret2.
        ls_bapiret2-tipo     = 'E'.
        ls_bapiret2-mensagem = |Erro ao atualizar status da ordem { ld_ordemid }|.
        APPEND ls_bapiret2 TO lt_bapiret2.
      ENDIF.
    ENDIF.

    CALL METHOD me->copy_data_to_ref
      EXPORTING
        is_data = lt_bapiret2
      CHANGING
        cr_data = er_data.
  ENDMETHOD.


  method CHECK_SUBSCRIPTION_AUTHORITY.
  endmethod.


  METHOD ov_headerset_create_entity.

    DATA: ls_cab    TYPE ztovheader.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

    MOVE-CORRESPONDING er_entity TO ls_cab.

    ls_cab-dtcriacao = sy-datum.
    ls_cab-hrcriacao = sy-uzeit.
    ls_cab-criadopor   = sy-uname.

    SELECT SINGLE MAX( ordemid )
      INTO @DATA(lv_lastid)
      FROM ztovheader.

    ls_cab-ordemid = lv_lastid + 1.
    INSERT ztovheader FROM ls_cab.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao inserir ordem'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    " atualizando
    MOVE-CORRESPONDING ls_cab TO er_entity.

    CONVERT
      DATE ls_cab-dtcriacao
      TIME ls_cab-hrcriacao
      INTO TIME STAMP er_entity-datacriacao
      TIME ZONE 'UTC'. "sy-zonlo.

  ENDMETHOD.


  METHOD ov_headerset_delete_entity.
    DATA: ls_key_tab LIKE LINE OF it_key_tab.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemID'.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'OrdemId não informado'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    DELETE FROM ztovitem WHERE ordemid = ls_key_tab-value.

    DELETE FROM ztovheader WHERE ordemid = ls_key_tab-value.
    IF sy-subrc <> 0.
      ROLLBACK WORK.

      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao remover ordem'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD ov_headerset_get_entity.
    DATA: ld_ordemid TYPE ztovheader-ordemid.
    DATA: ls_key_tab LIKE LINE OF it_key_tab.
    DATA: ls_cab     TYPE ztovheader.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " input
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemID'.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Id da ordem não informado'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
    ld_ordemid = ls_key_tab-value.

    SELECT SINGLE *
      INTO ls_cab
      FROM ztovheader
     WHERE ordemid = ld_ordemid.

    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_cab TO er_entity.

      CONVERT DATE ls_cab-dtcriacao
              TIME ls_cab-hrcriacao
         INTO TIME STAMP er_entity-datacriacao
         TIME ZONE 'UTC'. "sy-zonlo.
    ELSE.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Id da ordem não encontrado'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  ENDMETHOD.


  METHOD ov_headerset_get_entityset.
    DATA: lt_cab       TYPE STANDARD TABLE OF ztovheader.
    DATA: ls_cab       TYPE ztovheader.
    DATA: ls_entityset LIKE LINE OF et_entityset.

    DATA: lt_orderby   TYPE STANDARD TABLE OF string.
    DATA: ld_orderby   TYPE string.

    " montando orderby dinâmico
    LOOP AT it_order INTO DATA(ls_order).
      TRANSLATE ls_order-property TO UPPER CASE.
      TRANSLATE ls_order-order TO UPPER CASE.
      IF ls_order-order = 'DESC'.
        ls_order-order = 'DESCENDING'.
      ELSE.
        ls_order-order = 'ASCENDING'.
      ENDIF.
      APPEND |{ ls_order-property } { ls_order-order }|
          TO lt_orderby.
    ENDLOOP.
    CONCATENATE LINES OF lt_orderby INTO ld_orderby SEPARATED BY ', '. " Ajuste 05/06/2024

    " ordenação obrigatória caso nenhuma seja definida
    IF ld_orderby = '' .
      ld_orderby = 'OrdemId ASCENDING'.
    ENDIF.

    SELECT *
      FROM ztovheader
     WHERE (iv_filter_string) "it_filter_select_options
  ORDER BY (ld_orderby)
      INTO TABLE @lt_cab
     UP TO @is_paging-top ROWS
    OFFSET @is_paging-skip.

    LOOP AT lt_cab INTO ls_cab.
      CLEAR ls_entityset.
      MOVE-CORRESPONDING ls_cab TO ls_entityset.

      CONVERT DATE ls_cab-dtcriacao
              TIME ls_cab-hrcriacao
         INTO TIME STAMP ls_entityset-datacriacao
         TIME ZONE 'UTC'. "sy-zonlo.

      APPEND ls_entityset TO et_entityset.
    ENDLOOP.
  ENDMETHOD.


  METHOD ov_headerset_update_entity.
    DATA: ld_error TYPE flag.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

    er_entity-ordemid = it_key_tab[ name = 'OrdemID' ]-value.

    " validações
    IF er_entity-clienteid = 0.
      ld_error = 'X'.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Cliente vazio'
      ).
    ENDIF.

    IF er_entity-totalordem < 10.
      ld_error = 'X'.
      lo_msg->add_message(
        EXPORTING
          iv_msg_type   = 'E'
          iv_msg_id     = 'ZOV'
          iv_msg_number = 1
          iv_msg_v1     = 'R$ 10,00'
          iv_msg_v2     = |{ er_entity-ordemid }|
      ).
    ENDIF.

    IF ld_error = 'X'.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg
          http_status_code  = 500.
    ENDIF.

    UPDATE ztovheader
       SET clienteid  = er_entity-clienteid
           totalitens = er_entity-totalitens
           totalfrete = er_entity-totalfrete
           totalordem = er_entity-totalordem
           status     = er_entity-status
     WHERE ordemid    = er_entity-ordemid.

    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao atualizar ordem'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  ENDMETHOD.


  METHOD ov_itemset_create_entity.
    DATA: ls_item TYPE ztovitem.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

    MOVE-CORRESPONDING er_entity TO ls_item.

    IF er_entity-itemid = 0.
      SELECT SINGLE MAX( itemid )
        INTO er_entity-itemid
        FROM ztovitem
       WHERE ordemid = er_entity-ordemid.

      er_entity-itemid = er_entity-itemid + 1.
    ENDIF.

    INSERT ztovitem FROM ls_item.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao inserir item'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  ENDMETHOD.


  METHOD ov_itemset_delete_entity.
    DATA: ls_item    TYPE ztovitem.
    DATA: ls_key_tab LIKE LINE OF it_key_tab.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    ls_item-ordemid = it_key_tab[ name = 'OrdemID' ]-value.
    ls_item-itemid  = it_key_tab[ name = 'ItemID' ]-value.

    DELETE FROM ztovitem
     WHERE ordemid = ls_item-ordemid
       AND itemid  = ls_item-itemid.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao remover item'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  ENDMETHOD.


  METHOD ov_itemset_get_entity.
    DATA: ls_key_tab LIKE LINE OF it_key_tab.
    DATA: ls_item    TYPE ztovitem.
    DATA: ld_error   TYPE flag.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " input
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemID'.
    IF sy-subrc <> 0.
      ld_error = 'X'.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Id da ordem não informado'
      ).
    ENDIF.
    ls_item-ordemid = ls_key_tab-value.

    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'ItemID'.
    IF sy-subrc <> 0.
      ld_error = 'X'.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Id do item não informado'
      ).
    ENDIF.
    ls_item-itemid = ls_key_tab-value.

    IF ld_error = 'X'.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    SELECT SINGLE *
      INTO ls_item
      FROM ztovitem
     WHERE ordemid = ls_item-ordemid
       AND itemid  = ls_item-itemid.

    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_item TO er_entity.
    ELSE.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Item não encontrado'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  ENDMETHOD.


  METHOD ov_itemset_get_entityset.
    DATA: ld_ordemid       TYPE int4.
    DATA: lt_ordemid_range TYPE RANGE OF int4.
    DATA: ls_ordemid_range LIKE LINE OF lt_ordemid_range.
    DATA: ls_key_tab       LIKE LINE OF it_key_tab.

    " input
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrdemID'.
    IF sy-subrc = 0.
      ld_ordemid = ls_key_tab-value.

      CLEAR ls_ordemid_range.
      ls_ordemid_range-sign   = 'I'.
      ls_ordemid_range-option = 'EQ'.
      ls_ordemid_range-low    = ld_ordemid.
      APPEND ls_ordemid_range TO lt_ordemid_range.
    ENDIF.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE et_entityset
      FROM ztovitem
     WHERE ordemid IN lt_ordemid_range.
  ENDMETHOD.


  METHOD ov_itemset_update_entity.
    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

    er_entity-ordemid  = it_key_tab[ name = 'OrdemID' ]-value.
    er_entity-itemid   = it_key_tab[ name = 'ItemID' ]-value.
    er_entity-precotot = er_entity-quantidade * er_entity-precouni.

    UPDATE ztovitem
       SET material   = er_entity-material
           descricao  = er_entity-descricao
           quantidade = er_entity-quantidade
           precouni   = er_entity-precouni
           precotot   = er_entity-precotot
     WHERE ordemid    = er_entity-ordemid
       AND itemid     = er_entity-itemid.

    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao atualizar item'
      ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  ENDMETHOD.


  method OV_MESSAGESET_CREATE_ENTITY.
  endmethod.


  method OV_MESSAGESET_DELETE_ENTITY.
  endmethod.


  method OV_MESSAGESET_GET_ENTITY.
  endmethod.


  method OV_MESSAGESET_GET_ENTITYSET.
  endmethod.


  method OV_MESSAGESET_UPDATE_ENTITY.
  endmethod.
ENDCLASS.
