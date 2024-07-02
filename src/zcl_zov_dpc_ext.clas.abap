class ZCL_ZOV_DPC_EXT definition
  public
  inheriting from ZCL_ZOV_DPC
  create public .

public section.
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
  methods OV_MESSAGESET_UPDATE_ENTITY
    redefinition .
  methods OV_MESSAGESET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZOV_DPC_EXT IMPLEMENTATION.


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


  method OV_HEADERSET_DELETE_ENTITY.
  endmethod.


  method OV_HEADERSET_GET_ENTITY.
  endmethod.


  method OV_HEADERSET_GET_ENTITYSET.
  endmethod.


  method OV_HEADERSET_UPDATE_ENTITY.
  endmethod.


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


  method OV_ITEMSET_DELETE_ENTITY.
  endmethod.


  method OV_ITEMSET_GET_ENTITY.
  endmethod.


  method OV_ITEMSET_GET_ENTITYSET.
  endmethod.


  method OV_ITEMSET_UPDATE_ENTITY.
  endmethod.


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
