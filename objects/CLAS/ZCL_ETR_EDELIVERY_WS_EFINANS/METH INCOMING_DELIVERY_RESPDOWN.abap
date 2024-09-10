  METHOD incoming_delivery_respdown.
    DATA: lv_request_xml  TYPE string,
          lv_response_xml TYPE string,
          lv_content      TYPE string.

    IF is_document_numbers-ruuid IS INITIAL.
      RAISE EXCEPTION TYPE zcx_etr_regulative_exception
        MESSAGE e072(zetr_common).
    ENDIF.

    FIELD-SYMBOLS: <lv_return_field> TYPE any.

    CONCATENATE
    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ser="http://service.connector.uut.cs.com.tr/">'
      '<soapenv:Header>'
        '<wsse:Security xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd">'
          '<wsse:UsernameToken>'
                '<wsse:Username>' ms_company_parameters-wsusr '</wsse:Username>'
                '<wsse:Password>' ms_company_parameters-wspwd '</wsse:Password>'
          '</wsse:UsernameToken>'
        '</wsse:Security>'
      '</soapenv:Header>'
       '<soapenv:Body>'
       '<ser:gidenBelgeleriIndirEttn>'
              '<vergiTcKimlikNo>' mv_company_taxid '</vergiTcKimlikNo>'
              '<belgeEttnListesi>' is_document_numbers-ruuid '</belgeEttnListesi>'
              '<belgeTuru>IRSALIYE_YANITI</belgeTuru>'
              '<belgeFormati>' iv_content_type '</belgeFormati>'
        '</ser:gidenBelgeleriIndirEttn>'
      '</soapenv:Body>'
    '</soapenv:Envelope>'
    INTO lv_request_xml.
*    mv_request_url = '/efatura/ws/connectorService'.
    lv_response_xml = run_service( lv_request_xml ).

    DATA(lt_xml_table) = zcl_etr_regulative_common=>parse_xml( lv_response_xml ).
    LOOP AT lt_xml_table INTO DATA(ls_xml_line).
      CASE ls_xml_line-name.
        WHEN 'return'.
          CHECK ls_xml_line-node_type = 'CO_NT_VALUE'.
          CONCATENATE lv_content
              ls_xml_line-value
              INTO lv_content.
      ENDCASE.
    ENDLOOP.
    IF lv_content IS NOT INITIAL.
      DATA(lv_zipped_file) = xco_cp=>string( lv_content )->as_xstring( xco_cp_binary=>text_encoding->base64 )->value.
      zcl_etr_regulative_common=>unzip_file_single(
        EXPORTING
          iv_zipped_file_xstr = lv_zipped_file
        IMPORTING
          ev_output_data_xstr = DATA(lv_invoice_content) ).
      rv_response_data = lv_invoice_content.
    ENDIF.
  ENDMETHOD.