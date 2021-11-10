CLASS zcl_email_wrapper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_distribution_list TYPE STANDARD TABLE OF so_obj_nam,
           tt_email             TYPE STANDARD TABLE OF ad_smtpadr.
    METHODS: constructor
      IMPORTING
                iv_type    TYPE so_obj_tp DEFAULT 'RAW'
                iv_subject TYPE so_obj_des
      RAISING   cx_document_bcs,

      set_body
        IMPORTING
          it_text   TYPE    soli_tab OPTIONAL
          it_binary TYPE    solix_tab OPTIONAL,

      create_html_body_from_so10
        IMPORTING
          iv_reciever_name      TYPE string  DEFAULT 'Recipient'
          iv_hdr_so_txt         TYPE thead-tdname
          iv_ftr_so_txt         TYPE thead-tdname OPTIONAL
          it_embed_table        TYPE ANY TABLE OPTIONAL
          iv_table_as_attchment TYPE boolean DEFAULT abap_true
        RAISING
          cx_document_bcs,

      create_html_body_from_template
        IMPORTING
          iv_master_template    TYPE swww_t_template_name OPTIONAL
          iv_mail_body_template TYPE swww_t_template_name
          it_placeholders       TYPE swww_t_merge_table
        RAISING
          cx_document_bcs,
      set_sender
        IMPORTING
                  iv_sender_email TYPE adsmtp-smtp_addr DEFAULT 'noreply-sap@mmal.com.au'
                  iv_sender_name  TYPE string DEFAULT 'MMAL SAP Notifications'
        RAISING   cx_document_bcs,

      set_recievers_dist_list
        IMPORTING
                  it_dist_list       TYPE tt_distribution_list
                  iv_dist_is_private TYPE boolean DEFAULT abap_false
                  iv_send_as_cc      TYPE boolean DEFAULT abap_false
                  iv_send_as_bcc     TYPE boolean DEFAULT abap_false
        RAISING   cx_document_bcs,
      set_reciever_email
        IMPORTING
                  it_email       TYPE tt_email
                  iv_send_as_cc  TYPE boolean DEFAULT abap_false
                  iv_send_as_bcc TYPE boolean DEFAULT abap_false
        RAISING   cx_document_bcs,
      add_attachment
        IMPORTING
          iv_type           TYPE so_obj_tp
          iv_name           TYPE so_obj_des
          iv_size           TYPE so_obj_len OPTIONAL
          it_text_content   TYPE soli_tab OPTIONAL
          it_binary_content TYPE solix_tab OPTIONAL
          it_header         TYPE soli_tab OPTIONAL,
      send
        IMPORTING
          iv_importance TYPE bcs_docimp OPTIONAL
        RAISING
          cx_document_bcs,
      get_html_table
        IMPORTING
          iv_add_attchment    TYPE boolean DEFAULT abap_false
          im_table            TYPE table
        RETURNING
          VALUE(rv_htm_table) TYPE string,

      conv_html_string_to_w3htmltab
        IMPORTING iv_string          TYPE string
                  iv_max_line_length TYPE i DEFAULT 255
        RETURNING VALUE(rt_table)    TYPE w3htmltab.


  PROTECTED SECTION.
    METHODS: edit_value IMPORTING fragment TYPE any
                        CHANGING  buffer   TYPE string,
      append IMPORTING  fragment TYPE any,
      read_so10_text
        IMPORTING
          im_hdr_so_txt  TYPE thead-tdname
        RETURNING
          VALUE(rt_line) TYPE tline_tab
        RAISING
          cx_document_bcs.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_attachment,
             type    TYPE so_obj_tp,
             subject TYPE so_obj_des,
             size    TYPE so_obj_len,
             text    TYPE soli_tab,
             binary  TYPE solix_tab,
             header  TYPE soli_tab,
           END OF ts_attachment,
           tt_attachment TYPE STANDARD TABLE OF ts_attachment.

    DATA: mo_bcs             TYPE REF TO cl_bcs,
          mo_document        TYPE REF TO cl_document_bcs,
          mv_email_type      TYPE so_obj_tp,
          mv_subject         TYPE so_obj_des,
          mt_body_binary     TYPE solix_tab,
          mt_body_text       TYPE soli_tab,
          mt_attachment      TYPE tt_attachment,
          mv_htm_body_string TYPE string.
    CONSTANTS cr_lf TYPE string VALUE '<cr_lf>'.
    METHODS raise_addr_bcs_exception
      IMPORTING
        io_error_addr TYPE REF TO cx_address_bcs
      RAISING
        cx_document_bcs.
    METHODS raise_send_req_exception
      IMPORTING
        io_error TYPE REF TO cx_send_req_bcs
      RAISING
        cx_document_bcs.
    METHODS raise_exception_sys_msg
      RAISING
        cx_document_bcs.
    METHODS raise_bcs_exception
      IMPORTING
        io_error TYPE REF TO cx_bcs
      RAISING
        cx_document_bcs.
ENDCLASS.



CLASS zcl_email_wrapper IMPLEMENTATION.
  METHOD constructor.
    DATA: lv_syscat TYPE t000-cccategory.
    TRY.
        mo_bcs = cl_bcs=>create_persistent( ).
      CATCH cx_send_req_bcs INTO DATA(lo_error).
        "handle exception
        raise_send_req_exception( lo_error ).
    ENDTRY.

    mv_email_type = iv_type.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        system_client_role = lv_syscat
      EXCEPTIONS
        no_systemname      = 1
        no_systemtype      = 2
        OTHERS             = 3.

    CASE lv_syscat.
      WHEN 'P'.
        mv_subject = iv_subject.
      WHEN OTHERS.
        mv_subject = |[TEST] { iv_subject }|.
    ENDCASE.


  ENDMETHOD.
  METHOD set_body.
    mt_body_binary = it_binary.
    mt_body_text = it_text.
  ENDMETHOD.

  METHOD set_sender.
    TRY.
        DATA(lo_sender) = cl_cam_address_bcs=>create_internet_address( i_address_string = iv_sender_email
                                                                       i_address_name = CONV #( iv_sender_name ) ).

        mo_bcs->set_sender( lo_sender ).
      CATCH cx_send_req_bcs INTO DATA(lo_error).
        raise_send_req_exception( lo_error ).
      CATCH cx_address_bcs INTO DATA(lo_error_addr).
        raise_addr_bcs_exception( lo_error_addr ).
    ENDTRY.

  ENDMETHOD.

  METHOD set_recievers_dist_list.
    LOOP AT it_dist_list ASSIGNING FIELD-SYMBOL(<fs_dl>).
      TRY.
          DATA(lo_dl_bcs) = cl_distributionlist_bcs=>getu_persistent(
                         i_dliname = <fs_dl>
                         i_private = iv_dist_is_private ).
          mo_bcs->add_recipient(
            EXPORTING
              i_recipient     = lo_dl_bcs    " Recipient of Message
              i_copy          = iv_send_as_cc    " Send Copy
              i_blind_copy    = iv_send_as_bcc    " Send As Blind Copy
          ).
        CATCH cx_address_bcs INTO DATA(lo_error_addr).
          raise_addr_bcs_exception( lo_error_addr ).
        CATCH cx_send_req_bcs INTO DATA(lo_error).
          raise_send_req_exception( lo_error ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD raise_send_req_exception.

    RAISE EXCEPTION TYPE cx_document_bcs
      EXPORTING
        msgid = io_error->msgid
        msgno = io_error->msgno
        msgty = io_error->msgty
        msgv1 = io_error->msgv1
        msgv2 = io_error->msgv2
        msgv3 = io_error->msgv3
        msgv4 = io_error->msgv4.

  ENDMETHOD.



  METHOD raise_addr_bcs_exception.

    RAISE EXCEPTION TYPE cx_document_bcs
      EXPORTING
        msgid = io_error_addr->msgid
        msgno = io_error_addr->msgno
        msgty = io_error_addr->msgty
        msgv1 = io_error_addr->msgv1
        msgv2 = io_error_addr->msgv2
        msgv3 = io_error_addr->msgv3
        msgv4 = io_error_addr->msgv4.

  ENDMETHOD.



  METHOD set_reciever_email.
    LOOP AT it_email ASSIGNING FIELD-SYMBOL(<fs_email>).
      TRY.
          DATA(lo_reciever_email) = cl_cam_address_bcs=>create_internet_address( <fs_email> ).
          mo_bcs->add_recipient( EXPORTING
                                  i_recipient     = lo_reciever_email    " Recipient of Message
                                  i_copy          = iv_send_as_cc    " Send Copy
                                  i_blind_copy    = iv_send_as_bcc    " Send As Blind Copy
                              ).
        CATCH cx_address_bcs INTO DATA(lo_error_addr).
          raise_addr_bcs_exception( lo_error_addr ).
        CATCH cx_send_req_bcs INTO DATA(lo_error).
          raise_send_req_exception( lo_error ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_attachment.
    APPEND INITIAL LINE TO mt_attachment ASSIGNING FIELD-SYMBOL(<fs_attchment>).
    <fs_attchment>-type = iv_type.
    <fs_attchment>-subject = iv_name.
    <fs_attchment>-size = iv_size.
    <fs_attchment>-text = it_text_content.
    <fs_attchment>-binary = it_binary_content.
    <fs_attchment>-header = it_header.
  ENDMETHOD.

  METHOD send.
    DATA(lo_bcs_doc) = cl_document_bcs=>create_document(
                       i_type          = mv_email_type
                       i_subject       = mv_subject
                       i_importance    = iv_importance
                       i_text          = mt_body_text
                       i_hex           = mt_body_binary
                   ).

    LOOP AT mt_attachment ASSIGNING FIELD-SYMBOL(<fs_attachment>).
      lo_bcs_doc->add_attachment(
        EXPORTING
          i_attachment_type     = <fs_attachment>-type    " Document Class for Attachment
          i_attachment_subject  = <fs_attachment>-subject    " Attachment Title
          i_attachment_size     = <fs_attachment>-size    " Size of Document Content
          i_att_content_text    = <fs_attachment>-text    " Content (Text-Like)
          i_att_content_hex     = <fs_attachment>-binary    " Content (Binary)
          i_attachment_header   = <fs_attachment>-header    " Attachment Header Data
      ).
    ENDLOOP.
    TRY.
        mo_bcs->set_document( lo_bcs_doc ).
        IF mo_bcs->send( ) NE abap_true.
          raise_exception_sys_msg( ).
        ENDIF.

      CATCH cx_send_req_bcs INTO DATA(lo_error).
        raise_send_req_exception( lo_error ).
    ENDTRY.


  ENDMETHOD.

  METHOD raise_exception_sys_msg.

    RAISE EXCEPTION TYPE cx_document_bcs
      EXPORTING
        msgid = sy-msgid
        msgno = sy-msgno
        msgty = sy-msgty
        msgv1 = sy-msgv1
        msgv2 = sy-msgv2
        msgv3 = sy-msgv3
        msgv4 = sy-msgv4.

  ENDMETHOD.



  METHOD create_html_body_from_so10.
* Addressed to:
    append( |Dear { iv_reciever_name }| ).
    append( '<P>' ).

* Header part
    DATA(lt_line) = read_so10_text( iv_hdr_so_txt ).
    LOOP AT lt_line ASSIGNING FIELD-SYMBOL(<fs_line>).
      append( <fs_line>-tdline ).
      append( '<BR>' ).
    ENDLOOP.
    append( '</P>' ).


* Add table is supplied
    IF it_embed_table IS SUPPLIED AND it_embed_table IS NOT INITIAL.
      DATA(lv_htm_table) = get_html_table( iv_add_attchment = iv_table_as_attchment
                                           im_table = it_embed_table ).
      append( lv_htm_table ).
    ENDIF.

* Add Body Footer
    CLEAR lt_line.
    lt_line = read_so10_text( iv_ftr_so_txt ).
    append( '<P>' ).
    LOOP AT lt_line ASSIGNING <fs_line>.
      append( <fs_line>-tdline ).
      append( '<BR>' ).
    ENDLOOP.
    append( '</P>' ).

    TRY.
        cl_bcs_convert=>string_to_solix(
          EXPORTING
            iv_string   = mv_htm_body_string     " Input data
          IMPORTING
            et_solix    = DATA(lt_body_binary)    " Output data
        ).

        set_body( it_binary = lt_body_binary ).

      CATCH cx_bcs INTO DATA(lo_error).
        "handle exception
        raise_bcs_exception( lo_error ).
    ENDTRY.
*  CATCH cx_bcs.    "
  ENDMETHOD.

  METHOD raise_bcs_exception.

    RAISE EXCEPTION TYPE cx_document_bcs
      EXPORTING
        msgid = io_error->msgid
        msgno = io_error->msgno
        msgty = io_error->msgty
        msgv1 = io_error->msgv1
        msgv2 = io_error->msgv2
        msgv3 = io_error->msgv3
        msgv4 = io_error->msgv4.

  ENDMETHOD.



  METHOD edit_value.
    DATA: inttype TYPE inttype .

    DESCRIBE FIELD fragment TYPE inttype .

    DATA: edited TYPE so_text255 .

    WRITE fragment TO edited LEFT-JUSTIFIED .

    IF buffer EQ me->cr_lf .
      RETURN .
    ENDIF .

    CASE inttype .
      WHEN 'I' OR 'P' .

        IF fragment LT 0 .
          WRITE fragment TO edited+1 NO-SIGN LEFT-JUSTIFIED .
          WRITE '-' TO edited+0(1) .
        ENDIF .

        buffer = edited .

      WHEN OTHERS .
        buffer = fragment .
    ENDCASE .
  ENDMETHOD.

  METHOD append.
    DATA: buffer TYPE string .

    me->edit_value( EXPORTING fragment = fragment CHANGING buffer = buffer ) .

    CONDENSE buffer .

    CONCATENATE mv_htm_body_string buffer INTO mv_htm_body_string .
  ENDMETHOD.

  METHOD read_so10_text.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = 'ST'
        language                = sy-langu
        name                    = im_hdr_so_txt
        object                  = 'TEXT'
      TABLES
        lines                   = rt_line
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      raise_exception_sys_msg( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_html_table.
    DATA: lv_buffer   TYPE string,
          lo_data     TYPE REF TO data,
          lt_ddfields TYPE ddfields.

    FIELD-SYMBOLS:
      <fs_ddfields>  TYPE dfies,
      <fs_data>      TYPE any,
      <fs_component> TYPE any.

    DEFINE t_append.
      me->edit_value( EXPORTING fragment = &1 CHANGING buffer = lv_buffer ) .
      CONDENSE lv_buffer .
      CONCATENATE rv_htm_table lv_buffer INTO rv_htm_table.
      CLEAR lv_buffer.
    END-OF-DEFINITION.

* Get DDIC Information
    CALL METHOD cl_salv_ddic=>get_by_data
      EXPORTING
        data    = im_table
      RECEIVING
        t_dfies = lt_ddfields.

    t_append '<table border="1" cellspacing="0">' .
    t_append '<tr>' .

    LOOP AT lt_ddfields ASSIGNING <fs_ddfields>  .
      t_append '<th>' .
      t_append <fs_ddfields>-scrtext_l .
      t_append '</th>'.
    ENDLOOP .

    t_append '</tr>' .

    CREATE DATA lo_data LIKE LINE OF im_table .

    ASSIGN lo_data->* TO <fs_data> .

    LOOP AT im_table ASSIGNING <fs_data> .
      t_append '<tr>'.

      LOOP AT lt_ddfields ASSIGNING <fs_ddfields>  .
        t_append '<td>'.
        ASSIGN COMPONENT <fs_ddfields>-fieldname OF STRUCTURE <fs_data> TO <fs_component> .
        IF <fs_component> IS ASSIGNED.
          t_append <fs_component>.
        ENDIF.
        t_append '</td>' .
      ENDLOOP .

      t_append '</tr>' .
    ENDLOOP .

    t_append '</table>' .

    IF iv_add_attchment EQ abap_true AND rv_htm_table IS NOT INITIAL.
      APPEND INITIAL LINE TO mt_attachment ASSIGNING FIELD-SYMBOL(<fs_attachment>).
      <fs_attachment>-type    = 'htm'.
      <fs_attachment>-subject = 'attachment HTM (htm)'.
      <fs_attachment>-text = cl_bcs_convert=>string_to_soli( rv_htm_table ).
    ENDIF.
  ENDMETHOD.

  METHOD create_html_body_from_template.
* Templates used in tcode SMW0
* Placeholder commands: For more info check below link
*  https://help.sap.com/saphelp_autoid2007/helpdata/en/2b/d921034b8a11d1894c0000e8323c4f/content.htm?no_cache=true
*Command         Description
*space           Replace placeholder line (default).
*B               Insert before placeholder line.
*A               Insert after placeholder line.
*R               Replace placeholder only.

* If you are using a master template, make sure the placeholder to add body template is !MAIN_BODY!

* If you want to use a table as a placeholder, use the method get_html_table to get html code for
*  any table. Then use method conv_html_string_to_w3htmltab  to get placeholder-html. Append this line with
*  defined place holder name, command as R and value as this table.
    DATA: lt_html_body TYPE swww_t_html_table.
    DATA(lt_placeholders) = it_placeholders.
    CALL FUNCTION 'WWW_HTML_MERGER'
      EXPORTING
        template           = iv_mail_body_template
      IMPORTING
        html_table         = lt_html_body
      CHANGING
        merge_table        = lt_placeholders
      EXCEPTIONS
        template_not_found = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      raise_exception_sys_msg( ).
    ENDIF.

    IF iv_master_template IS SUPPLIED AND iv_master_template IS NOT INITIAL.
      lt_placeholders = VALUE #( BASE lt_placeholders ( name = '!MAIN_BODY!' html = lt_html_body ) ).
      CLEAR lt_html_body.
      CALL FUNCTION 'WWW_HTML_MERGER'
        EXPORTING
          template           = iv_master_template
        IMPORTING
          html_table         = lt_html_body
        CHANGING
          merge_table        = lt_placeholders
        EXCEPTIONS
          template_not_found = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.
        raise_exception_sys_msg( ).
      ENDIF.
    ENDIF.
    set_body( it_text   = lt_html_body ).
  ENDMETHOD.

  METHOD conv_html_string_to_w3htmltab.
    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = iv_string " String that is to be cut into the table
        i_tabline_length = iv_max_line_length    " Length of the table lines
      TABLES
        et_table         = rt_table.     " Character field lines (pass length in import parameter)
  ENDMETHOD.

ENDCLASS.
