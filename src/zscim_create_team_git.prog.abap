*&---------------------------------------------------------------------*
*& Report ZSCIM_CREATE_TEAM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscim_create_team_git.

DATA: profile        TYPE oa2c_profile,
      target         TYPE string,
      http_method    TYPE string,
      http_client    TYPE REF TO if_http_client,
      oa2c_client    TYPE REF TO if_oauth2_client,
      status_code    TYPE i,
      response_data  TYPE string,
      fields         TYPE tihttpnvp,
      hfields        TYPE tihttpnvp,
      oa2c_exception TYPE REF TO cx_oa2c.

FIELD-SYMBOLS: <ls_field> LIKE LINE OF fields.

PARAMETERS:

  p_prof TYPE oa2c_profile
            OBLIGATORY
            DEFAULT 'ZOAUTH_SCIM',

  p_role TYPE agr_name
        OBLIGATORY
        DEFAULT 'Z_SAC_SCIM_TEST_01',

  p_team TYPE char20
        OBLIGATORY
        DEFAULT 'SCIM_TEST_ROLE_01',

  p_teamd TYPE char30
        OBLIGATORY
        DEFAULT 'SCIM Test Team Name'.


CLASS zcl_scim_util DEFINITION.
  PUBLIC SECTION.
*  DATA:
    TYPES: BEGIN OF ty_final,
             active   TYPE string,
             id       TYPE string,
             username TYPE string,
           END OF ty_final,

           BEGIN OF ty_users,
             bname      TYPE xubname,
             persnumber TYPE ad_persnum,
             addrnumber TYPE ad_addrnum,
             smtp_addr  TYPE ad_smtpadr,
           END OF ty_users.


    TYPES: tt_sac_users TYPE STANDARD TABLE OF ty_final WITH DEFAULT KEY.
    TYPES: tt_bw_users TYPE STANDARD TABLE OF ty_users.

    METHODS constructor IMPORTING im_prof TYPE oa2c_profile im_team type char20 im_teamd type char30.
    METHODS: get_users_from_role IMPORTING im_role TYPE agr_name EXPORTING ex_users TYPE tt_bw_users.
    METHODS: get_sac_users EXPORTING ex_sacusers TYPE tt_sac_users.
    METHODS: create_team.
    METHODS: add_users_to_team IMPORTING im_user TYPE tt_sac_users.


  PRIVATE SECTION.
    DATA gv_oa2c_client TYPE REF TO if_oauth2_client.
    DATA gv_http_client type ref to if_http_client.
    DATA gv_profile TYPE oa2c_profile.
    DATA gv_teamdesc type char30.
    DATA gv_teamid type char20.
    DATA gt_cookies TYPE tihttpcki.
    DATA gv_csrf TYPE STRING.
    DATA gt_headers TYPE tihttpnvp.

ENDCLASS.


TYPES: BEGIN OF ty_final,
         active   TYPE string,
         id       TYPE string,
         username TYPE string,
       END OF ty_final.


DATA: lr_data  TYPE REF TO data,
      ls_final TYPE ty_final,
      lt_final TYPE TABLE OF ty_final.


CLASS zcl_scim_util IMPLEMENTATION.
  METHOD get_users_from_role.

    SELECT DISTINCT COUNT( * )
    FROM agr_define
    WHERE agr_name EQ @im_role
    INTO @DATA(count).

    IF count EQ 0.
      WRITE: 'Error: Role does not exist'.
      EXIT.
    ENDIF.

    "Select Users with given Role
    SELECT *
      FROM agr_users
      WHERE agr_name EQ @im_role
      INTO TABLE @DATA(user_roles).

    "READ BW Userlist with E-Mail Adressess
*    SELECT *
*      FROM zuserlist
*      FOR ALL ENTRIES IN @user_roles
*      WHERE bname = @user_roles-uname
*      INTO TABLE @ex_users.


    SELECT u~bname,
           u~persnumber,
           u~addrnumber,
           a~smtp_addr
      FROM usr21 AS u
      INNER JOIN adr6 AS a  ON ( a~persnumber = u~persnumber AND a~addrnumber = u~addrnumber )
      INTO TABLE @ex_users
      WHERE u~bname IN ( SELECT uname
         FROM agr_users
                 WHERE agr_name EQ @im_role ).

  ENDMETHOD.

  METHOD constructor.

    gv_profile = im_prof.
    gv_teamdesc = im_teamd.
    gv_teamid = im_team.

  ENDMETHOD.

  METHOD get_sac_users.


    target =  `https://zpartner.eu10.hcs.cloud.sap/api/v1/scim/Users`.
    http_method = 'GET'.


    cl_http_client=>create_by_url( EXPORTING url                = target
                                         ssl_id             = 'ANONYM' "'DFAULT'
                               IMPORTING client             = gv_http_client
                               EXCEPTIONS argument_not_found = 1
                                          plugin_not_active  = 2
                                          internal_error     = 3
                                          OTHERS             = 4 ).


    CALL METHOD gv_http_client->request->set_header_field
      EXPORTING
        name  = 'X-CSRF-Token'
        value = 'Fetch'.

    gv_http_client->propertytype_logon_popup = 0.
    gv_http_client->request->set_method( EXPORTING method = http_method ).

    TRY.
        gv_oa2c_client = cl_oauth2_client=>create( EXPORTING i_profile = p_prof ).
      CATCH cx_oa2c INTO oa2c_exception.
        WRITE: `Error calling CREATE.`.
        WRITE: / oa2c_exception->get_text( ).
        RETURN.
    ENDTRY.


    TRY.

        gv_oa2c_client->set_token( EXPORTING io_http_client = gv_http_client ).
      CATCH cx_oa2c INTO oa2c_exception.
        TRY.
            gv_oa2c_client->execute_refresh_flow( ).
          CATCH cx_oa2c INTO oa2c_exception.
            WRITE: `Error calling EXECUTE_REFRESH_FLOW.`.
            WRITE: / oa2c_exception->get_text( ).
            RETURN.
        ENDTRY.
        TRY.
            gv_oa2c_client->set_token( EXPORTING io_http_client = gv_http_client ).
          CATCH cx_oa2c INTO oa2c_exception.
            WRITE: `Error calling SET_TOKEN.`.
            WRITE: / oa2c_exception->get_text( ).
            RETURN.
        ENDTRY.
    ENDTRY.

    DATA: lv_data     TYPE xstring.
    " Setting Header fields

    gv_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.
    gv_http_client->propertytype_accept_compress = if_http_client=>co_enabled.


    gv_http_client->send( EXCEPTIONS http_communication_failure = 1
                                     http_invalid_state         = 2
                                     http_processing_failed     = 3
                                     http_invalid_timeout       = 4
                                     OTHERS                     = 5 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    WRITE: / 'http_client->receive is called'.
    gv_http_client->receive( EXCEPTIONS http_communication_failure = 1
                                        http_invalid_state         = 2
                                        http_processing_failed     = 3
                                        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " Display result
    gv_http_client->response->get_status( IMPORTING code = status_code ).


    response_data = gv_http_client->response->get_cdata( ).




    " Deserialize data and write into internal table

    TYPES: BEGIN OF ty_sacusers,
             active   TYPE string,
             id       TYPE string,
             username TYPE string,
           END OF ty_sacusers.


    DATA: lr_data  TYPE REF TO data,
          ls_sacusers TYPE ty_sacusers.

    FIELD-SYMBOLS:
      <data>        TYPE data,
      <results>     TYPE any,
      <structure>   TYPE any,
      <table>       TYPE ANY TABLE,
      <field>       TYPE any,
      <field_value> TYPE data.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json         = response_data
        pretty_name  = /ui2/cl_json=>pretty_mode-user
        assoc_arrays = abap_true
      CHANGING
        data         = lr_data.


    IF lr_data IS BOUND.
      ASSIGN lr_data->* TO <data>.
      ASSIGN COMPONENT `RESOURCES` OF STRUCTURE <data> TO <results>.
      ASSIGN <results>->* TO <table>.

      LOOP AT <table> ASSIGNING <structure>.
        ASSIGN <structure>->* TO <data>.

        ASSIGN COMPONENT `ACTIVE` OF STRUCTURE <data> TO <field>.
        IF <field> IS ASSIGNED.
          lr_data = <field>.
          ASSIGN lr_data->* TO <field_value>.
          ls_sacusers-active = <field_value>.
        ENDIF.
        UNASSIGN: <field>, <field_value>.

        ASSIGN COMPONENT `ID` OF STRUCTURE <data> TO <field>.
        IF <field> IS ASSIGNED.
          lr_data = <field>.
          ASSIGN lr_data->* TO <field_value>.
          ls_sacusers-id = <field_value>.
        ENDIF.
        UNASSIGN: <field>, <field_value>.

        ASSIGN COMPONENT `USERNAME` OF STRUCTURE <data> TO <field>.
        IF <field> IS ASSIGNED.
          lr_data = <field>.
          ASSIGN lr_data->* TO <field_value>.
          ls_sacusers-username = <field_value>.
        ENDIF.
        UNASSIGN: <field>, <field_value>.

        APPEND ls_sacusers TO ex_sacusers.

        CLEAR ls_sacusers.
      ENDLOOP.
    ENDIF.

    gv_http_client->response->get_cookies( CHANGING cookies = gt_cookies ).

    gv_http_client->response->get_header_fields( CHANGING fields = gt_headers ).

    READ TABLE gt_headers ASSIGNING FIELD-SYMBOL(<fs_fields>)
      WITH KEY name = 'x-csrf-token'.
    IF sy-subrc IS INITIAL.
      gv_csrf = <fs_fields>-value.
    ENDIF.

    Free gv_http_client.

  ENDMETHOD.

  METHOD create_team.

    target =  `https://zpartner.eu10.hcs.cloud.sap/api/v1/scim/Groups/`.
    http_method = 'POST'.


    cl_http_client=>create_by_url( EXPORTING url                = target
                                         ssl_id             = 'ANONYM' "'DFAULT'
                               IMPORTING client             = gv_http_client
                               EXCEPTIONS argument_not_found = 1
                                          plugin_not_active  = 2
                                          internal_error     = 3
                                          OTHERS             = 4 ).


    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    gv_http_client->propertytype_logon_popup = 0.
    gv_http_client->request->set_method( EXPORTING method = http_method ).

    gv_http_client->request->set_content_type( 'application/json' ).

    CALL METHOD gv_http_client->request->set_header_field
          EXPORTING
            name  = 'X-CSRF-Token'
            value = gv_csrf.

                 TRY.
                     gv_oa2c_client->set_token( EXPORTING io_http_client = gv_http_client ).
                   CATCH cx_oa2c INTO oa2c_exception.
                     TRY.
                         gv_oa2c_client->execute_refresh_flow( ).
                       CATCH cx_oa2c INTO oa2c_exception.
                         WRITE: `Error calling EXECUTE_REFRESH_FLOW.`.
                         WRITE: / oa2c_exception->get_text( ).
                         RETURN.
                     ENDTRY.
                     TRY.
                         gv_oa2c_client->set_token( EXPORTING io_http_client = gv_http_client ).
                       CATCH cx_oa2c INTO oa2c_exception.
                         WRITE: `Error calling SET_TOKEN.`.
                         WRITE: / oa2c_exception->get_text( ).
                         RETURN.
                     ENDTRY.
                 ENDTRY.

      "" Set Cookies from first call

      LOOP AT gt_cookies ASSIGNING FIELD-SYMBOL(<cookie2>).
          gv_http_client->request->set_cookie( name = <cookie2>-name
                                                 value = <cookie2>-value ).
     ENDLOOP.


     """ Build payload

     DATA: lv_datastr  TYPE string.
        CONCATENATE '{"schemas": ["urn:ietf:params:scim:schemas:core:2.0:Group"],"id":"' gv_teamid '","displayName": "' gv_teamdesc  '"}' INTO lv_datastr.


   gv_http_client->request->set_cdata(
   EXPORTING
    data    =  lv_datastr
     ).

     gv_http_client->send( EXCEPTIONS http_communication_failure = 1
                                 http_invalid_state         = 2
                                 http_processing_failed     = 3
                                 http_invalid_timeout       = 4
                                 OTHERS                     = 5 ).
     IF sy-subrc <> 0.
       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
     ENDIF.
     WRITE: / 'http_client->receive is called'.
     gv_http_client->receive( EXCEPTIONS http_communication_failure = 1
                                         http_invalid_state         = 2
                                         http_processing_failed     = 3
                                         OTHERS                     = 4 ).
     IF sy-subrc <> 0.
       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
     ENDIF.

*     gv_http_client->response->get_header_fields( CHANGING fields = fields_s ).
      response_data = gv_http_client->response->get_cdata( ).

      WRITE / |{ response_data }|.

      Free gv_http_client.

  ENDMETHOD.

METHOD add_users_to_team.

    CONCATENATE 'https://zpartner.eu10.hcs.cloud.sap/api/v1/scim/Groups/' gv_teamid INTO target .
    http_method = 'PUT'.


    cl_http_client=>create_by_url( EXPORTING url                = target
                                         ssl_id             = 'ANONYM' "'DFAULT'
                               IMPORTING client             = gv_http_client
                               EXCEPTIONS argument_not_found = 1
                                          plugin_not_active  = 2
                                          internal_error     = 3
                                          OTHERS             = 4 ).


    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    gv_http_client->propertytype_logon_popup = 0.
    gv_http_client->request->set_method( EXPORTING method = http_method ).

    gv_http_client->request->set_content_type( 'application/json' ).

    CALL METHOD gv_http_client->request->set_header_field
          EXPORTING
            name  = 'X-CSRF-Token'
            value = gv_csrf.

                 TRY.
                     gv_oa2c_client->set_token( EXPORTING io_http_client = gv_http_client ).
                   CATCH cx_oa2c INTO oa2c_exception.
                     TRY.
                         gv_oa2c_client->execute_refresh_flow( ).
                       CATCH cx_oa2c INTO oa2c_exception.
                         WRITE: `Error calling EXECUTE_REFRESH_FLOW.`.
                         WRITE: / oa2c_exception->get_text( ).
                         RETURN.
                     ENDTRY.
                     TRY.
                         gv_oa2c_client->set_token( EXPORTING io_http_client = gv_http_client ).
                       CATCH cx_oa2c INTO oa2c_exception.
                         WRITE: `Error calling SET_TOKEN.`.
                         WRITE: / oa2c_exception->get_text( ).
                         RETURN.
                     ENDTRY.
                 ENDTRY.

      "" Set Cookies from first call

      LOOP AT gt_cookies ASSIGNING FIELD-SYMBOL(<cookie2>).
          gv_http_client->request->set_cookie( name = <cookie2>-name
                                                 value = <cookie2>-value ).
     ENDLOOP.


     """ Build payload

     DATA: lv_datastr  TYPE string.
     CONCATENATE '{"schemas": ["urn:ietf:params:scim:schemas:core:2.0:Group"],"id": "' p_team  '","displayName": "'  p_team '","members": [' INTO lv_datastr.

     LOOP AT im_user ASSIGNING FIELD-SYMBOL(<fs_sac_user>).
       IF <fs_sac_user>-active = 'X'.
          CONCATENATE lv_datastr  '{ "value": "'  <fs_sac_user>-id  '"},' INTO lv_datastr.
        ENDIF.
      ENDLOOP.

              DATA lv_length TYPE i.
              lv_length = strlen( lv_datastr ) - 1.
              lv_datastr = lv_datastr+0(lv_length).

              CONCATENATE lv_datastr  '] }' INTO lv_datastr.


   gv_http_client->request->set_cdata(
   EXPORTING
    data    =  lv_datastr
     ).

     gv_http_client->send( EXCEPTIONS http_communication_failure = 1
                                 http_invalid_state         = 2
                                 http_processing_failed     = 3
                                 http_invalid_timeout       = 4
                                 OTHERS                     = 5 ).
     IF sy-subrc <> 0.
       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
     ENDIF.
     WRITE: / 'http_client->receive is called'.
     gv_http_client->receive( EXCEPTIONS http_communication_failure = 1
                                         http_invalid_state         = 2
                                         http_processing_failed     = 3
                                         OTHERS                     = 4 ).
     IF sy-subrc <> 0.
       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
     ENDIF.

*     gv_http_client->response->get_header_fields( CHANGING fields = fields_s ).
      response_data = gv_http_client->response->get_cdata( ).

      WRITE / |{ response_data }|.

ENDMETHOD.


ENDCLASS.

START-OF-SELECTION.

  " Get user role assignment from system

  DATA(scim_cl) = NEW zcl_scim_util( im_prof = p_prof im_team = p_team im_teamd = p_teamd  ).

  scim_cl->get_users_from_role(
    EXPORTING
      im_role  = p_role
         IMPORTING
      ex_users = DATA(lt_users)
  ).

  scim_cl->get_sac_users(
    IMPORTING
      ex_sacusers = DATA(lt_sacuser)
  ).

  " Match users BW with SAC through emailadress

  LOOP AT lt_sacuser ASSIGNING FIELD-SYMBOL(<fs_sacuser>).
    <fs_sacuser>-active = ''.
    LOOP AT lt_users ASSIGNING FIELD-SYMBOL(<fs_bw_user>).
      IF <fs_sacuser>-username = <fs_bw_user>-smtp_addr.
        <fs_sacuser>-active = 'X'.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  " Create Team

  scim_cl->create_team(  ).

  scim_cl->add_users_to_team( EXPORTING im_user = lt_sacuser ).
