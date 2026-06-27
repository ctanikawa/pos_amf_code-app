       IDENTIFICATION              DIVISION.
       PROGRAM-ID.      GEC02.
       AUTHOR.          STEFANINI.
       DATE-WRITTEN.    MARCO/2008.
       SECURITY.

      ******************************************************************
      *                                                                *
      *              DIGITACAO DA CHAVE DE BUSCA - versao main         *
      *                                                                *
      ******************************************************************
      * TRANSACAO = GEC2                         TIPO PROGRAMOTECA: 53 *
      ******************************************************************
      * NOME DO PROGRAMADOR + NOME DO ANALISTA +     DATA     + VERSAO *
      * CARLOS TANIKAWA     + CELIA SATOMI     +  MARCO/08    +        *
      ******************************************************************

      ******************************************************************
      *            ENVIRONMENT     DIVISION                            *
      ******************************************************************
       ENVIRONMENT                 DIVISION.

      ******************************************************************
      *                     CONFIGURATION                              *
      ******************************************************************
       CONFIGURATION               SECTION.

       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

      ******************************************************************
      *            DATA            DIVISION                            *
      ******************************************************************
       DATA                        DIVISION.

       WORKING-STORAGE             SECTION.
      ******************************************************************
      *                      BOOK DO MAPA DO PGM                       *
      ******************************************************************
       01          FILLER.
           03      FILLER          PIC      X(08)  VALUE 'INIC-WRK'.

      ******************************************************************
      *                      CAMPOS AUXILIARES                         *
      ******************************************************************
       01          FILLER.
           03      FILLER          PIC      X(08)  VALUE 'INIC-WRK'.

      ******************************************************************
      *                         SUBROTINAS                             *
      ******************************************************************
       01          FILLER.
           03      FILLER          PIC      X(08)  VALUE 'SUBROTIN'.
           03      SR-YUROVAL      PIC      X(08)  VALUE 'YUROVAL'.
      ******************************************************************
      *                         FLAGS                                  *
      ******************************************************************
       01          FILLER.
           03      FILLER          PIC      X(08)  VALUE 'FLAGS'.
           03      FL-ERRO         PIC      X(03)  VALUE 'NAO'.
           03      FL-PER-I-OK     PIC      X(03)  VALUE 'NAO'.
           03      FL-PER-F-OK     PIC      X(03)  VALUE 'NAO'.
      ******************************************************************
      *                         LITERAIS                               *
      ******************************************************************
       01          FILLER.
           03      FILLER          PIC      X(08)  VALUE 'LITERAIS'.
           03      LT-00           PIC      9(02)  VALUE 00.
           03      LT-01           PIC      9(02)  VALUE 01.
           03      LT-02           PIC      9(02)  VALUE 02.
           03      LT-03           PIC      9(02)  VALUE 03.
           03      LT-04           PIC      9(02)  VALUE 04.
           03      LT-ASRA         PIC      X(04)  VALUE 'ASRA'.
           03      LT-SIM          PIC      X(03)  VALUE 'SIM'.
           03      LT-NAO          PIC      X(03)  VALUE 'NAO'.
           03      LT-GEC02        PIC      X(08)  VALUE 'GEC02'.
           03      LT-ASKTIME      PIC      X(07)  VALUE 'ASKTIME'.
           03      LT-FORMATTIME   PIC      X(10)  VALUE 'FORMATTIME'.
           03      LT-RECEIVE      PIC      X(07)  VALUE 'RECEIVE'.
           03      LT-RETURN       PIC      X(06)  VALUE 'RETURN'.
           03      LT-XCTL         PIC      X(04)  VALUE 'XCTL'.
           03      LT-LINK         PIC      X(04)  VALUE 'LINK'.
           03      LT-SEND-ERASE   PIC      X(10)  VALUE 'SEND ERASE'.
           03      LT-SEND-DATAONLY
                                   PIC      X(13)  VALUE
                                                   'SEND DATAONLY'.
      ******************************************************************
      *                         MENSAGENS                              *
      ******************************************************************
       01          FILLER.
           03      FILLER          PIC      X(16)  VALUE 'MENSAGENS'.

           03      MN-MENS4001A.
             05    FILLER          PIC      X(41)  VALUE
                   'GE0001I GEC02 I4001 ERRO CICS - COMANDO: '.
             05    MN-COMANDO-1    PIC      X(13).
             05    FILLER          PIC      X(12)  VALUE
                   ' - EIBRESP: '.
             05    MN-EIBRESP-1    PIC      9(02).
             05    FILLER          PIC      X(08)  VALUE
                   ' - PTO: '.
             05    MN-PTO-ERRO-1   PIC      9(02).

           03      MN-MENS4001B.
             05    FILLER          PIC      X(26)  VALUE
                   'GE0001I GEC02 I4001 DATA: '.
             05    MN-DATA-1       PIC      X(10)  VALUE  SPACES.
             05    FILLER          PIC      X(09)  VALUE ' - HORA: '.
             05    MN-HORA-1       PIC      X(08)  VALUE  SPACES.

           03      MN-MENS4002.
             05    FILLER          PIC      X(20)  VALUE
                   'GE0002I GEC02 I4002 '.
             05    FILLER          PIC      X(29)  VALUE
                   'ABEND INTERCEPTADO - CODIGO: '.
             05    MN-ABEND-2      PIC      X(04).
             05    FILLER          PIC      X(08)  VALUE
                   ' - PTO: '.
             05    MN-PTO-ERRO-2   PIC      9(02).

           03      MN-MENS4003.
             05    FILLER          PIC      X(29)  VALUE
                   'GE0003I GEC02 I4003 ERRO API '.
             05    MN-API-3        PIC      X(07).
             05    FILLER          PIC      X(13)  VALUE
                   ' - COD.RET.: '.
             05    MN-CODRET-3     PIC      9(02).
             05    FILLER          PIC      X(08)  VALUE ' - PTO: '.
             05    MN-PTO-ERRO-3   PIC      9(02).
             05    FILLER          PIC      X(05)  VALUE ' CHV='.
             05    MN-CHAVE-3      PIC      X(134).

           03      MN-MENS4004.
             05    FILLER          PIC      X(49)  VALUE
                   'DIGITE AS INFORMAÇOES PARA PESQUISA E TECLE ENTER'.

           03      MN-MENS4005.
             05    FILLER          PIC      X(19)  VALUE
                   'TRANSACAO ENCERRADA'.

           03      MN-MENS4006.
             05    FILLER          PIC      X(19)  VALUE
                   'TECLA INVALIDA     '.

           03      MN-MENS4007.
             05    FILLER          PIC      X(38)  VALUE
                   'CAMPO INVÁLIDO, REDIGITE A INFORMAÇÃO'.

           03      MN-MENS4008.
             05    FILLER          PIC      X(47)  VALUE
                   'TRANSACAO CANCELADA. ENTRAR PELO MENU PRINCIPAL'.

           03      MN-MENS4099.
             05    FILLER          PIC      X(61)  VALUE
                   'PROBLEMAS NO COMPUTADOR CENTRAL. CONTATE ANALISTA RE
      -            'SPONSAVEL'.

           03      FILLER          PIC      X(16)  VALUE 'BOOKS MAPA'.

      ******************************************************************
      *                      CAMPOS AUXILIARES                         *
      ******************************************************************
       01          FILLER.
           03      FILLER          PIC      X(08)  VALUE 'INIC-WRK'.

           03      WS-ABEND        PIC      X(04)  VALUE  SPACES.

           03      WS-ABSTIME      PIC     S9(15)  VALUE +0      COMP-3.

           03      WS-MENS-CSMT    PIC      X(200) VALUE  SPACES.
           03      WS-MENS-CSMT-1  PIC      X(200) VALUE  SPACES.
           03      WS-MENS-CSMT-2  PIC      X(200) VALUE  SPACES.
      *
           03      WS-DATA-API         PIC 9(08).
           03      FILLER   REDEFINES  WS-DATA-API.
             05    WS-ANO-API          PIC 9(04).
             05    WS-MES-API          PIC 9(02).
             05    WS-DIA-API          PIC 9(02).
      *
           03      WS-PERIODO-I        PIC 9(08).
           03      WS-PERIODO-F        PIC 9(08).
      *
           COPY        GEC2MS.
      ******************************************************************
      *                         BOOKS CICS                             *
      ******************************************************************
       01          FILLER.
           03      FILLER          PIC      X(16)  VALUE 'BOOKS CICS'.

      *----------------------------------------------------------------*
      *                 ATRIBUTOS  PARA  O  BMS                        *
      *----------------------------------------------------------------*

       01          DFHATTRB.

          03       DFHASBRT            PIC   X(001)   VALUE   '8'.
      *--         (ASKIP,BRT)

          03       DFHASKFS            PIC   X(001)   VALUE   '1'.
      *--         (ASKIP,FSET)

          03       DFHASBFS            PIC   X(001)   VALUE   '9'.
      *--         (ASKIP,BRT,FSET)

          03       DFHPROFS            PIC   X(001)   VALUE   '/'.
      *--         (PROT,FSET)

          03       DFHPROFB            PIC   X(001)   VALUE   'Z'.
      *--         (PROT,FSET,BRT)

          03       DFHUNFDN            PIC   X(001)   VALUE   ')'.
      *--         (UNPROT,FSET,DARK,NUM)

          03       DFHUNFSD            PIC   X(001)   VALUE   '('.
      *--         (UNPROT,FSET,DRK)

          03       DFHUNFBN            PIC   X(001)   VALUE   'R'.
      *--         (UNPROT,FSET,BRT,NUM)

          03       DFHUNFSB            PIC   X(001)   VALUE   'I'.
      *--         (UNPROT,FSET,BRT)

          03       DFHUNFNN            PIC   X(001)   VALUE   'J'.
      *--         (UNPROT,FSET,NORM,NUM)

          03       DFHUNFSN            PIC   X(001)   VALUE   'A'.
      *--         (UNPROT,FSET,NORM)

      *----------------------------------------------------------------*

           COPY        DFHAID.
      *
           COPY        GXXXEWR1.
      *
           COPY        YVALXWR1.
      *
       LINKAGE                     SECTION.
      ******************************************************************
      *                       LINKAGE                                  *
      ******************************************************************

       01          DFHCOMMAREA         PIC   X(200).

      ******************************************************************
      *            PROCEDURE       DIVISION                            *
      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
      *                      LOGICA PRINCIPAL                          *
      ******************************************************************
       LP-LOGICA-PRINCIPAL         SECTION.

           EXEC    CICS    HANDLE  ABEND
                           LABEL (ZA-TRATA-ABEND)
           END-EXEC

           IF      EIBCALEN        EQUAL   ZEROS
                   MOVE    MN-MENS4008    TO     T1MENSAO
                   PERFORM WB-ENVIA-MENSAGEM
           ELSE
                   MOVE DFHCOMMAREA TO GXXXEWK1
           END-IF

           IF      GXXXE-PASSO     EQUAL  LT-00
                   PERFORM B-TRATA-PRIMEIRA-VEZ
           ELSE
                   PERFORM C-TRATA-DEMAIS-VEZES
           END-IF.

       LP-LOGICA-PRINCIPAL-FIM.
           GOBACK.
₢
      ******************************************************************
      *                      OBTER DATA E HORA                         *
      ******************************************************************
       A-OBTEM-DATA-HORA           SECTION.

           EXEC    CICS    ASKTIME
                           ABSTIME (WS-ABSTIME)
                           NOHANDLE
           END-EXEC

           IF      EIBRESP                 NOT    EQUAL  ZEROS
                   MOVE    LT-ASKTIME      TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    1               TO     MN-PTO-ERRO-1
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1
                   PERFORM ZB-TRATA-ERRO.

           EXEC    CICS    FORMATTIME
                           ABSTIME  (WS-ABSTIME)
                           DDMMYY (T1DATAO)
                           DATESEP  ('/')
                           TIME     (T1HORAO)
                           TIMESEP  (':')
                           NOHANDLE
           END-EXEC

           IF      EIBRESP                 NOT    EQUAL  ZEROS
                   MOVE    LT-FORMATTIME   TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    2               TO     MN-PTO-ERRO-1
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1
                   PERFORM ZB-TRATA-ERRO.

       A-OBTEM-DATA-HORA-FIM.
           EXIT.

      ******************************************************************
      *                    TRATAR PRIMEIRA VEZ                         *
      ******************************************************************
       B-TRATA-PRIMEIRA-VEZ        SECTION.

           MOVE    LOW-VALUES              TO     GEC2M1I

           IF       GXXXE-OPCAO   EQUAL     LT-01
                    MOVE   'Empresa'        TO     T1LOGATO
           ELSE
             IF     GXXXE-OPCAO   EQUAL     LT-02
                    MOVE   'Região'         TO     T1LOGATO
             ELSE
               IF   GXXXE-OPCAO   EQUAL     LT-03
                    MOVE   'Fase'           TO     T1LOGATO
               ELSE
                 IF GXXXE-OPCAO   EQUAL     LT-04
                    MOVE   'Escritório'     TO     T1LOGATO
                 ELSE
                    MOVE   'NAO PREVISTO'   TO     T1LOGATO
                 END-IF
               END-IF
             END-IF
           END-IF.

           MOVE    -1                      TO     T1PERIDL

           MOVE    LT-01                   TO     GXXXE-PASSO

           MOVE    MN-MENS4004    TO     T1MENSAO

           PERFORM WAB-ENVIA-TELA-ERASE.

       B-TRATA-PRIMEIRA-VEZ-FIM.
           EXIT.

      ******************************************************************
      *                                                                *
      ******************************************************************
       C-TRATA-DEMAIS-VEZES        SECTION.

           PERFORM RA-RECEBE-TELA.

           IF       EIBAID  EQUAL  DFHENTER
                    PERFORM CA-TRATA-ENTER
           ELSE
             IF     EIBAID  EQUAL  DFHCLEAR OR DFHPF12
                    MOVE    MN-MENS4005    TO     T1MENSAO
                    PERFORM WB-ENVIA-MENSAGEM
             ELSE
               IF   EIBAID  EQUAL  DFHPF3
                    PERFORM CB-TRATA-PF3
               ELSE
                 IF EIBAID EQUAL  DFHPF4
                    PERFORM CC-TRATA-PF4
                 ELSE
                    MOVE    -1             TO     T1PERIDL
                    MOVE    MN-MENS4006    TO     T1MENSAO
                    PERFORM WA-ENVIA-TELA
                 END-IF
               END-IF
             END-IF
           END-IF.

       C-TRATA-DEMAIS-VEZES-FIM.
           EXIT.

       CA-TRATA-ENTER              SECTION.
      ******************************************************************
      * TRATA A TECLA ENTER                                            *
      ******************************************************************

             MOVE  LT-NAO       TO     FL-ERRO

             MOVE  LT-SIM       TO     FL-PER-I-OK
                                       FL-PER-F-OK

             MOVE  DFHUNFSN            TO   T1CAMPOA
                                            T1TIPOEA
                                            T1USUARA
                                            T1PERFAA
                                            T1PERFMA
                                            T1PERFDA
                                            T1PERIAA
                                            T1PERIMA
                                            T1PERIDA

             IF    T1CAMPOI     EQUAL  SPACES     OR
                   T1CAMPOI     EQUAL  LOW-VALUES OR
                   T1CAMPOI     EQUAL  HIGH-VALUES
                   MOVE    -1   TO     T1CAMPOL
                   MOVE    DFHUNFSB    TO   T1CAMPOA
                   MOVE    LT-SIM      TO   FL-ERRO
             END-IF

             IF    T1TIPOEI     EQUAL  SPACES     OR
                   T1TIPOEI     EQUAL  LOW-VALUES OR
                   T1TIPOEI     EQUAL  HIGH-VALUES
                   MOVE    -1   TO     T1TIPOEL
                   MOVE    DFHUNFSB    TO   T1TIPOEA
                   MOVE    LT-SIM      TO   FL-ERRO
             END-IF

             IF    T1USUARI     EQUAL  SPACES     OR
                   T1USUARI     EQUAL  LOW-VALUES OR
                   T1USUARI     EQUAL  HIGH-VALUES
                   MOVE    -1          TO   T1USUARL
                   MOVE    DFHUNFSB    TO   T1USUARA
                   MOVE    LT-SIM      TO   FL-ERRO
             END-IF

             IF    T1PERFAI     NOT NUMERIC OR
                   T1PERFAI     NOT GREATER ZEROS
                   MOVE   -1     TO   T1PERFAL
                   MOVE    DFHUNFBN    TO   T1PERFAA
                   MOVE    LT-SIM      TO   FL-ERRO
                   MOVE    LT-NAO      TO   FL-PER-F-OK
             END-IF

             IF    T1PERFMI     NOT NUMERIC OR
                   T1PERFMI     NOT GREATER ZEROS
                   MOVE   -1     TO   T1PERFML
                   MOVE    DFHUNFBN    TO   T1PERFMA
                   MOVE    LT-SIM      TO   FL-ERRO
                   MOVE    LT-NAO      TO   FL-PER-F-OK
             END-IF

             IF    T1PERFDI     NOT NUMERIC OR
                   T1PERFDI     NOT GREATER ZEROS
                   MOVE   -1     TO   T1PERFDL
                   MOVE    DFHUNFBN    TO   T1PERFDA
                   MOVE    LT-SIM      TO   FL-ERRO
                   MOVE    LT-NAO      TO   FL-PER-F-OK
             END-IF

             IF    FL-PER-F-OK  EQUAL  LT-SIM
                   MOVE   T1PERFAI     TO   WS-ANO-API
                   MOVE   T1PERFMI     TO   WS-MES-API
                   MOVE   T1PERFDI     TO   WS-DIA-API
                   PERFORM YA-ACESSA-YUROVAL
                   IF   YXRBVAL-COD-RETORNO NOT EQUAL ZEROS
                        MOVE   -1     TO   T1PERFDL
                        MOVE    DFHUNFBN    TO   T1PERFDA
                                                 T1PERFMA
                                                 T1PERFAA
                        MOVE    LT-SIM      TO   FL-ERRO
                        MOVE    LT-NAO      TO   FL-PER-F-OK
                   ELSE
                        MOVE    WS-DATA-API TO   WS-PERIODO-F

                   END-IF
             END-IF

             IF    T1PERIAI     NOT NUMERIC OR
                   T1PERIAI     NOT GREATER ZEROS
                   MOVE   -1     TO   T1PERIAL
                   MOVE    DFHUNFBN    TO   T1PERIAA
                   MOVE    LT-SIM      TO   FL-ERRO
                   MOVE    LT-NAO      TO   FL-PER-F-OK
             END-IF

             IF    T1PERIMI     NOT NUMERIC OR
                   T1PERIMI     NOT GREATER ZEROS
                   MOVE   -1     TO   T1PERIML
                   MOVE    DFHUNFBN    TO   T1PERIMA
                   MOVE    LT-SIM      TO   FL-ERRO
                   MOVE    LT-NAO      TO   FL-PER-F-OK
             END-IF

             IF    T1PERIDI     NOT NUMERIC OR
                   T1PERIDI     NOT GREATER ZEROS
                   MOVE  -1     TO   T1PERIDL
                   MOVE    DFHUNFBN    TO   T1PERIDA
                   MOVE    LT-SIM      TO   FL-ERRO
                   MOVE    LT-NAO      TO   FL-PER-F-OK
             END-IF.

             IF    FL-PER-I-OK  EQUAL  LT-SIM
                   MOVE   T1PERIAI     TO   WS-ANO-API
                   MOVE   T1PERIMI     TO   WS-MES-API
                   MOVE   T1PERIDI     TO   WS-DIA-API
                   PERFORM YA-ACESSA-YUROVAL
                   IF   YXRBVAL-COD-RETORNO NOT EQUAL ZEROS
                        MOVE   -1     TO   T1PERIDL
                        MOVE    DFHUNFBN    TO   T1PERIDA
                                                 T1PERIMA
                                                 T1PERIAA
                        MOVE    LT-SIM      TO   FL-ERRO
                        MOVE    LT-NAO      TO   FL-PER-I-OK
                   ELSE
                        MOVE    WS-DATA-API TO   WS-PERIODO-I
                   END-IF
             END-IF

             IF    FL-PER-I-OK  EQUAL   LT-SIM AND
                   FL-PER-F-OK  EQUAL   LT-SIM
                   IF   WS-PERIODO-F    LESS    WS-PERIODO-I
                        MOVE   -1     TO   T1PERIDL
                        MOVE    DFHUNFBN    TO   T1PERFDA T1PERIDA
                                                 T1PERFMA T1PERIMA
                                                 T1PERFAA T1PERIAA
                        MOVE    LT-SIM      TO   FL-ERRO
                   END-IF
             END-IF

             IF    FL-ERRO      EQUAL   LT-SIM
                   MOVE    MN-MENS4007    TO     T1MENSAO
                   PERFORM WA-ENVIA-TELA
             ELSE
                   PERFORM CAB-CHAMAR-PAGINACAO
             END-IF.

       CA-TRATA-ENTER-FIM.
           EXIT.

       CAB-CHAMAR-PAGINACAO         SECTION.
      ******************************************************************
      * TRATA O PGM PARA PAGINAR INFORMACOES                           *
      ******************************************************************

           MOVE    WS-PERIODO-I TO  GXXXE-PERIODO-INI
           MOVE    WS-PERIODO-F TO  GXXXE-PERIODO-FIM
           MOVE    T1USUARO     TO  GXXXE-USUARIO
           MOVE    T1TIPOEO     TO  GXXXE-TIPO-EVENTO
           MOVE    T1CAMPOO     TO  GXXXE-CAMPO

           MOVE    LT-GEC02     TO   GXXXE-PGM-CHAMADOR

           MOVE    LT-00        TO   GXXXE-PASSO

           EXEC    CICS XCTL PROGRAM   ('GEC03')
                             COMMAREA  (GXXXEWR1)
                             LENGTH    (LENGTH OF GXXXEWR1)
                             NOHANDLE
           END-EXEC.

           IF      EIBRESP                 NOT    EQUAL  ZEROS
                   MOVE    LT-XCTL         TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    3               TO     MN-PTO-ERRO-1
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1
                   PERFORM ZB-TRATA-ERRO.

       CAB-CHAMAR-PAGINACAO-FIM.
           EXIT.

       CB-TRATA-PF3                SECTION.
      ******************************************************************
      * TRATA A TECLA PF3                                              *
      ******************************************************************

           MOVE    LT-GEC02 TO        GXXXE-PGM-CHAMADOR

           MOVE    LT-00    TO        GXXXE-PASSO

           EXEC CICS XCTL PROGRAM   ('GEC01')
                          COMMAREA  (GXXXEWR1)
                          LENGTH    (LENGTH OF GXXXEWR1)
                          NOHANDLE
           END-EXEC.

           IF      EIBRESP                 NOT    EQUAL  ZEROS
                   MOVE    LT-XCTL         TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    4               TO     MN-PTO-ERRO-1
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1
                   PERFORM ZB-TRATA-ERRO.

       CB-TRATA-PF3-FIM.
           EXIT.

       CC-TRATA-PF4                SECTION.
      ******************************************************************
      * TRATA A TECLA PF4                                              *
      ******************************************************************

           MOVE    LT-00    TO        GXXXE-PASSO

           PERFORM B-TRATA-PRIMEIRA-VEZ.

       CC-TRATA-PF3-FIM.
           EXIT.

      ******************************************************************
      *                      RESTARTAR TRANSACAO                       *
      ******************************************************************
       D-RETURN-TRANSID            SECTION.

           EXEC    CICS    RETURN
                           TRANSID  ('GEC2')
                           COMMAREA (GXXXEWK1)
                           LENGTH   (LENGTH OF GXXXEWK1)
                           NOHANDLE
           END-EXEC

           IF      EIBRESP                 NOT    EQUAL  ZEROS
                   MOVE    LT-RETURN       TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    5               TO     MN-PTO-ERRO-1
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1
                   PERFORM ZB-TRATA-ERRO.

       D-RETURN-TRANSID-FIM.
           EXIT.

      ******************************************************************
      *                       ENCERRAR PGM                             *
      ******************************************************************
       E-ENCERRA-PGM               SECTION.

           PERFORM WB-ENVIA-MENSAGEM.

       E-ENCERRA-PGM-FIM.
           EXIT.

      ******************************************************************
      *                          RECEBER TELA                          *
      ******************************************************************
       RA-RECEBE-TELA              SECTION.

           EXEC    CICS    RECEIVE
                           MAP    ('GEC2M1')
                           MAPSET ('GEC2MS')
                           INTO   (GEC2M1O)
                           NOHANDLE
           END-EXEC

           IF      EIBRESP                 NOT    EQUAL (ZEROS AND +36)
                   MOVE    LT-RECEIVE      TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    6               TO     MN-PTO-ERRO-1
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1
                   PERFORM ZB-TRATA-ERRO.

       RA-RECEBE-TELA-FIM.
           EXIT.

      ******************************************************************
      *             TRATAR ENVIO DA TELA (ERASE OU DATAONLY)           *
      ******************************************************************
       WA-ENVIA-TELA               SECTION.

           IF      GXXXE-PASSO     EQUAL  0
                   PERFORM WAB-ENVIA-TELA-ERASE
           ELSE
                   PERFORM WAA-ENVIA-TELA-DATAONLY
           END-IF.

       WA-ENVIA-TELA-FIM.
           EXIT.

      ******************************************************************
      *                   ENVIAR TELA (COM DATAONLY)                   *
      ******************************************************************
       WAA-ENVIA-TELA-DATAONLY     SECTION.

           PERFORM A-OBTEM-DATA-HORA

           EXEC    CICS    SEND
                           MAP    ('GEC2M1')
                           MAPSET ('GEC2MS')
                           FROM   (GEC2M1O)
                           LENGTH (LENGTH OF GEC2M1O)
                           DATAONLY
                           CURSOR
                           FREEKB
                           NOHANDLE
           END-EXEC

           IF      EIBRESP                 NOT    EQUAL  ZEROS
                   MOVE    LT-SEND-DATAONLY
                                           TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    7               TO     MN-PTO-ERRO-1
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1
                   PERFORM ZB-TRATA-ERRO.

           PERFORM D-RETURN-TRANSID.

       WAA-ENVIA-TELA-DATAONLY-FIM.
           EXIT.

      ******************************************************************
      *                    ENVIAR TELA (COM ERASE)                     *
      ******************************************************************
       WAB-ENVIA-TELA-ERASE        SECTION.

           PERFORM A-OBTEM-DATA-HORA

           EXEC    CICS    SEND
                           MAP    ('GEC2M1')
                           MAPSET ('GEC2MS')
                           FROM   (GEC2M1O)
                           LENGTH (LENGTH OF GEC2M1O)
                           ERASE
                           CURSOR
                           FREEKB
                           NOHANDLE
           END-EXEC

           IF      EIBRESP                 NOT    EQUAL  ZEROS
                   MOVE    LT-SEND-ERASE   TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    8               TO     MN-PTO-ERRO-1
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1
                   PERFORM ZB-TRATA-ERRO.

           PERFORM D-RETURN-TRANSID.

       WAB-ENVIA-TELA-ERASE-FIM.
           EXIT.

      ******************************************************************
      *                      ENVIAR MENSAGEM                           *
      ******************************************************************
       WB-ENVIA-MENSAGEM           SECTION.

           EXEC    CICS    SEND
                           FROM   (T1MENSAO)
                           LENGTH (LENGTH OF T1MENSAO)
                           ERASE
                           NOHANDLE
           END-EXEC.

           EXEC    CICS    RETURN
           END-EXEC.

       WB-ENVIA-MENSAGEM-FIM.
           EXIT.

      ******************************************************************
      *                          GRAVAR CSMT                           *
      ******************************************************************
       WC-GRAVA-CSMT               SECTION.

           EXEC    CICS    WRITEQ  TD
                           QUEUE      ('CSMT')
                           FROM       (WS-MENS-CSMT)
                           LENGTH     (LENGTH OF WS-MENS-CSMT)
                           NOHANDLE
           END-EXEC.

       WC-GRAVA-CSMT-FIM.
           EXIT.

       YA-ACESSA-YUROVAL       SECTION.
      ******************************************************************
      *    ACESSA  YUROVAL - VALIDA DATA.
      ******************************************************************

           MOVE    WS-DATA-API         TO      YXRBVAL-PARM-ENTRADA

           EXEC    CICS    LINK
                           PROGRAM  (SR-YUROVAL)
                           COMMAREA (YBVALWK1)
                           LENGTH   (LENGTH OF YBVALWK1)
                           NOHANDLE
           END-EXEC

           IF      EIBRESP NOT EQUAL  ZEROS
                   MOVE    LT-LINK         TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    9               TO     MN-PTO-ERRO-1
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1
                   PERFORM ZB-TRATA-ERRO
           END-IF

           IF      YXRBVAL-COD-RETORNO  NOT EQUAL ZEROS AND 01 AND 02
                   MOVE   SR-YUROVAL           TO MN-API-3
                   MOVE   YXRBVAL-COD-RETORNO  TO MN-CODRET-3
                   MOVE    10                  TO MN-PTO-ERRO-3
                   MOVE   YXRBVAL-PARM-ENTRADA TO MN-CHAVE-3
                   MOVE   MN-MENS4003          TO WS-MENS-CSMT-1
                   PERFORM ZB-TRATA-ERRO
           END-IF.

       YA-ACESSA-YUROVAL-FIM.
           EXIT.

      ******************************************************************
      *                          TRATAR ABENDS                         *
      ******************************************************************
       ZA-TRATA-ABEND              SECTION.

           EXEC    CICS    HANDLE
                           ABEND
                           CANCEL
           END-EXEC

           EXEC    CICS    ASSIGN
                           ABCODE (WS-ABEND)
           END-EXEC

           IF      WS-ABEND                EQUAL  LT-ASRA
                   EXEC    CICS    ABEND
                                   ABCODE (LT-ASRA)
                   END-EXEC
           END-IF

           MOVE    WS-ABEND                TO     MN-ABEND-2
           MOVE    11                      TO     MN-PTO-ERRO-2
           MOVE    MN-MENS4002             TO     WS-MENS-CSMT-1

           PERFORM ZB-TRATA-ERRO.

       ZA-TRATA-ABEND-FIM.
           EXIT.

      ******************************************************************
      *                         TRATAR ERRO                            *
      ******************************************************************
       ZB-TRATA-ERRO               SECTION.

           EXEC CICS    SYNCPOINT
                        ROLLBACK
           END-EXEC

           EXEC    CICS    ASKTIME
                           ABSTIME (WS-ABSTIME)
                           NOHANDLE
           END-EXEC

           EXEC    CICS    FORMATTIME
                           ABSTIME  (WS-ABSTIME)
                           DDMMYY (MN-DATA-1)
                           DATESEP  ('/')
                           TIME     (MN-HORA-1)
                           TIMESEP  (':')
                           NOHANDLE
           END-EXEC
      *-*  DATA E HORA DO ABEND
           MOVE    MN-MENS4001B    TO     WS-MENS-CSMT
           PERFORM WC-GRAVA-CSMT

      *-*  TEXTO DO 1.A MENSAGEM DE ERRO
           MOVE    WS-MENS-CSMT-1  TO     WS-MENS-CSMT
           PERFORM WC-GRAVA-CSMT

      *-*  TEXTO DA 2.A MENSAGEM DE ERRO (SE NECESSARIO...)
           IF      WS-MENS-CSMT-2  NOT EQUAL SPACES
                   MOVE WS-MENS-CSMT-2 TO    WS-MENS-CSMT
                   PERFORM WC-GRAVA-CSMT
           END-IF

           MOVE    MN-MENS4099             TO     T1MENSAO

           PERFORM E-ENCERRA-PGM.

       ZB-TRATA-ERRO-FIM.
           EXIT.

      ********************* F I N A L   P G M   ************************
