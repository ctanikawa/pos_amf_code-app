       IDENTIFICATION              DIVISION.
       PROGRAM-ID.      GEC03.
       AUTHOR.          STEFANINI.
       DATE-WRITTEN.    MARCO/2008.
       SECURITY.

      ******************************************************************
      *                                                                *
      *              PAGINACAO DA PESQUISA - versão main               *
      *                                                                *
      ******************************************************************
      * TRANSACAO = GEC3                         TIPO PROGRAMOTECA: 53 *
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
      *                         INDEXADORES                            *
      ******************************************************************
       01          FILLER.
           03      FILLER          PIC      X(08)  VALUE 'SUBROTIN'.
           03      IN-IND          PIC      9(03)  VALUE ZEROS.
MACETE     03      IN-PAG          PIC      9(03)  VALUE ZEROS.

      ******************************************************************
      *                         FLAGS                                  *
      ******************************************************************
       01          FILLER.
           03      FILLER          PIC      X(08)  VALUE 'FLAGS'.
      ******************************************************************
      *                         LITERAIS                               *
      ******************************************************************
       01          FILLER.
           03      FILLER          PIC      X(08)  VALUE 'LITERAIS'.
           03      LT-00           PIC      9(02)  VALUE 00.
           03      LT-01           PIC      9(02)  VALUE 01.
           03      LT-ASRA         PIC      X(04)  VALUE 'ASRA'.
           03      LT-GEC03        PIC      X(08)  VALUE 'GEC03'.
           03      LT-ASKTIME      PIC      X(07)  VALUE 'ASKTIME'.
           03      LT-FORMATTIME   PIC      X(10)  VALUE 'FORMATTIME'.
           03      LT-RECEIVE      PIC      X(07)  VALUE 'RECEIVE'.
           03      LT-RETURN       PIC      X(06)  VALUE 'RETURN'.
           03      LT-XCTL         PIC      X(04)  VALUE 'XCTL'.
           03      LT-SEND-ERASE   PIC      X(10)  VALUE 'SEND ERASE'.
           03      LT-SEND-DATAONLY
                                   PIC      X(13)  VALUE
                                                   'SEND DATAONLY'.
           03      LT-WRITEQ-TS
                                   PIC      X(09)  VALUE
                                                   'WRITEQ TS'.
           03      LT-READQ-TS
                                   PIC      X(08)  VALUE
                                                   'READQ TS'.
           03      LT-DELETEQ-TS
                                   PIC      X(10)  VALUE
                                                   'DELETEQ TS'.
      ******************************************************************
      *                         MENSAGENS                              *
      ******************************************************************
       01          FILLER.
           03      FILLER          PIC      X(16)  VALUE 'MENSAGENS'.

           03      MN-MENS4001A.
             05    FILLER          PIC      X(41)  VALUE
                   'GE0001I GEC03 I4001 ERRO CICS - COMANDO: '.
             05    MN-COMANDO-1    PIC      X(13).
             05    FILLER          PIC      X(12)  VALUE
                   ' - EIBRESP: '.
             05    MN-EIBRESP-1    PIC      9(02).
             05    FILLER          PIC      X(08)  VALUE
                   ' - PTO: '.
             05    MN-PTO-ERRO-1   PIC      9(02).

           03      MN-MENS4001B.
             05    FILLER          PIC      X(26)  VALUE
                   'GE0001I GEC03 I4001 DATA: '.
             05    MN-DATA-1       PIC      X(10)  VALUE  SPACES.
             05    FILLER          PIC      X(09)  VALUE ' - HORA: '.
             05    MN-HORA-1       PIC      X(08)  VALUE  SPACES.

           03      MN-MENS4002.
             05    FILLER          PIC      X(20)  VALUE
                   'GE0002I GEC03 I4002 '.
             05    FILLER          PIC      X(29)  VALUE
                   'ABEND INTERCEPTADO - CODIGO: '.
             05    MN-ABEND-2      PIC      X(04).
             05    FILLER          PIC      X(08)  VALUE
                   ' - PTO: '.
             05    MN-PTO-ERRO-2   PIC      9(02).

XXXXXX     03      MN-MENS4003.
XXXXXX       05    FILLER          PIC      X(08)  VALUE
XXXXXX             ' PAGINA '.
XXXXXX       05    MN-PAGINA-03    PIC      9(03)  VALUE ZEROS.
XXXXXX       05    FILLER          PIC      X(01)  VALUE '/'.
XXXXXX       05    MN-QTDPAG-03    PIC      9(03)  VALUE ZEROS.

           03      MN-MENS4005.
             05    FILLER          PIC      X(19)  VALUE
                   'TRANSACAO ENCERRADA'.

           03      MN-MENS4006.
             05    FILLER          PIC      X(19)  VALUE
                   'TECLA INVALIDA     '.

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
           03      WS-QUEUE.
             05    FILLER          PIC      X(04)  VALUE 'GEC3'.
             05    WS-TERM         PIC      X(04)  VALUE  SPACES.
           03      WS-TS-ITEM      PIC     S9(04)  VALUE  +0 COMP.
      *
           COPY        GEC3MS.
      *
         01        TB-TELA REDEFINES GEC3M1I.
            03     FILLER                     PIC X(36).
            03     TB-ITEM   OCCURS   5   TIMES.
              05   TB-DATAL                   PIC S9(4) COMP.
              05   TB-DATAA                   PIC X.
              05   TB-DATA                    PIC X(10).
              05   TB-USUAL                   PIC S9(4) COMP.
              05   TB-USUAA                   PIC X.
              05   TB-USUA                    PIC X(8).
              05   TB-EVENL                   PIC S9(4) COMP.
              05   TB-EVENA                   PIC X.
              05   TB-EVEN                    PIC X(6).
              05   TB-ENTIL                   PIC S9(4) COMP.
              05   TB-ENTIA                   PIC X.
              05   TB-ENTI                    PIC X(10).
              05   TB-CAMPL                   PIC S9(4) COMP.
              05   TB-CAMPA                   PIC X.
              05   TB-CAMP                    PIC X(9).
              05   TB-ANTEL                   PIC S9(4) COMP.
              05   TB-ANTEA                   PIC X.
              05   TB-ANTE                    PIC X(20).
              05   TB-HORAL                   PIC S9(4) COMP.
              05   TB-HORAA                   PIC X.
              05   TB-HORA                    PIC X(5).
              05   TB-NOMEL                   PIC S9(4) COMP.
              05   TB-NOMEA                   PIC X.
              05   TB-NOME                    PIC X(20).
              05   TB-ATUAL                   PIC S9(4) COMP.
              05   TB-ATUAA                   PIC X.
              05   TB-ATUA                    PIC X(20).
            03     FILLER                     PIC X(77).
      *
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
       LINKAGE                     SECTION.
      ******************************************************************
      *                       LINKAGE                                  *
      ******************************************************************

       01          DFHCOMMAREA         PIC   X(300).

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
           END-IF.

           MOVE    DFHCOMMAREA     TO GXXXEWK1

           MOVE    EIBTRMID        TO     WS-TERM

           IF      GXXXE-PASSO     EQUAL  LT-00
                   PERFORM B-TRATA-PRIMEIRA-VEZ
           ELSE
                   PERFORM C-TRATA-DEMAIS-VEZES
           END-IF.

       LP-LOGICA-PRINCIPAL-FIM.
           GOBACK.

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

           MOVE    LOW-VALUES              TO     GEC3M1I

           MOVE    LT-01                   TO     GXXXE-PASSO

           MOVE    ZEROS                   TO     GXXXE-QTD-PAGINAS
                                                  GXXXE-PAG-ATU
      *-*  EMBUTIR NESTA ROTINA ACESSOS DB2...
           PERFORM BA-MONTA-TS
      *
           IF      GXXXE-QTD-PAGINAS GREATER ZEROS
                   MOVE     01     TO   GXXXE-PAG-ATU
                   PERFORM  RB-LER-TS
XXXXXX******IF     GXXXE-QTD-PAGINAS  EQUAL  1
XXXXXX******       MOVE   'PAGINA UNICA' TO T1MENSAO
XXXXXX******ELSE
XXXXXX             MOVE   GXXXE-PAG-ATU     TO  MN-PAGINA-03
XXXXXX             MOVE   GXXXE-QTD-PAGINAS TO  MN-QTDPAG-03
XXXXXX             MOVE   MN-MENS4003       TO  T1MENSAO
XXXXXX******END-IF
           ELSE
                   MOVE    'NAO EXISTEM INFORMACOES' TO T1MENSAO
           END-IF

           PERFORM WAB-ENVIA-TELA-ERASE.

       B-TRATA-PRIMEIRA-VEZ-FIM.
           EXIT.

      ******************************************************************
      *               MONTAGEM DA PAGINACAO                            *
      ******************************************************************
       BA-MONTA-TS                 SECTION.
      *-* MACETE-INI PARA FORMATAR PAGINAS...

           MOVE    01   TO   IN-IND
                             IN-PAG
           PERFORM UNTIL IN-PAG GREATER 500

               PERFORM UNTIL IN-IND GREATER 5
                  MOVE    '04/03/2008'        TO TB-DATA (IN-IND)
                  MOVE    'P100002'           TO TB-USUA (IN-IND)
                  MOVE    IN-IND              TO TB-EVEN (IN-IND)
                  MOVE    IN-PAG              TO TB-ENTI (IN-IND)
                  MOVE    'CAMPOXX'           TO TB-CAMP (IN-IND)
                  MOVE    'ANTERIORXX'        TO TB-ANTE (IN-IND)
                  MOVE    '13:15'             TO TB-HORA (IN-IND)
                  MOVE    'FULANO DE TAL'     TO TB-NOME (IN-IND)
                  MOVE    'ATUALXX'           TO TB-ATUA (IN-IND)
                  ADD     01                  TO IN-IND
                END-PERFORM

              PERFORM     WD-GRAVA-TS

              MOVE        LOW-VALUES         TO TB-TELA
              ADD   01    TO  IN-PAG
              MOVE  01    TO  IN-IND

           END-PERFORM.

      *-* MACETE-FIM PARA FORMATAR PAGINAS...

       BA-MONTA-TS-FIM.
           EXIT.
      ******************************************************************
      *                                                                *
      ******************************************************************
       C-TRATA-DEMAIS-VEZES        SECTION.

           PERFORM RA-RECEBE-TELA.

           IF     EIBAID  EQUAL  DFHCLEAR OR DFHPF12
                  PERFORM WE-DELETA-TS
                  MOVE    MN-MENS4005    TO     T1MENSAO
                  PERFORM WB-ENVIA-MENSAGEM
           END-IF.

           IF     EIBAID EQUAL  DFHPF1
                  PERFORM WE-DELETA-TS
                  PERFORM CA-TRATA-PF1
           END-IF.

           IF     EIBAID EQUAL  DFHPF3
                  PERFORM WE-DELETA-TS
                  PERFORM CB-TRATA-PF3
           END-IF.

           IF      EIBAID EQUAL  DFHPF7
                   PERFORM CC-TRATA-PF7
           END-IF.

           IF      EIBAID EQUAL  DFHPF8
                   PERFORM CD-TRATA-PF8
           END-IF.

           MOVE    -1             TO     T1DATAL
           MOVE    MN-MENS4006    TO     T1MENSAO
           PERFORM WA-ENVIA-TELA.

       C-TRATA-DEMAIS-VEZES-FIM.
           EXIT.

       CA-TRATA-PF1                SECTION.
      ******************************************************************
      * TRATA A TECLA PF1                                              *
      ******************************************************************

           MOVE   LT-GEC03 TO        GXXXE-PGM-CHAMADOR

           MOVE    LT-00    TO        GXXXE-PASSO

           EXEC CICS XCTL PROGRAM   ('GEC02')
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

       CA-TRATA-PF1-FIM.
           EXIT.

       CB-TRATA-PF3                SECTION.
      ******************************************************************
      * TRATA A TECLA PF3                                              *
      ******************************************************************

           MOVE   LT-GEC03 TO        GXXXE-PGM-CHAMADOR

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

       CC-TRATA-PF7                SECTION.
      ******************************************************************
      * TRATA A TECLA PF7                                              *
      ******************************************************************

           IF      GXXXE-PAG-ATU  GREATER   1
                   SUBTRACT 01  FROM GXXXE-PAG-ATU
                   PERFORM   RB-LER-TS
           END-IF.

           MOVE    ' '       TO  T1MENSAO

           IF      GXXXE-QTD-PAGINAS EQUAL ZEROS
                   MOVE  'NAO EXISTEM INFORMACOES' TO T1MENSAO
           ELSE
XXXXXX*******IF    GXXXE-QTD-PAGINAS EQUAL 1
XXXXXX*******      MOVE  'PAGINA UNICA'  TO T1MENSAO
XXXXXX*******ELSE
XXXXXX*******  IF  GXXXE-PAG-ATU EQUAL 1
XXXXXX*******      MOVE  'PRIMEIRA PAGINA' TO T1MENSAO
XXXXXX*******  ELSE
XXXXXX             MOVE   GXXXE-PAG-ATU     TO  MN-PAGINA-03
XXXXXX             MOVE   GXXXE-QTD-PAGINAS TO  MN-QTDPAG-03
XXXXXX             MOVE   MN-MENS4003       TO  T1MENSAO
XXXXXX*******  END-IF
XXXXXX*******END-IF
           END-IF.

           PERFORM   WA-ENVIA-TELA.

       CC-TRATA-PF7-FIM.
           EXIT.

       CD-TRATA-PF8                SECTION.
      ******************************************************************
      * TRATA A TECLA PF8                                              *
      ******************************************************************

           IF      GXXXE-PAG-ATU  LESS  GXXXE-QTD-PAGINAS
                   ADD   01  TO  GXXXE-PAG-ATU
                   PERFORM   RB-LER-TS
           END-IF.

           MOVE    ' '       TO  T1MENSAO

           IF      GXXXE-QTD-PAGINAS EQUAL ZEROS
                   MOVE  'NAO EXISTEM INFORMACOES' TO T1MENSAO
           ELSE
XXXXXX*******IF    GXXXE-QTD-PAGINAS EQUAL 1
XXXXXX*******      MOVE  'PAGINA UNICA'  TO T1MENSAO
XXXXXX*******ELSE
XXXXXX*******  IF  GXXXE-PAG-ATU EQUAL GXXXE-QTD-PAGINAS
XXXXXX*******      MOVE  'ULTIMA PAGINA' TO T1MENSAO
XXXXXX*******  ELSE
XXXXXX             MOVE   GXXXE-PAG-ATU     TO  MN-PAGINA-03
XXXXXX             MOVE   GXXXE-QTD-PAGINAS TO  MN-QTDPAG-03
XXXXXX             MOVE   MN-MENS4003       TO  T1MENSAO
XXXXXX*******  END-IF
XXXXXX*******END-IF
           END-IF.

           PERFORM   WA-ENVIA-TELA.

       CD-TRATA-PF8-FIM.
           EXIT.

      ******************************************************************
      *                      RESTARTAR TRANSACAO                       *
      ******************************************************************
       D-RETURN-TRANSID            SECTION.

           EXEC    CICS    RETURN
                           TRANSID  ('GEC3')
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
                           MAP    ('GEC3M1')
                           MAPSET ('GEC3MS')
                           INTO   (GEC3M1O)
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
      *                          RECEBER TELA                          *
      ******************************************************************
       RB-LER-TS                   SECTION.

           MOVE    GXXXE-PAG-ATU    TO   WS-TS-ITEM

           EXEC    CICS    READQ    TS
                           QUEUE   (WS-QUEUE)
                           INTO    (TB-TELA)
                           LENGTH  (LENGTH OF TB-TELA)
                           ITEM    (WS-TS-ITEM)
                           NOHANDLE
           END-EXEC.

           IF      EIBRESP NOT EQUAL  ZEROS
                   MOVE    LT-READQ-TS     TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    7               TO     MN-PTO-ERRO-1
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1
                   PERFORM ZB-TRATA-ERRO
           END-IF.

       RB-LER-TS-FIM.
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
                           MAP    ('GEC3M1')
                           MAPSET ('GEC3MS')
                           FROM   (GEC3M1O)
                           LENGTH (LENGTH OF GEC3M1O)
                           DATAONLY
                           CURSOR
                           FREEKB
                           NOHANDLE
           END-EXEC

           IF      EIBRESP                 NOT    EQUAL  ZEROS
                   MOVE    LT-SEND-DATAONLY
                                           TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    8               TO     MN-PTO-ERRO-1
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
                           MAP    ('GEC3M1')
                           MAPSET ('GEC3MS')
                           FROM   (GEC3M1O)
                           LENGTH (LENGTH OF GEC3M1O)
                           ERASE
                           CURSOR
                           FREEKB
                           NOHANDLE
           END-EXEC

           IF      EIBRESP                 NOT    EQUAL  ZEROS
                   MOVE    LT-SEND-ERASE   TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    9               TO     MN-PTO-ERRO-1
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

      ******************************************************************
      *                          GRAVAR TS                             *
      ******************************************************************
       WD-GRAVA-TS                 SECTION.

           ADD     01                     TO     GXXXE-QTD-PAGINAS

           MOVE    GXXXE-QTD-PAGINAS      TO     WS-TS-ITEM

           EXEC    CICS    WRITEQ  TS
                           QUEUE  (WS-QUEUE)
                           FROM   (TB-TELA)
                           LENGTH (LENGTH OF TB-TELA)
                           ITEM   (WS-TS-ITEM)
                           NOHANDLE
           END-EXEC

           IF      EIBRESP NOT EQUAL  ZEROS
                   MOVE    LT-WRITEQ-TS    TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    10              TO     MN-PTO-ERRO-1
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1
                   PERFORM ZB-TRATA-ERRO
           END-IF.

       WD-GRAVA-TS-FIM.
           EXIT.

      ******************************************************************
      *                          DELETE TS                             *
      ******************************************************************
       WE-DELETA-TS                SECTION.

           EXEC    CICS    DELETEQ  TS
                           QUEUE   (WS-QUEUE)
                           NOHANDLE
           END-EXEC

           IF      EIBRESP NOT EQUAL  ZEROS
                   MOVE    LT-DELETEQ-TS   TO     MN-COMANDO-1
                   MOVE    EIBRESP         TO     MN-EIBRESP-1
                   MOVE    11              TO     MN-PTO-ERRO-1
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1
                   PERFORM ZB-TRATA-ERRO
           END-IF.

       WE-DELETA-TS-FIM.
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
           MOVE    12                      TO     MN-PTO-ERRO-2
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
