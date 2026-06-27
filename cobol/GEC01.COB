       IDENTIFICATION              DIVISION.                                    
       PROGRAM-ID.      GEC01.                                                  
       AUTHOR.          STEFANINI.                                              
       DATE-WRITTEN.    MARCO/2008.                                             
       SECURITY. 
      ******************************************************************
      * gerei esta diferenca para aprender a retira-las                *
      ******************************************************************        
      ***  editado no vscode                                       ***         
      ******************************************************************        
      *                                                                *        
      *                     M E N U  - versao main                     *        
      *                                                                *        
      ******************************************************************        
      * TRANSACAO = GEC1                         TIPO PROGRAMOTECA: 53 *        
      ******************************************************************        
      * NOME DO PROGRAMADOR + NOME DO ANALISTA +     DATA     + VERSAO *        
      * CARLOS TANIKAWA     + CELIA SATOMI     +  MARCO/08    +        *        
      ******************************************************************        
                                                                                
      ******************************************************************        
      *            ENVIRONMENT     DIVISION - alterei aqui tb          *        
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
      *                         LITERAIS                               *        
      ******************************************************************        
       01          FILLER.                                                      
           03      FILLER          PIC      X(gg)  VALUE 'LITERAIS'.            
           03      LT-00           PIC      9(02)  VALUE 00.                    
           03      LT-01           PIC      9(02)  VALUE 01.                    
           03      LT-02           PIC      9(02)  VALUE 02.                    
           03      LT-03           PIC      9(02)  VALUE 03.                    
           03      LT-04           PIC      9(02)  VALUE 04.                    
           03      LT-GEC01        PIC      X(08)  VALUE 'GEC01'.               
           03      LT-ASRA         PIC      X(04)  VALUE 'ASRA'.                
           03      LT-ASKTIME      PIC      X(07)  VALUE 'ASKTIME'.             
           03      LT-FORMATTIME   PIC      X(10)  VALUE 'FORMATTIME'.          
           03      LT-RECEIVE      PIC      X(07)  VALUE 'RECEIVE'.             
           03      LT-RETURN       PIC      X(06)  VALUE 'RETURN'.              
           03      LT-XCTL         PIC      X(06)  VALUE 'XCTL'.                
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
                   'GE0001I GEC01 I4001 ERRO CICS - COMANDO: '.                 
             05    MN-COMANDO-1    PIC      X(13).                              
             05    FILLER          PIC      X(12)  VALUE                        
                   ' - EIBRESP: '.                                              
             05    MN-EIBRESP-1    PIC      9(02).                              
             05    FILLER          PIC      X(08)  VALUE                        
                   ' - PTO: '.                                                  
             05    MN-PTO-ERRO-1   PIC      9(02).                              
                                                                                
           03      MN-MENS4001B.                                                
             05    FILLER          PIC      X(26)  VALUE                        
                   'GE0001I GEC01 I4001 DATA: '.                                
             05    MN-DATA-1       PIC      X(10)  VALUE  SPACES.               
             05    FILLER          PIC      X(09)  VALUE ' - HORA: '.           
             05    MN-HORA-1       PIC      X(08)  VALUE  SPACES.               
                                                                                
           03      MN-MENS4002.                                                 
             05    FILLER          PIC      X(20)  VALUE                        
                   'GE0002I GEC01 I4002 '.                                      
             05    FILLER          PIC      X(29)  VALUE                        
                   'ABEND INTERCEPTADO - CODIGO: '.                             
             05    MN-ABEND-2      PIC      X(04).                              
             05    FILLER          PIC      X(08)  VALUE                        
                   ' - PTO: '.                                                  
             05    MN-PTO-ERRO-2   PIC      9(02).                              
                                                                                
           03      MN-MENS4003.     
                                                       
             05    FILLER          PIC      X(29)  VALUE                        
                   'GE0003I GEC01 I4003 ERRO API '.                             
             05    MN-API-3        PIC      X(07).                              
             05    FILLER          PIC      X(13)  VALUE                        
                   ' - COD.RET.: '.                                             
             05    MN-CODRET-3     PIC      9(02).                              
             05    FILLER          PIC      X(gg)  VALUE ' - PTO: '.            
             05    MN-PTO-ERRO-3   PIC      9(02).                              
             05    FILLER          PIC      X(05)  VALUE ' CHV='.               
             05    MN-CHAVE-3      PIC      X(150).                             
                                                                                
           03      MN-MENS4004.                                                 
             05    FILLER          PIC      X(46)  VALUE                        
                   'DIGITE A OPCAO DESEJADA E TECLE ENTER         '.            
                                                                                
           03      MN-MENS4005.                                                 
             05    FILLER          PIC      X(19)  VALUE                        
                   'TRANSACAO ENCERRADA'.                                       
                                                                                
           03      MN-MENS4006.                                                 
             05    FILLER          PIC      X(19)  VALUE                        
                   'TECLA INVALIDA     '.                                       
                                                                                
           03      MN-MENS4007.                                                 
             05    FILLER          PIC      X(14)  VALUE                        
                   'OPCAO INVALIDA'.                                            
                                                                                
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
           03      WS-MENS-CSMT-1  PIC      X(ggg) VALUE  SPACES.               
           03      WS-MENS-CSMT-2  PIC      X(200) VALUE  SPACES.               
                                                                                
           COPY        GEC1MS.                                                  
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
                   INITIALIZE        GXXXEWR1                                   
                   MOVE  LT-00       TO    GXXXE-PASSO                          
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
                                                                                
           MOVE    LOW-VALUES              TO     GEC1M1I                       
                                                                                
           MOVE    -1                      TO     T1OPCAOL                      
                                                                                
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
                                                                                
           IF      EIBAID  EQUAL  DFHENTER                                      
                   PERFORM CA-TRATA-ENTER                                       
           ELSE                                                                 
              IF   EIBAID  EQUAL  DFHCLEAR OR DFHPF12                           
                   MOVE    MN-MENS4005    TO     T1MENSAO                       
                   PERFORM WB-ENVIA-MENSAGEM                                    
              ELSE                                                              
                   MOVE    -1             TO     T1OPCAOL                       
                   MOVE    MN-MENS4006    TO     T1MENSAO                       
                   PERFORM WA-ENVIA-TELA                                        
              END-IF                                                            
           END-IF.                                                              
                                                                                
       C-TRATA-DEMAIS-VEZES-FIM.                                                
           EXIT.                                                                
           IF      EIBAID  EQUAL  DFHENTER                                      
                   PERFORM CA-TRATA-ENTER                                       
           ELSE                                                                 
              IF   EIBAID  EQUAL  DFHCLEAR OR DFHPF12                           
                   MOVE    MN-MENS4005    TO     T1MENSAO                       
                   PERFORM WB-ENVIA-MENSAGEM                                    
              ELSE                                                              
                   MOVE    -1             TO     T1OPCAOL                       
                   MOVE    MN-MENS4006    TO     T1MENSAO                       
                   PERFORM WA-ENVIA-TELA                                        
              END-IF                                                            
           END-IF.                                                              
       CA-TRATA-ENTER              SECTION.                                     
      ******************************************************************        
      * TRATA A TECLA ENTER                                            *        
      ******************************************************************        
                                                                                
           IF      T1OPCAOO   NOT EQUAL  LT-01 AND LT-02 AND                    
                                         LT-03 AND LT-04                        
                   MOVE    -1            TO     T1OPCAOL                        
                   MOVE    MN-MENS4007   TO     T1MENSAO                        
                   PERFORM WA-ENVIA-TELA                                        
           END-IF.                                                              
                                                                                
           PERFORM CAA-CHAMAR-OPCOES.                                           
                                                                                
       CA-TRATA-ENTER-FIM.                                                      
           EXIT.                                                                
                                                                                
       CAA-CHAMAR-OPCOES           SECTION.                                     
      ******************************************************************        
      * CHAMAR PROGRAMA DE ACORDO COM OPCAO DA TELA ESCOLHIDA          *        
      ******************************************************************        
                                                                                
           MOVE    LT-GEC01        TO  GXXXE-PGM-CHAMADOR                       
                                                                                
           MOVE    LT-00           TO  GXXXE-PASSO                              
                                                                                
           MOVE    T1OPCAOI        TO  GXXXE-OPCAO                              
                                                                                
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
                                                                                
       CAA-CHAMAR-OPCOES-FIM.                                                   
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *                      RESTARTAR TRANSACAO                       *        
      ******************************************************************        
       D-RETURN-TRANSID            SECTION.                                     
                                                                                
           EXEC    CICS    RETURN                                               
                           TRANSID  ('GEC1')                                    
                           COMMAREA (GXXXEWK1)                                  
                           LENGTH   (LENGTH OF GXXXEWK1)                        
                           NOHANDLE                                             
           END-EXEC                                                             
                                                                                
           IF      EIBRESP                 NOT    EQUAL  ZEROS                  
                   MOVE    LT-RETURN       TO     MN-COMANDO-1                  
                   MOVE    EIBRESP         TO     MN-EIBRESP-1                  
                   MOVE    4               TO     MN-PTO-ERRO-1                 
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1                
                   PERFORM ZB-TRATA-ERRO.                                       
                                                                                
       D-RETURN-TRANSID-FIM.                                                    
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *                       ENCERRAR PGM                             *        
      ******************************************************************        
       E-ENCERRA-PGM               SECTION.                                     
                                                                                
           PERFORM WB-ENVIA-MENSAGEM                                            
                                                                                
           EXEC    CICS    RETURN                                               
           END-EXEC.                                                            
                                                                                
       E-ENCERRA-PGM-FIM.                                                       
           EXIT.                                                                
                                                                                
      ******************************************************************        
      *                          RECEBER TELA                          *        
      ******************************************************************        
       RA-RECEBE-TELA              SECTION.                                     
                                                                                
           EXEC    CICS    RECEIVE                                              
                           MAP    ('GEC1M1')                                    
                           MAPSET ('GEC1MS')                                    
                           INTO   (GEC1M1O)                                     
                           NOHANDLE                                             
           END-EXEC                                                             
                                                                                
           IF      EIBRESP                 NOT    EQUAL (ZEROS AND +36)         
                   MOVE    LT-RECEIVE      TO     MN-COMANDO-1                  
                   MOVE    EIBRESP         TO     MN-EIBRESP-1                  
                   MOVE    5               TO     MN-PTO-ERRO-1                 
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
                           MAP    ('GEC1M1')                                    
                           MAPSET ('GEC1MS')                                    
                           FROM   (GEC1M1O)                                     
                           LENGTH (LENGTH OF GEC1M1O)                           
                           DATAONLY                                             
                           CURSOR                                               
                           FREEKB                                               
                           NOHANDLE                                             
           END-EXEC                                                             
                                                                                
           IF      EIBRESP                 NOT    EQUAL  ZEROS                  
                   MOVE    LT-SEND-DATAONLY                                     
                                           TO     MN-COMANDO-1                  
                   MOVE    EIBRESP         TO     MN-EIBRESP-1                  
                   MOVE    6               TO     MN-PTO-ERRO-1                 
                   MOVE    MN-MENS4001A    TO     WS-MENS-CSMT-1                
                   PERFORM ZB-TRATA-ERRO.                                       
                                                                                
           PERFORM D-RETURN-TRANSID.                                            
                                                                                
       WAA-ENVIA-TELA-DATAONLY-FIM.                                             
           EXIT.                                                                
                                                                                
      *****************************************************************         
      *                    ENVIAR TELA (COM ERASE)                     *        
      ******************************************************************        
       WAB-ENVIA-TELA-ERASE        SECTION.                                     
                                                                                
           PERFORM A-OBTEM-DATA-HORA                                            
                                                                                
           EXEC    CICS    SEND                                                 
                           MAP    ('GEC1M1')                                    
                           MAPSET ('GEC1MS')                                    
                           FROM   (GEC1M1O)                                     
                           LENGTH (LENGTH OF GEC1M1O)                           
                           ERASE                                                
                           CURSOR                                               
                           FREEKB                                               
                           NOHANDLE                                             
           END-EXEC                                                             
                                                                                
           IF      EIBRESP                 NOT    EQUAL  ZEROS                  
                   MOVE    LT-SEND-ERASE   TO     MN-COMANDO-1                  
                   MOVE    EIBRESP         TO     MN-EIBRESP-1                  
                   MOVE    7               TO     MN-PTO-ERRO-1                 
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
           MOVE    8                       TO     MN-PTO-ERRO-2                 
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
