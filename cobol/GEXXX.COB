       IDENTIFICATION              DIVISION.
       PROGRAM-ID.      GE0PJ.
       AUTHOR.          STEFANINI.
       DATE-WRITTEN.    11/10/2005.
       SECURITY.

      ******************************************************************
      *                                                                *
      *            TESTE SEND MAPAS                                    *
      *                                                                *
      ******************************************************************
      * TRANSACAO = GEPJ                         TIPO PROGRAMOTECA: 53 *
      ******************************************************************
      * NOME DO PROGRAMADOR + NOME DO ANALISTA +     DATA     + VERSAO *
      * CARLOS TANIKAWA     + STEFANINI        +  FEVEREIRO/08+        *
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
           03      FILLER          PIC      X(16)  VALUE 'BOOKS MAPA'.

           COPY        GEC1MS.
           COPY        GEC2MS.
           COPY        GEC3MS.

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
       LINKAGE                     SECTION.
      ******************************************************************
      *                       LINKAGE                                  *
      ******************************************************************

       01          DFHCOMMAREA         PIC   X(1000).

      ******************************************************************
      *            PROCEDURE       DIVISION                            *
      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
      *                      LOGICA PRINCIPAL                          *
      ******************************************************************
       LP-LOGICA-PRINCIPAL         SECTION.

           MOVE    -1              TO       T1OPCAOL
           MOVE    '29/02/2008'    TO       T1DATAO  OF GEC1M1O
                                            T1DATAO  OF GEC2M1O
                                            T1DATAO  OF GEC3M1O

           MOVE    '13:01:59'      TO       T1HORAO  OF GEC1M1O
                                            T1HORAO  OF GEC2M1O
                                            T1HORAO  OF GEC3M1O

           MOVE    ALL 'NO'        TO       T1MENSAO OF GEC1M1O
                                            T1MENSAO OF GEC2M1O
                                            T1MENSAO OF GEC3M1O

           EXEC    CICS    SEND
                           MAP    ('GEC1M1')
                           MAPSET ('GEC1MS')
                           FROM   (GEC1M1O)
                           LENGTH (LENGTH OF GEC1M1O)
                           ERASE
                           CURSOR
                           FREEKB
                           NOHANDLE
           END-EXEC.

           EXEC    CICS    RECEIVE
                           MAP    ('GEC1M1')
                           MAPSET ('GEC1MS')
                           INTO   (GEC1M1I)
                           NOHANDLE
           END-EXEC.

           MOVE    -1     TO   T1PERIDL
           MOVE ALL 'W'   TO   T1LOGATO
           MOVE 29        TO   T1PERIDO
           MOVE 02        TO   T1PERIMO
           MOVE 2008      TO   T1PERIAO
           MOVE 31        TO   T1PERFDO
           MOVE 12        TO   T1PERFMO
           MOVE 2008      TO   T1PERFAO
           MOVE 'P100002' TO   T1USUARO
           MOVE 'X'       TO   T1TIPOEO
           MOVE ALL 'Y'   TO   T1CAMPOO

           EXEC    CICS    SEND
                           MAP    ('GEC2M1')
                           MAPSET ('GEC2MS')
                           FROM   (GEC2M1O)
                           LENGTH (LENGTH OF GEC2M1O)
                           ERASE
                           CURSOR
                           FREEKB
                           NOHANDLE
           END-EXEC.

           EXEC    CICS    RECEIVE
                           MAP    ('GEC2M1')
                           MAPSET ('GEC2MS')
                           INTO   (GEC1M1I)
                           NOHANDLE
           END-EXEC.

           MOVE '23/07/2008'   TO  T1DATA1O
           MOVE 'P100002'      TO  T1USUA1O
           MOVE ALL 'W'        TO  T1EVEN1O
           MOVE ALL 'X'        TO  T1ENTI1O
           MOVE ALL 'Y'        TO  T1CAMP1O
           MOVE ALL 'AN'       TO  T1ANTE1O
           MOVE '13:01'        TO  T1HORA1O
           MOVE ALL 'NO'       TO  T1NOME1O
           MOVE ALL 'AT'       TO  T1ATUA1O

           MOVE '23/07/2008'   TO  T1DATA2O
           MOVE 'P100002'      TO  T1USUA2O
           MOVE ALL 'W'        TO  T1EVEN2O
           MOVE ALL 'X'        TO  T1ENTI2O
           MOVE ALL 'Y'        TO  T1CAMP2O
           MOVE ALL 'AN'       TO  T1ANTE2O
           MOVE '13:01'        TO  T1HORA2O
           MOVE ALL 'NO'       TO  T1NOME2O
           MOVE ALL 'AT'       TO  T1ATUA2O

           MOVE '23/07/2008'   TO  T1DATA3O
           MOVE 'P100002'      TO  T1USUA3O
           MOVE ALL 'W'        TO  T1EVEN3O
           MOVE ALL 'X'        TO  T1ENTI3O
           MOVE ALL 'Y'        TO  T1CAMP3O
           MOVE ALL 'AN'       TO  T1ANTE3O
           MOVE '13:01'        TO  T1HORA3O
           MOVE ALL 'NO'       TO  T1NOME3O
           MOVE ALL 'AT'       TO  T1ATUA3O

           MOVE '23/07/2008'   TO  T1DATA4O
           MOVE 'P100002'      TO  T1USUA4O
           MOVE ALL 'W'        TO  T1EVEN4O
           MOVE ALL 'X'        TO  T1ENTI4O
           MOVE ALL 'Y'        TO  T1CAMP4O
           MOVE ALL 'AN'       TO  T1ANTE4O
           MOVE '13:01'        TO  T1HORA4O
           MOVE ALL 'NO'       TO  T1NOME4O
           MOVE ALL 'AT'       TO  T1ATUA4O

           MOVE '23/07/2008'   TO  T1DATA5O
           MOVE 'P100002'      TO  T1USUA5O
           MOVE ALL 'W'        TO  T1EVEN5O
           MOVE ALL 'X'        TO  T1ENTI5O
           MOVE ALL 'Y'        TO  T1CAMP5O
           MOVE ALL 'AN'       TO  T1ANTE5O
           MOVE '13:01'        TO  T1HORA5O
           MOVE ALL 'NO'       TO  T1NOME5O
           MOVE ALL 'AT'       TO  T1ATUA5O

           EXEC    CICS    SEND
                           MAP    ('GEC3M1')
                           MAPSET ('GEC3MS')
                           FROM   (GEC3M1O)
                           LENGTH (LENGTH OF GEC3M1O)
                           ERASE
                           CURSOR
                           FREEKB
                           NOHANDLE
           END-EXEC.

           EXEC    CICS    RECEIVE
                           MAP    ('GEC3M1')
                           MAPSET ('GEC3MS')
                           INTO   (GEC1M1I)
                           NOHANDLE
           END-EXEC.

       LP-LOGICA-PRINCIPAL-FIM.
           GOBACK.

