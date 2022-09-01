      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NOVEDADES ASSIGN TO "..\novedades.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-ALU
           ASSIGN TO "sortwork".
           SELECT ALUMNOS ASSIGN TO "..\alumnos.dat"
           ORGANIZATION RELATIVE
           ACCESS MODE is DYNAMIC
           RELATIVE key is rel-nro.
            SELECT CUOTAS ASSIGN TO "..\cuotas.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS cuota-llave.
       DATA DIVISION.
       FILE SECTION.

       FD  NOVEDADES.
       01  nov-cab-reg.
           03 nov-cab-tipo PIC 9.
           03 nov-cab-fecha.
               05 nov-cab-anio pic 9(4).
               05 nov-cab-mes pic 9(2).
               05 nov-cab-dia pic 9(2).
       01  nov-det-reg.
           03 nov-det-tipo pic 9.
           03 nov-det-recibo pic 9(10).
           03 nov-det-alu pic 9(5).
           03 nov-det-cuota pic 99.
           03 nov-det-importe pic 9(8).

       SD  SORT-ALU.
       01  srt-cab-reg.
           03 srt-cab-tipo PIC 9.
           03 srt-cab-fecha.
               05 srt-cab-anio pic 9(4).
               05 srt-cab-mes pic 9(2).
               05 srt-cab-dia pic 9(2).
       01  srt-det-reg.
           03 srt-det-tipo pic 9.
           03 srt-det-recibo pic 9(10).
           03 srt-det-alu pic 9(5).
           03 srt-det-cuota pic 99.
           03 srt-det-importe pic 9(8).

       FD  ALUMNOS.
       01  alu-reg.
           03 alu-alumno pic 9(5).
           03 alu-cuotas pic 9(2).
           03 alu-estado pic x.
           03 alu-puntero pic 9(2).
       FD  CUOTAS.
       01  cuo-reg.
           03 cuota-llave.
               05 cuo-alumno pic 9(5).
               05 cuo-cuota pic 9(2).
           03 cuo-fecha pic 9(8).
           03 cuo-estado pic x.
           03 cuo-importe pic 9(8).
           03 cuo-pagado pic 9(8).
           03 cuo-debe pic 9(8).

       WORKING-STORAGE SECTION.
       01  w-flag-sort PIC 9 VALUE ZERO.
       01  w-flag-nov PIC 9 VALUE ZERO.
       01  w-flag-cuota pic 9 value zero.
       01  w-alu pic 9(5).
       01  w-acum-imp-cuot pic 9(8).
       01  w-cuota-ant pic 99.
       01  rel-nro pic 9(2).
       01  w-alu-ant pic 9(5).
       01  w-resul pic 9(2).
       01  w-resto pic 9(2).
       77  w-oficina pic 9(2) value 98.
       01  w-posicion-nula pic 9(2) VALUE ZERO.
       01  w-dif-imp pic 9(8).
       01  w-primer-alu pic 9(5) value 99999.
       01  w-cuota-imp pic x value "C".

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
             SORT SORT-ALU ASCENDING srt-det-alu srt-det-cuota
             srt-cab-fecha
            INPUT PROCEDURE is DATOS-ENTRADA
            OUTPUT PROCEDURE IS DATOS-SALIDA.
            STOP RUN.

       DATOS-ENTRADA.

            PERFORM 100-INICIO.
            PERFORM 120-LEER-NOVEDADES.
               PERFORM UNTIL w-flag-nov IS =1 OR
               nov-cab-anio IS = 2022
                   PERFORM 120-LEER-NOVEDADES
               END-PERFORM.
            PERFORM 175-FIN-ANIO.

       100-INICIO.
           OPEN input NOVEDADES.

       120-LEER-NOVEDADES.
           READ NOVEDADES AT END MOVE 1 TO w-flag-nov.

       150-DESAGOTAR-DETALLE.
            IF nov-cab-tipo =1
               PERFORM 120-LEER-NOVEDADES.

       170-CARGAR-SORT.
           MOVE nov-det-alu to srt-det-alu.
           MOVE nov-det-cuota to srt-det-cuota.
           move nov-det-importe to srt-det-importe.
           RELEASE srt-det-reg.

       175-FIN-ANIO.
           PERFORM UNTIL w-flag-nov =1
               PERFORM 150-DESAGOTAR-DETALLE
               PERFORM UNTIL w-flag-nov =1 OR nov-cab-tipo=1
                PERFORM 170-CARGAR-SORT
               PERFORM 120-LEER-NOVEDADES
               END-PERFORM
           END-PERFORM.
           PERFORM 180-FIN-NOVEDADES.

       180-FIN-NOVEDADES.
           CLOSE NOVEDADES.

       DATOS-SALIDA.

           PERFORM 190-INICIO-GENERAL.
           PERFORM 200-LEER-SORT.
           PERFORM UNTIL w-flag-sort =1
              PERFORM 230-INICIO-ALU-SORT
              PERFORM UNTIL w-flag-sort =1 OR srt-det-alu is NOT =w-alu
                   PERFORM 240-INICIO-CUOTA
                   PERFORM UNTIL w-flag-sort =1
                   OR srt-det-alu is NOT =w-alu
                   OR srt-det-cuota IS NOT=w-cuota-ant
                       PERFORM 245-PROCESO-CUOTA
                       PERFORM 200-LEER-SORT
                   END-PERFORM
                   PERFORM 250-FIN-CUOTA
              END-PERFORM
              PERFORM 290-FIN-SORT-ALU
           END-PERFORM.
           PERFORM 1000-FIN-GENERAL.

       190-INICIO-GENERAL.
           OPEN I-O ALUMNOS.
           OPEN I-O CUOTAS.

       200-LEER-SORT.
           RETURN SORT-ALU AT END MOVE 1 TO w-flag-sort.

       230-INICIO-ALU-SORT.
           MOVE srt-det-alu TO  w-alu.

       240-INICIO-CUOTA.
           move srt-det-cuota to w-cuota-ant.
           MOVE ZERO TO w-acum-imp-cuot.
           move zero to w-dif-imp.

       245-PROCESO-CUOTA.
           ADD srt-det-importe TO w-acum-imp-cuot.

       250-FIN-CUOTA.
           MOVE w-alu TO w-alu-ant.
           PERFORM 300-BUSCAR-ALU-EN-ALU.
           IF alu-estado= "A"
               PERFORM 900-CARGAR-ARCH-CUOTAS.

       290-FIN-SORT-ALU.
           if w-primer-alu > w-alu
               move w-alu to w-primer-alu.

       300-BUSCAR-ALU-EN-ALU.
           PERFORM 410-INVOCAR-FUNCION-HASHING.
           PERFORM 430-LEER-ALU.
            IF  w-alu-ant = alu-alumno
                 PERFORM 620-MOSTRAR-ALU
             ELSE
                PERFORM 630-BUSCAR-ALU-SINONIMO
            END-IF.

       410-INVOCAR-FUNCION-HASHING.
           DIVIDE 71 INTO w-alu-ant GIVING w-resul REMAINDER rel-nro.
           add 1 to rel-nro.
           MOVE rel-nro to w-resto.
       430-LEER-ALU.
           READ ALUMNOS.

       620-MOSTRAR-ALU.
           DISPLAY alu-alumno.
           DISPLAY alu-estado.

       630-BUSCAR-ALU-SINONIMO.
           PERFORM UNTIL alu-puntero is =0 or w-alu-ant is =alu-alumno
               MOVE  alu-puntero to rel-nro
               PERFORM 430-LEER-ALU
           END-PERFORM
           IF  w-alu-ant = alu-alumno
            PERFORM 620-MOSTRAR-ALU
           ELSE
              PERFORM 900-CARGAR-ARCH-CUOTAS
           END-IF.

       900-CARGAR-ARCH-CUOTAS.
           PERFORM 930-LEER-CUOTA.

       930-LEER-CUOTA.
           MOVE w-alu TO cuo-alumno.
           MOVE w-cuota-ant TO cuo-cuota.
           READ CUOTAS
           INVALID KEY DISPLAY "NO ENCONTRE ALUMNO"
           NOT INVALID KEY PERFORM 950-CARGAR-CUOTA.

       950-CARGAR-CUOTA.
            MOVE w-acum-imp-cuot TO cuo-pagado.
            COMPUTE w-dif-imp= cuo-importe - w-acum-imp-cuot.
            move w-dif-imp to cuo-debe.
             if cuo-debe >0
               move "I" TO cuo-estado
           ELSE
               MOVE "C" TO cuo-estado
           END-IF.
           REWRITE cuo-reg.

       1000-FIN-GENERAL.
           PERFORM 970-CARGAR-EST-ALU.
           PERFORM 1010-CERRAR-ARCHIVOS.

       970-CARGAR-EST-ALU.
           PERFORM 975-POSICIONAR-ALU.

       975-POSICIONAR-ALU.
           MOVE w-primer-alu TO cuo-alumno.
           MOVE ZERO TO cuo-cuota.
           START CUOTAS KEY IS > cuota-llave
           INVALID KEY
           DISPLAY "NO ESTA"
           NOT INVALID KEY
           PERFORM 980-PROCESO-EST-ALU.

       980-PROCESO-EST-ALU.
           PERFORM 985-LEER-SIG-CUOTA.
           PERFORM UNTIL w-flag-cuota=1
               PERFORM 990-INICIO-CUOTA
               PERFORM UNTIL w-flag-cuota=1
               or cuo-alumno is not=w-alu-ant
                   PERFORM 987-PROCESO-ESTADO
                   PERFORM 985-LEER-SIG-CUOTA
               END-PERFORM
               PERFORM 995-FIN-CUOTA
           END-PERFORM.

       985-LEER-SIG-CUOTA.
           READ CUOTAS NEXT AT END MOVE 1 TO w-flag-cuota.

       987-PROCESO-ESTADO.
           if cuo-estado ="I"
               MOVE "I" TO w-cuota-imp
           ELSE
               MOVE "C" TO w-cuota-imp.

       990-INICIO-CUOTA.
           MOVE cuo-alumno TO w-alu-ant.

       995-FIN-CUOTA.
           PERFORM 1005-BUSCAR-ALU.

       1005-BUSCAR-ALU.
           PERFORM 410-INVOCAR-FUNCION-HASHING.
           PERFORM 430-LEER-ALU.
            IF  w-alu-ant = alu-alumno
                 PERFORM 1006-CAMBIAR-ESTADO
            ELSE
               PERFORM UNTIL alu-puntero is =0
               or w-alu-ant is =alu-alumno
               MOVE  alu-puntero to rel-nro
               PERFORM 430-LEER-ALU
               END-PERFORM.
               IF  w-alu-ant = alu-alumno
                   PERFORM 1006-CAMBIAR-ESTADO.

       1006-CAMBIAR-ESTADO.
           IF w-cuota-imp = "I"
               MOVE "A" TO alu-estado
           ELSE
               MOVE "P" TO alu-estado.
           REWRITE alu-reg.

       1010-CERRAR-ARCHIVOS.
           CLOSE ALUMNOS.
           CLOSE CUOTAS.

       END PROGRAM YOUR-PROGRAM-NAME.
