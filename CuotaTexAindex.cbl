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
           SELECT CTA ASSIGN TO "..\cuotas.txt"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT CUOTAS ASSIGN TO "..\cuotas.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS cuota-llave.
       DATA DIVISION.
       FILE SECTION.

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
       FD  CTA.
       01  cuo-tex-reg.
           03 cuo-tex-alumno pic 9(5).
           03 cuo-tex-cuota pic 9(2).
           03 cuo-tex-fecha pic 9(8).
           03 cuo-tex-estado pic x.
           03 cuo-tex-importe pic 9(8).
           03 cuo-tex-pagado pic 9(8).
           03 cuo-tex-debe pic 9(8).

       WORKING-STORAGE SECTION.
       77  w-flag-cta pic 9.
           88 fin-archivo value 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            PERFORM 100-INICIO.
           PERFORM 200-LEER-ARCH-CTA.
           PERFORM UNTIL fin-archivo
            PERFORM 300-PROCESO
            PERFORM 200-LEER-ARCH-CTA
           END-PERFORM.
           PERFORM 400-FIN.
            STOP RUN.

       100-INICIO.
           OPEN INPUT CTA.
           OPEN OUTPUT CUOTAS.

       200-LEER-ARCH-CTA.
           READ CTA AT END MOVE 1 TO w-flag-cta.

       300-PROCESO.
           MOVE cuo-tex-alumno TO cuo-alumno.
           MOVE cuo-tex-cuota TO cuo-cuota.
           move cuo-tex-fecha TO cuo-fecha.
           move cuo-tex-estado TO cuo-estado.
           move cuo-tex-importe TO cuo-importe.
           move cuo-tex-pagado TO cuo-pagado.
           MOVE cuo-tex-debe TO cuo-debe.
           write cuo-reg.

       400-FIN.
           CLOSE CTA.
           CLOSE CUOTAS.

       END PROGRAM YOUR-PROGRAM-NAME.
