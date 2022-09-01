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

       WORKING-STORAGE SECTION.
       77  sen pic 9 value 0.
       01  lin-cabecera.
           03 filler pic x(7) value "LEGAJO:".
           03 filler pic x(2) value spaces.
           03 filler pic x(6) value "CUOTA:".
           03 filler pic x(2) value spaces.
           03 filler pic x(6) value "FECHA:".
           03 filler pic x(3) value spaces.
           03 filler pic x(7) value "ESTADO:".
           03 filler pic x(4) value spaces.
           03 filler pic x(8) value "IMPORTE:".
           03 filler pic x(4) value spaces.
           03 filler pic x(7) value "PAGADO:".
           03 filler pic x(4) value spaces.
           03 filler pic x(5) value "DEBE:".
           03 filler pic x(4) value spaces.
       01  lin-guarda.
           03 filler pic x(80) value all "-".
       01  lin-detalle.
           03 l-alu pic z(5) value spaces.
           03 filler pic x(4) value spaces.
           03 l-cuota pic z9 value spaces.
           03 filler pic x(4) value spaces.
           03 l-fecha pic Z(8) value spaces.
           03 filler pic x(5) value spaces.
           03 l-estado pic x.
           03 filler pic x(4) value spaces.
           03 l-importe pic zz.zzz.zz9.
           03 filler pic x(2) value spaces.
           03 l-pagado pic zz.zzz.zz9.
           03 filler pic x(2) value spaces.
           03 l-debe pic zz.zzz.zz9.
           03 filler pic x(5) value spaces.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-LECTURA.
           PERFORM 200-LEE-ARCH-CUOTA.
           PERFORM UNTIL sen is equal 1
               PERFORM 300-PROCESO-LECTURA
               PERFORM 200-LEE-ARCH-CUOTA
           END-PERFORM.
           PERFORM 400-FIN-LECTURA.
            STOP RUN.

           100-INICIO-LECTURA.
           PERFORM 130-ABRIR-ARCHIVOS.
           PERFORM 150-LISTAR-ENCABEZADO.

       130-ABRIR-ARCHIVOS.
           OPEN INPUT CUOTAS.

       150-LISTAR-ENCABEZADO.
           DISPLAY lin-guarda.
           DISPLAY lin-cabecera.
           DISPLAY lin-guarda.

       200-LEE-ARCH-CUOTA.
           READ CUOTAS at end move 1 to sen.

       300-PROCESO-LECTURA.
           MOVE cuo-alumno TO l-alu.
           MOVE cuo-cuota TO l-cuota.
           MOVE cuo-fecha TO l-fecha.
           MOVE cuo-estado TO l-estado.
           MOVE cuo-importe TO l-importe.
           MOVE cuo-pagado TO l-pagado.
           MOVE cuo-debe TO l-debe.
           DISPLAY lin-detalle.

       400-FIN-LECTURA.
           CLOSE CUOTAS.
       END PROGRAM YOUR-PROGRAM-NAME.
