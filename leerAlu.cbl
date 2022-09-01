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
           SELECT ALUMNOS ASSIGN TO "..\alumnos.dat"
           ORGANIZATION RELATIVE
           ACCESS MODE is SEQUENTIAL
           RELATIVE key is rel-nro.
       DATA DIVISION.
       FILE SECTION.

       FD  ALUMNOS.
       01  alu-reg.
           03 alu-alumno pic 9(5).
           03 alu-cuotas pic 9(2).
           03 alu-estado pic x.
           03 alu-puntero pic 9(2).

       WORKING-STORAGE SECTION.
       01  rel-nro pic 9(2).
       77  sen pic 9 value 0.
       77  i pic 99 value 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN input ALUMNOS.

           perform leo.
           perform until sen = 1
               add 1 to i
               display "posicion: ", i
               display "legajo: ", alu-alumno
               display "cuota: ", alu-cuotas
               display "estado: ", alu-estado
               display "puntero:", alu-puntero
               perform leo
            END-PERFORM.
           close ALUMNOS.

           STOP RUN.

       leo.
           read ALUMNOS at end move 1 to sen.

       END PROGRAM YOUR-PROGRAM-NAME.
