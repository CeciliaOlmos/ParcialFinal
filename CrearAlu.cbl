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
       77  i pic 9(2).
           88 sala VALUE 70.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN OUTPUT ALUMNOS.

           perform varying i from 1 by 1 until i > 97
               MOVE 0 TO alu-alumno
               move 0 TO alu-cuotas
               move " " to alu-estado
               if sala
                   move 71 to alu-puntero
               else
                   move 0 to alu-puntero
               end-if
               write alu-reg INVALID KEY DISPLAY "no pude"
                             NOT INVALID KEY
                               DISPLAY "GRABACION EXITOSA"
           END-PERFORM.

           close ALUMNOS.

           STOP RUN.

       END PROGRAM YOUR-PROGRAM-NAME.
