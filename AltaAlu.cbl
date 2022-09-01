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
           ACCESS MODE is DYNAMIC
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
       77  sen pic 9.
           88 fin-de-archivo value 1.
       01  rel-nro pic 99.
           88 no-quiere-mas value 0.
       77  w-llave-menu pic 9.
           88 salir-menu VALUE 3.
       01  w-alu-ant pic 9(5).
       01  w-resul pic 9(2).
       01  w-resto pic 9(2).
       77  w-oficina pic 9(2) value 98.
       01  w-posicion-nula pic 9(2) VALUE ZERO.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-MENU.
           PERFORM UNTIL salir-menu
                PERFORM 300-PROCESO
               PERFORM 200-MENU
           END-PERFORM.
           PERFORM 800-FIN.
            STOP RUN.

        100-INICIO.
           OPEN I-O ALUMNOS.
           move w-oficina to w-posicion-nula.

       110-PRIMER-POSICION-VACIA.
           MOVE w-oficina TO rel-nro
           START ALUMNOS KEY IS = rel-nro
            INVALID KEY
            DISPLAY "NO HAY LUGAR PARA SINONIMOS"
            NOT INVALID KEY
               READ ALUMNOS
               MOVE alu-puntero TO w-posicion-nula.

       200-MENU.

           DISPLAY ".................................... ".
           DISPLAY "  INGRESE UNA OPCION: "
           DISPLAY "  1- ALTA"
           DISPLAY "  2- CONSULTA"
           DISPLAY "  3- FIN"
           DISPLAY "..................................... ".
           ACCEPT w-llave-menu.
           PERFORM 210-VALIDAR-OPCION.

       210-VALIDAR-OPCION.
           PERFORM UNTIL w-llave-menu < 4 AND w-llave-menu >0
               DISPLAY "Opcion incorrecta"
               PERFORM 200-MENU
           END-PERFORM.

       300-PROCESO.
           IF w-llave-menu is EQUAL 1
               PERFORM 400-ALTA
           ELSE
               PERFORM 600-CONSULTA
           END-IF.

       400-ALTA.
           PERFORM 405-PIDO-ALU.
           PERFORM 410-INVOCAR-FUNCION-HASHING.
           PERFORM 420-BUSCAR-UBICACION.

       405-PIDO-ALU.
           DISPLAY "Ingrese legajo del alumno".
           ACCEPT w-alu-ant.

       410-INVOCAR-FUNCION-HASHING.
           DIVIDE 71 INTO w-alu-ant GIVING w-resul REMAINDER rel-nro.
           add 1 to rel-nro.
           MOVE rel-nro to w-resto.

       420-BUSCAR-UBICACION.
           PERFORM 430-LEER-ALU
           IF alu-alumno=0
               PERFORM 440-PRIMER-INGRESO
           ELSE
               IF w-alu-ant=alu-alumno
                   PERFORM 460-INGRESO-EXISTENTE

               ELSE
                   PERFORM 480-UBICAR-SINONIMO
               END-IF
           END-IF.

       430-LEER-ALU.
           READ ALUMNOS.

       440-PRIMER-INGRESO.
            MOVE w-alu-ant to alu-alumno
               PERFORM 450-PIDO-RESTO
               PERFORM 470-ACTUALIZAR-ALU.

       460-INGRESO-EXISTENTE.
           DISPLAY "El socio ya se encuentra registrado"
           PERFORM 620-MOSTRAR-ALU.

       470-ACTUALIZAR-ALU.
           REWRITE alu-reg.

       450-PIDO-RESTO.
           DISPLAY "Ingrese resto de datos".
           DISPLAY "cuota".
           ACCEPT alu-cuotas.
           DISPLAY "estado".
           ACCEPT alu-estado.

       455-MOVER-VARIABLES.
           MOVE alu-puntero to rel-nro.
           MOVE w-alu-ant TO alu-alumno.
           MOVE ZERO to alu-puntero.
           PERFORM 450-PIDO-RESTO.
           PERFORM 470-ACTUALIZAR-ALU.

       480-UBICAR-SINONIMO.
            PERFORM UNTIL alu-puntero is =0
            or w-alu-ant is =alu-alumno
             MOVE  alu-puntero to rel-nro
             PERFORM 430-LEER-ALU
            END-PERFORM.
             IF  w-alu-ant = alu-alumno
                PERFORM 460-INGRESO-EXISTENTE
             ELSE
                PERFORM 500-BUSCO-LUGAR
            END-IF.

       500-BUSCO-LUGAR.
           add 1 to  w-oficina.
           MOVE w-oficina to alu-puntero.
           PERFORM 470-ACTUALIZAR-ALU.
           PERFORM 455-MOVER-VARIABLES.
           PERFORM 550-REINICIO-OFICINA.

       550-REINICIO-OFICINA.
           ADD 1 TO rel-nro.
           MOVE rel-nro to alu-puntero.
           MOVE w-posicion-nula TO rel-nro.
           MOVE zero to alu-alumno.
           move ZERO to alu-cuotas.
           move " " to alu-estado.
           PERFORM 470-ACTUALIZAR-ALU.

      ******************************************************************
      ******************************************************************

       600-CONSULTA.
            PERFORM 405-PIDO-ALU.
            PERFORM 410-INVOCAR-FUNCION-HASHING.
            PERFORM 430-LEER-ALU.
             IF  w-alu-ant = alu-alumno
                 PERFORM 620-MOSTRAR-ALU
             ELSE
                PERFORM 630-BUSCAR-ALU-SINONIMO
            END-IF.

       620-MOSTRAR-ALU.
            display "LEGAJO: ",alu-alumno," CUOTAS: ", alu-cuotas.
            DISPLAY "ESTADO:",alu-estado, "PUNTERO:",alu-puntero.

       630-BUSCAR-ALU-SINONIMO.
           PERFORM UNTIL alu-puntero is =0 or w-alu-ant is =alu-alumno
               MOVE  alu-puntero to rel-nro
               PERFORM 430-LEER-ALU
           END-PERFORM
           IF  w-alu-ant = alu-alumno
            PERFORM 620-MOSTRAR-ALU
           ELSE
               DISPLAY "El legajo ingresado no se encuentra"
           END-IF.

       800-FIN.
           CLOSE ALUMNOS.

       END PROGRAM YOUR-PROGRAM-NAME.
