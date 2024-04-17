       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODIFY-EMPLOYEE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       SELECT EMPLEADOS-ARCHIVO ASSIGN TO "PHYSICAL-FILE.dat"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS EMPLEADO-ID
           FILE STATUS IS FS-EMPLEADOS.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLEADOS-ARCHIVO.
       01 EMPLEADO-REGISTRO.
          05 EMPLEADO-ID     PIC 9(5).
          05 NOMBRE-EMPLEADO PIC X(30).
          05 APELLIDO-EMPLEADO PIC X(30).
          05 EDAD-EMPLEADO    PIC 99.
          05 TELEFONO        PIC X(12).
          05 DIRECCION       PIC X(50).
       WORKING-STORAGE SECTION.
       77 FS-EMPLEADOS          PIC X(02).
       77 MODIFICAR-DATOS       PIC X.

       PROCEDURE DIVISION.
           OPEN I-O EMPLEADOS-ARCHIVO
           PERFORM UNTIL FS-EMPLEADOS = "00"
               DISPLAY "¿Desea modificar este registro? (S/N)"
               ACCEPT MODIFICAR-DATOS
               IF MODIFICAR-DATOS = "S"
                   PERFORM MODIFICAR-REGISTRO
               END-IF
               READ EMPLEADOS-ARCHIVO
                   INVALID KEY
                       MOVE "00" TO FS-EMPLEADOS
                   NOT INVALID KEY
                       DISPLAY "Registro leído: " EMPLEADO-ID ", " NOMBRE-EMPLEADO
               END-READ
           END-PERFORM
           CLOSE EMPLEADOS-ARCHIVO
           STOP RUN.

       MODIFICAR-REGISTRO.
           DISPLAY "Ingrese el nuevo nombre: "
           ACCEPT NOMBRE-EMPLEADO
           DISPLAY "Ingrese el nuevo apellido: "
           ACCEPT APELLIDO-EMPLEADO
           DISPLAY "Ingrese la nueva edad: "
           ACCEPT EDAD-EMPLEADO
           DISPLAY "Ingrese el nuevo teléfono: "
           ACCEPT TELEFONO
           DISPLAY "Ingrese la nueva dirección: "
           ACCEPT DIRECCION
           REWRITE EMPLEADO-REGISTRO
               INVALID KEY
                   DISPLAY "Error al modificar el registro."
               NOT INVALID KEY
                   DISPLAY "Registro modificado correctamente."
           END-REWRITE.
