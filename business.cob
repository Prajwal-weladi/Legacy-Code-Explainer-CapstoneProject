       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL01.
       AUTHOR. PRAJWAL.
       DATE-WRITTEN. 2026-01-16.
       DESCRIPTION.
           PAYROLL BATCH PROGRAM TO PROCESS EMPLOYEE SALARIES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMP-IN-FILE
               ASSIGN TO EMPIN
               ORGANIZATION IS SEQUENTIAL.

           SELECT EMP-OUT-FILE
               ASSIGN TO EMPOUT
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD EMP-IN-FILE.
       01 EMP-IN-REC.
          05 IN-EMP-ID        PIC 9(5).
          05 IN-EMP-NAME      PIC X(20).
          05 IN-DEPT-CODE     PIC X(4).
          05 IN-BASIC-SAL     PIC 9(7)V99.

       FD EMP-OUT-FILE.
       01 EMP-OUT-REC.
          05 OUT-EMP-ID       PIC 9(5).
          05 OUT-EMP-NAME     PIC X(20).
          05 OUT-DEPT-CODE    PIC X(4).
          05 OUT-BASIC-SAL    PIC 9(7)V99.
          05 OUT-BONUS        PIC 9(7)V99.
          05 OUT-TAX          PIC 9(7)V99.
          05 OUT-NET-PAY      PIC 9(7)V99.

       WORKING-STORAGE SECTION.

       01 WS-FLAGS.
          05 WS-EOF           PIC X VALUE 'N'.

       01 WS-CALCULATION.
          05 WS-BONUS         PIC 9(7)V99.
          05 WS-TAX           PIC 9(7)V99.
          05 WS-NET-PAY       PIC 9(7)V99.

       01 WS-TOTALS.
          05 WS-READ-COUNT    PIC 9(5) VALUE 0.
          05 WS-WRITE-COUNT   PIC 9(5) VALUE 0.
          05 WS-TOT-BASIC     PIC 9(9)V99 VALUE 0.
          05 WS-TOT-BONUS     PIC 9(9)V99 VALUE 0.
          05 WS-TOT-TAX       PIC 9(9)V99 VALUE 0.
          05 WS-TOT-NET       PIC 9(9)V99 VALUE 0.

       01 WS-TAX-RATE         PIC V99 VALUE .15.
       01 WS-BONUS-RATE       PIC V99 VALUE .10.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INIT-PARA
           PERFORM READ-PARA
           PERFORM PROCESS-PARA UNTIL WS-EOF = 'Y'
           PERFORM FINAL-PARA
           STOP RUN.

       INIT-PARA.
           OPEN INPUT EMP-IN-FILE
           OPEN OUTPUT EMP-OUT-FILE.

       READ-PARA.
           READ EMP-IN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   ADD 1 TO WS-READ-COUNT
           END-READ.

       PROCESS-PARA.
           IF WS-EOF = 'N'
               PERFORM VALIDATE-PARA
               PERFORM CALC-PARA
               PERFORM WRITE-PARA
               PERFORM READ-PARA
           END-IF.

       VALIDATE-PARA.
           IF IN-BASIC-SAL <= 0
               DISPLAY "INVALID SALARY FOR EMP ID: " IN-EMP-ID
               MOVE 0 TO IN-BASIC-SAL
           END-IF.

       CALC-PARA.
           COMPUTE WS-BONUS = IN-BASIC-SAL * WS-BONUS-RATE
           COMPUTE WS-TAX   = (IN-BASIC-SAL + WS-BONUS) * WS-TAX-RATE
           COMPUTE WS-NET-PAY =
               IN-BASIC-SAL + WS-BONUS - WS-TAX.

           ADD IN-BASIC-SAL TO WS-TOT-BASIC
           ADD WS-BONUS     TO WS-TOT-BONUS
           ADD WS-TAX       TO WS-TOT-TAX
           ADD WS-NET-PAY   TO WS-TOT-NET.

       WRITE-PARA.
           MOVE IN-EMP-ID     TO OUT-EMP-ID
           MOVE IN-EMP-NAME   TO OUT-EMP-NAME
           MOVE IN-DEPT-CODE  TO OUT-DEPT-CODE
           MOVE IN-BASIC-SAL  TO OUT-BASIC-SAL
           MOVE WS-BONUS      TO OUT-BONUS
           MOVE WS-TAX        TO OUT-TAX
           MOVE WS-NET-PAY    TO OUT-NET-PAY

           WRITE EMP-OUT-REC
           ADD 1 TO WS-WRITE-COUNT.

       FINAL-PARA.
           CLOSE EMP-IN-FILE
           CLOSE EMP-OUT-FILE

           DISPLAY "----------------------------------"
           DISPLAY "PAYROLL PROCESSING COMPLETE"
           DISPLAY "RECORDS READ    : " WS-READ-COUNT
           DISPLAY "RECORDS WRITTEN : " WS-WRITE-COUNT
           DISPLAY "TOTAL BASIC     : " WS-TOT-BASIC
           DISPLAY "TOTAL BONUS     : " WS-TOT-BONUS
           DISPLAY "TOTAL TAX       : " WS-TOT-TAX
           DISPLAY "TOTAL NET PAY   : " WS-TOT-NET
           DISPLAY "----------------------------------".
