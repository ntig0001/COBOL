      **************************************************************
      *        This program reads data from a                      *
      *        file called invent_in and outputs it to             *
      *        another file called invent_out.                     *
      *                                                            *
      **************************************************************
      *
      **************************************************************
      *                           Glossary                         *
      *        INVENT   --------------------------- Inventory      *
      *        QTY      --------------------------- Quantity       *
      *        INIT     --------------------------- Initialize     *
      *        TERM     --------------------------- Terminate      *
      *        WS       --------------------------- Working        *
      *                                             Storage        *
      *                                             Variables      *
      *        WS-EOF-FLAG ------------------------ End of File    *
      *                                             Flag           *
      *                                                            *
      **************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Inventory-Report-Project2.
       AUTHOR. Elysé Ntigirishari.
       
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT INVENT-FILE-IN ASSIGN TO "D:\Cobol\invent_in.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
               
               SELECT INVENT-FILE-OUT ASSIGN TO 
                                               "D:\Cobol\invent_out.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
                                               
       DATA DIVISION.
           FILE SECTION.
           FD INVENT-FILE-IN.
           01 INVENT-RECORD-IN.
              05 PART-NUMBER-IN        PIC 9(7).
              05 PART-NAME-IN          PIC X(20).
              05 QTY-ON-HAND-IN        PIC 9(4).
              05 UNIT-PRICE-IN         PIC 9(4).
              05 SUPPLIER-CODE-IN      PIC X(5).
              
           FD INVENT-FILE-OUT.
           01 INVENT-RECORD-OUT        PIC X(67).
           
           WORKING-STORAGE SECTION.
           01 WS-INVENT-RECORD-DETAIL.
              05 WS-PART-NUMBER-OUT    PIC 9(7).
              05 FILLER                PIC X(6)    VALUE SPACES.
              05 WS-PART-NAME-OUT      PIC X(20).
              05 FILLER                PIC X(4)    VALUE SPACES.
              05 WS-SUPPLIER-CODE-OUT  PIC X(5).
              05 FILLER                PIC X(6)    VALUE SPACES.
              05 WS-QTY-ON-HAND-OUT    PIC 9(4).
              05 FILLER                PIC X(6)    VALUE SPACES.
              05 WS-STOCK-VALUE-OUT    PIC 9(8).
              
           01 WS-COLUMN-HEADER.
              05 FILLER                PIC X(11)   VALUE "Part Number".
              05 FILLER                PIC X(2)    VALUE SPACES.
              05 FILLER                PIC X(9)    VALUE "Part Name".
              05 FILLER                PIC X(15)   VALUE SPACES.        
              05 FILLER                PIC X(9)    VALUE "Sup. Code".
              05 FILLER                PIC X(2)    VALUE SPACES.        
              05 FILLER                PIC X(8)    VALUE "Quantity".
              05 FILLER                PIC X(2)    VALUE SPACES.        
              05 FILLER                PIC X(5)    VALUE "Value".
              
           01 WS-AUDIT-TRAIL.
              05 FILLER                PIC X(12)   VALUE "Total Value:".
              05 FILLER                PIC X       VALUE SPACES.
              05 WS-TOTAL-INV-VALUE    PIC 9(10)   VALUE ZERO.
              05 FILLER                PIC X(5)    VALUE SPACES.        
              05 FILLER                PIC X(5)    VALUE "Read:".
              05 FILLER                PIC X       VALUE SPACES.
              05 WS-READ-COUNTER       PIC 9(4)    VALUE ZERO.
              05 FILLER                PIC X(5)    VALUE SPACES.
              05 FILLER                PIC X(8)    VALUE "Written:".    
              05 FILLER                PIC X       VALUE SPACES.
              05 WS-WRITE-COUNTER      PIC 9(4)    VALUE ZERO.
              
           01 WS-FLAGS-AND-COUNTERS.
              05 WS-EOF-FLAG           PIC X(3)    VALUE "NO".
              
       PROCEDURE DIVISION.
           100-PRODUCE-INVENTORY-REPORT.
               PERFORM 201-INIT-INVENTORY-REPORT.
               PERFORM 202-PRODUCE-INVENT-DETAIL-RECORD
                       UNTIL WS-EOF-FLAG = "YES".
               PERFORM 203-TERM-INVENTORY-REPORT.
               STOP RUN.
           
           201-INIT-INVENTORY-REPORT.
               PERFORM 301-OPEN-INVENT-FILES.
               PERFORM 304-READ-INVENT-RECORD.
               PERFORM 302-WRITE-COLUMN-HEADERS.
           
           202-PRODUCE-INVENT-DETAIL-RECORD.
               PERFORM 303-CALCULATE-INVENT-VALUE.
               PERFORM 305-CALCULATE-TOTAL-INVENT-VALUE.
               PERFORM 306-WRITE-INVENT-DETAIL.
               PERFORM 304-READ-INVENT-RECORD.
           
           203-TERM-INVENTORY-REPORT.
               PERFORM 307-WRITE-AUDIT-TRAIL.
               PERFORM 308-CLOSE-INVENT-FILES.
               
               301-OPEN-INVENT-FILES.
                   OPEN INPUT  INVENT-FILE-IN
                        OUTPUT INVENT-FILE-OUT.
               
               302-WRITE-COLUMN-HEADERS.
                   WRITE INVENT-RECORD-OUT FROM WS-COLUMN-HEADER.
                   WRITE INVENT-RECORD-OUT FROM SPACES.
               
               303-CALCULATE-INVENT-VALUE.
                   MULTIPLY QTY-ON-HAND-IN BY UNIT-PRICE-IN
                   GIVING WS-STOCK-VALUE-OUT.
           
               304-READ-INVENT-RECORD.
                   READ INVENT-FILE-IN
                       AT END MOVE "YES" TO WS-EOF-FLAG
                       NOT AT END ADD 1 TO WS-READ-COUNTER.
           
               305-CALCULATE-TOTAL-INVENT-VALUE.
                   ADD WS-STOCK-VALUE-OUT TO WS-TOTAL-INV-VALUE.
               
               306-WRITE-INVENT-DETAIL.
                   MOVE PART-NUMBER-IN TO WS-PART-NUMBER-OUT.
                   MOVE PART-NAME-IN TO WS-PART-NAME-OUT.
                   MOVE SUPPLIER-CODE-IN TO WS-SUPPLIER-CODE-OUT.
                   MOVE QTY-ON-HAND-IN TO WS-QTY-ON-HAND-OUT.        
                   WRITE INVENT-RECORD-OUT FROM WS-INVENT-RECORD-DETAIL.
                   ADD 1 TO WS-WRITE-COUNTER.
               
               307-WRITE-AUDIT-TRAIL.
                   WRITE INVENT-RECORD-OUT FROM SPACES.
                   WRITE INVENT-RECORD-OUT FROM WS-AUDIT-TRAIL.
                
               308-CLOSE-INVENT-FILES.
                   CLOSE INVENT-FILE-IN INVENT-FILE-OUT.
                   