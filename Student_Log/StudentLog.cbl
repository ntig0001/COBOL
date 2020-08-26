      ************************************************
      *    This program saves data to a file         *
      *    called students.txt from the keyboard.    *
      *                                              *
      ************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. StudentList-Project-1.
       AUTHOR. Elysé Ntigirishari.
       
       ENVIRONMENT DIVISION.
          INPUT-OUTPUT SECTION.
          FILE-CONTROL.
              SELECT STUDENT-FILE-OUT ASSIGN TO "D:\Cobol\students.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
              
       DATA DIVISION.
          FD STUDENT-FILE-OUT.
          01 STUDENT-RECORD-OUT.
             05 STUDENT-NAME                  PIC X(20).
             05 FILLER                        PIC X(2) VALUE SPACES.
             05 STUDENT-ID                    PIC X(9).
             05 FILLER                        PIC X(5) VALUE SPACES.
             05 STUDENT_TOTAL_PERCENTAGE      PIC 9(2).99.
             05 FILLER                        PIC X(6) VALUE SPACES.
             05 STUDENT-GPA                   PIC 9.99.
             05 FILLER                        PIC X(8) VALUE SPACES.
       
          WORKING-STORAGE SECTION.
          01 WS-PROMPTS.
             05 WS-PROMPT-FOR-RECORD          PIC X(22)   VALUE 
                                              "Record to Enter:Y or N".
             05 WS-PROMPT-STUDENT-NAME        PIC X(19)   VALUE
                                              "Enter Student Name:".
             05 WS-PROMPT-STUDENT-ID          PIC X(17)   VALUE
                                              "Enter Student ID:".
             05 WS-PROMPT-STUDENT-AGE         PIC X(18)   VALUE
                                              "Enter Total Mark:".
             05 WS-PROMPT-STUDENT-GPA         PIC X(18)   VALUE
                                              "Enter Student GPA:".
          01 FILE-COLUMN-HEADER.
             05 FILLER              PIC X(20)     VALUE "Student_Name".
             05 FILLER              PIC X(2)     VALUE SPACES.
             05 FILLER              PIC X(10)     VALUE "Student_ID".
             05 FILLER              PIC X(2)      VALUE SPACES.        
             05 FILLER              PIC X(11)     VALUE "Percentage".
             05 FILLER              PIC X(2)      VALUE SPACES.        
             05 FILLER              PIC X(11)     VALUE "GPA".
                                                                                                                                       
          01 WS-RESPONSES.
             05 WS-RECORD-RESPONSE               PIC X(1).
             
       PROCEDURE DIVISION.
       100-PRODUCE-STUDENT-FILE.
          PERFORM 201-INIT-CREATE-STUDENT-FILE.
          PERFORM 202-PRODUCE-STUDENT-RECORD 
                      UNTIL WS-RECORD-RESPONSE = "N" OR "n".
          PERFORM 203-TERMINATE-CREATE-STUDENT.
          STOP RUN.
          
          201-INIT-CREATE-STUDENT-FILE.
              PERFORM 701-OPEN-STUDENT-FILE.
              PERFORM 705-CLEAR-FIELDS.
              PERFORM 702-PROMPT-FOR-RECORD.
          202-PRODUCE-STUDENT-RECORD.
              PERFORM 703-ACCEPT-STUDENT-DATA.
              PERFORM 704-WRITE-STUDENT-RECORD.
              PERFORM 705-CLEAR-FIELDS.
              PERFORM 702-PROMPT-FOR-RECORD.
          203-TERMINATE-CREATE-STUDENT.
              CLOSE STUDENT-FILE-OUT.
       
              701-OPEN-STUDENT-FILE.
                  OPEN OUTPUT STUDENT-FILE-OUT.
                  WRITE STUDENT-RECORD-OUT FROM FILE-COLUMN-HEADER.
              702-PROMPT-FOR-RECORD.
                  DISPLAY WS-PROMPT-FOR-RECORD LINE 2 COLUMN 2.
                  ACCEPT WS-RECORD-RESPONSE LINE 3 COLUMN 2.
              703-ACCEPT-STUDENT-DATA.
                  DISPLAY " " WITH BLANK SCREEN.
                  DISPLAY WS-PROMPT-STUDENT-NAME LINE 6 COLUMN 4.
                  ACCEPT STUDENT-NAME LINE 7 COLUMN 4.
                  DISPLAY WS-PROMPT-STUDENT-ID LINE 9 COLUMN 4.
                  ACCEPT STUDENT-ID LINE 10 COLUMN 4.
                  DISPLAY WS-PROMPT-STUDENT-AGE LINE 12 COLUMN 4.
                  ACCEPT STUDENT_TOTAL_PERCENTAGE LINE 13 COLUMN 4.    
                  DISPLAY WS-PROMPT-STUDENT-GPA LINE 15 COLUMN 4.
                  ACCEPT STUDENT-GPA LINE 16 COLUMN 4.
              704-WRITE-STUDENT-RECORD.
                  WRITE STUDENT-RECORD-OUT AFTER ADVANCING 1 LINE.
              705-CLEAR-FIELDS.
                  MOVE " " TO WS-RECORD-RESPONSE.
                  MOVE SPACES TO STUDENT-RECORD-OUT.
           DISPLAY " " WITH BLANK SCREEN.