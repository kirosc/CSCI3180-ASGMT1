       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INSTRUCTOR-FILE ASSIGN TO 'instructors.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INSTRUCTOR-FILE.
       01 INSTRUCTOR.
           05 COURSE-ID PIC X(5).
           05 REQ-SKILLS PIC X(15) OCCURS 3 TIMES.
           05 OPT-SKILLS PIC X(15) OCCURS 3 TIMES.

       WORKING-STORAGE SECTION.
       01 EOF PIC A(1).

       PROCEDURE DIVISION.
       MAIN.
           PERFORM READ-INSTRUCTOR.
           STOP RUN.

      * Read the instructor.txt
       READ-INSTRUCTOR.
           OPEN INPUT INSTRUCTOR-FILE.
               PERFORM READ-INSTRUCTOR-LINE.
           CLOSE INSTRUCTOR-FILE.

      * Read a line in instructor.txt
       READ-INSTRUCTOR-LINE.
           IF NOT EOF='Y' THEN
               READ INSTRUCTOR-FILE INTO INSTRUCTOR
                   AT END MOVE 'Y' TO EOF
                   NOT AT END
                       DISPLAY INSTRUCTOR
                       GO TO READ-INSTRUCTOR-LINE
               END-READ.
           END-IF.