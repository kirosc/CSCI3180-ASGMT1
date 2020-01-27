       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INSTRUCTOR-FILE ASSIGN TO 'instructors.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CANDIDATE-FILE ASSIGN TO 'candidates.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INSTRUCTOR-FILE.
       01 INSTRUCTOR.
           05 COURSE-ID PIC X(5).
           05 REQ-SKILLS.
               10 REQ-SKILL PIC X(15) OCCURS 3 TIMES.
           05 OPT-SKILLS.
               10 OPT-SKILL PIC X(15) OCCURS 5 TIMES.

       FD CANDIDATE-FILE.
       01 CANDIDATE.
           05 SID PIC X(11).
           05 TA-SKILLS PIC X(120).
           05 PREFERENCES.
               10 PREFERENCE PIC X(5) OCCURS 3 TIMES.

       WORKING-STORAGE SECTION.
       01 INSTRUCTOR-EOF PIC A(1).
       01 CANDIDATE-EOF PIC A(1).
       01 MATCHED-SKILLS PIC 9(1).
       01 SCORES PIC 9(1)V9(1) VALUE 1.
       01 COURSE-CANDIDATES.
           05 COURSE-CANDIDATE PIC X(11) OCCURS 3 TIMES
           VALUE '0000000000 '.
       01 TEMP PIC A(100).

       PROCEDURE DIVISION.
       MAIN.
           PERFORM OPEN-CANDIDATE-FILE.
           PERFORM READ-INSTRUCTOR-FILE.
      *    PERFORM READ-CANDIDATE-LINE 8 TIMES.

           PERFORM CLOSE-CANDIDATE-FILE.
           STOP RUN.

      * Read the instructors.txt
       READ-INSTRUCTOR-FILE.
           OPEN INPUT INSTRUCTOR-FILE.
               PERFORM READ-INSTRUCTOR-LINES.
           CLOSE INSTRUCTOR-FILE.
           MOVE ' ' TO INSTRUCTOR-EOF.

       OPEN-CANDIDATE-FILE.
           OPEN INPUT CANDIDATE-FILE.

       CLOSE-CANDIDATE-FILE.
           CLOSE CANDIDATE-FILE.
           MOVE ' ' TO CANDIDATE-EOF.

      * Read all instructors information
       READ-INSTRUCTOR-LINES.
           IF NOT INSTRUCTOR-EOF='Y' THEN
               READ INSTRUCTOR-FILE INTO INSTRUCTOR
                   AT END MOVE 'Y' TO INSTRUCTOR-EOF
                   NOT AT END
                       PERFORM RANK-TA 6 TIMES
                       DISPLAY "###"
                       GO TO READ-INSTRUCTOR-LINES
               END-READ
           END-IF.

       RANK-TA.
      *    Read all candidates and move to top
           IF CANDIDATE-EOF='Y' THEN
               PERFORM CLOSE-CANDIDATE-FILE
               PERFORM OPEN-CANDIDATE-FILE
               EXIT PARAGRAPH
           END-IF.

           PERFORM READ-CANDIDATE-LINE.
           PERFORM CALCULATE-CANDIDATE-SCORE.
           PERFORM RESET-WS.

      * Read a candidate information
       READ-CANDIDATE-LINE.
           IF NOT CANDIDATE-EOF='Y' THEN
               READ CANDIDATE-FILE INTO CANDIDATE
                   AT END
      *                Because of the empty line at the end of the file,
      *                that line has to be discarded
                       MOVE 'Y' TO CANDIDATE-EOF
                       PERFORM CLOSE-CANDIDATE-FILE
                       PERFORM OPEN-CANDIDATE-FILE
                       READ CANDIDATE-FILE INTO CANDIDATE
               END-READ
           END-IF.

       CALCULATE-CANDIDATE-SCORE.
           PERFORM CHECK-REQ-SKILLS.
      *    IF NOT MATCHED-SKILLS = 3 THEN
      *        EXIT PARAGRAPH
      *    END-IF

           PERFORM CHECK-OPT-SKILLS.
           PERFORM CHECK-PREFERENCES.
           DISPLAY SCORES.

      *    Easier and cleaner if insert candidate here
           PERFORM INSERT-CANDIDATE.

       CHECK-REQ-SKILLS.
           INSPECT TA-SKILLS
           TALLYING MATCHED-SKILLS FOR ALL REQ-SKILL(1)
           TALLYING MATCHED-SKILLS FOR ALL REQ-SKILL(2)
           TALLYING MATCHED-SKILLS FOR ALL REQ-SKILL(3).
           
       CHECK-OPT-SKILLS.
           MOVE 0 TO MATCHED-SKILLS

           INSPECT TA-SKILLS
           TALLYING MATCHED-SKILLS FOR ALL OPT-SKILL(1)
           TALLYING MATCHED-SKILLS FOR ALL OPT-SKILL(2)
           TALLYING MATCHED-SKILLS FOR ALL OPT-SKILL(3)
           TALLYING MATCHED-SKILLS FOR ALL OPT-SKILL(4)
           TALLYING MATCHED-SKILLS FOR ALL OPT-SKILL(5).

           ADD MATCHED-SKILLS TO SCORES.

      * Check candidate's preference and add the preference_score   
       CHECK-PREFERENCES.
           IF PREFERENCE(1) EQUAL COURSE-ID THEN
               ADD 1.5 TO SCORES
               EXIT PARAGRAPH
           END-IF.
           IF PREFERENCE(2) EQUAL COURSE-ID THEN
               ADD 1.0 TO SCORES
               EXIT PARAGRAPH
           END-IF.
           IF PREFERENCE(3) EQUAL COURSE-ID THEN
               ADD 0.5 TO SCORES
               EXIT PARAGRAPH
           END-IF.

       RESET-WS.
           INITIALIZE COURSE-CANDIDATES REPLACING
           ALPHANUMERIC DATA BY '0000000000 '.
           MOVE 1 TO SCORES.
           MOVE 0 TO MATCHED-SKILLS.
           
       INSERT-CANDIDATE.
           DISPLAY ' '.