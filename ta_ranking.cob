       IDENTIFICATION DIVISION.
       PROGRAM-ID. TA-RANKING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL INSTRUCTOR-FILE ASSIGN TO 'instructors.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS1.
           SELECT OPTIONAL CANDIDATE-FILE ASSIGN TO 'candidates.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS2.
           SELECT OPTIONAL OUTPUT-FILE ASSIGN TO 'output.txt'
               ORGANIZATION IS BINARY SEQUENTIAL.

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

       FD OUTPUT-FILE.
       01 RESULT.
           05 RESULT-COURSE-ID PIC X(5).
           05 RESULT-SIDS.
               10 RESULT-SID PIC X(11) OCCURS 3 TIMES.
           05 RESULT-EOL PIC X.

       WORKING-STORAGE SECTION.
      *File status
       01 FS1              PIC 9(2).
       01 FS2              PIC 9(2).
      *Variables to keep track of the EOF
       01 INSTRUCTOR-EOF   PIC A(1).
       01 CANDIDATE-EOF    PIC A(1).
      *Variables for current evaluated candidate
       01 MATCHED-SKILLS   PIC 9(1).
       01 SCORES           PIC 9(1)V9(1) VALUE 1.
      *Index for ranked candidates
       01 IDX              PIC 9(1).
      *Ranked candidates
       01 COURSE-CANDIDATES.
           05 COURSE-SIDS.
               10  COURSE-SID PIC X(11) OCCURS 4 TIMES
               VALUE '0000000000 '.
           05 COURSE-CANDIDATE-SCORE PIC 9(1)V9(1) OCCURS 4 TIMES
           VALUE 0.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM CHECK-FS.
           PERFORM READ-INSTRUCTOR-FILE.

           CLOSE CANDIDATE-FILE.
           STOP RUN.

      *Check the presence of required files
       CHECK-FS.
           OPEN INPUT CANDIDATE-FILE.
           OPEN INPUT INSTRUCTOR-FILE.
           IF NOT FS1 = 00 OR NOT FS2 EQUAL 00 THEN
               DISPLAY 'non-existing file!'
               CLOSE CANDIDATE-FILE
               CLOSE INSTRUCTOR-FILE
               STOP RUN
           END-IF.
      *    Empty output.txt content
           OPEN OUTPUT OUTPUT-FILE.
           CLOSE OUTPUT-FILE.

      *Read the instructors.txt
       READ-INSTRUCTOR-FILE.
           PERFORM READ-INSTRUCTOR-LINES.
           CLOSE INSTRUCTOR-FILE.
           MOVE ' ' TO INSTRUCTOR-EOF.

       REOPEN-CANDIDATE-FILE.
           CLOSE CANDIDATE-FILE.
           OPEN INPUT CANDIDATE-FILE.

      *Read all instructors information
       READ-INSTRUCTOR-LINES.
           IF NOT INSTRUCTOR-EOF='Y' THEN
               READ INSTRUCTOR-FILE INTO INSTRUCTOR
                   AT END MOVE 'Y' TO INSTRUCTOR-EOF
                   NOT AT END
                       PERFORM RANK-TA
                       PERFORM WRITE-TO-OUTPUT
                       PERFORM RESET-CANDIDATES
                       GO TO READ-INSTRUCTOR-LINES
               END-READ
           END-IF.

      *Rank a candidates for a course
       RANK-TA.
           PERFORM READ-CANDIDATE-LINE.
           IF CANDIDATE-EOF='Y' THEN
               MOVE ' ' TO CANDIDATE-EOF
               EXIT PARAGRAPH
           END-IF.

           PERFORM CALCULATE-CANDIDATE-SCORE.
           PERFORM RESET-SCORE-VARIABLES.
      *    Repeatedly rank for all candidates
           GO TO RANK-TA.

      *Read a candidate information
       READ-CANDIDATE-LINE.
           IF NOT CANDIDATE-EOF='Y' THEN
               READ CANDIDATE-FILE INTO CANDIDATE
                   AT END
      *                Because of the empty line at the end of the file,
      *                that line has to be discarded
                       MOVE 'Y' TO CANDIDATE-EOF
                       PERFORM REOPEN-CANDIDATE-FILE
               END-READ
           END-IF.

       CALCULATE-CANDIDATE-SCORE.
           PERFORM CHECK-REQ-SKILLS.
           IF NOT MATCHED-SKILLS = 3 THEN
               EXIT PARAGRAPH
           END-IF

           PERFORM CHECK-OPT-SKILLS.
           PERFORM CHECK-PREFERENCES.

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

      *Check candidate's preference and add the preference_score   
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

      *Reset variables used for calculating score
       RESET-SCORE-VARIABLES.
           MOVE 1 TO SCORES.
           MOVE 0 TO MATCHED-SKILLS.

      *Reset the ranking of candidates
       RESET-CANDIDATES.
           INITIALIZE COURSE-CANDIDATES REPLACING
           ALPHANUMERIC DATA BY '0000000000 '
           NUMERIC DATA BY 0.
           
      *Insert candidate by insertion sort    
       INSERT-CANDIDATE.
           IF SCORES >= COURSE-CANDIDATE-SCORE(1) THEN
      *        Empty slot    
               IF COURSE-CANDIDATE-SCORE(1) = 0 THEN
                   MOVE SID TO COURSE-SID(1)
                   MOVE SCORES TO COURSE-CANDIDATE-SCORE(1)
                   EXIT PARAGRAPH
               END-IF
      *        A candidate has a higher score or
      *        same score but a smaller SID
               IF SCORES > COURSE-CANDIDATE-SCORE(1) OR
                  SID < COURSE-SID(1) THEN
                   MOVE 1 TO IDX
                   PERFORM SWAP-CANDIDATE
                   PERFORM INSERT-CANDIDATE
                   EXIT PARAGRAPH
               END-IF
           END-IF.
           IF SCORES >= COURSE-CANDIDATE-SCORE(2) THEN
               IF COURSE-CANDIDATE-SCORE(2) = 0 THEN
                   MOVE SID TO COURSE-SID(2)
                   MOVE SCORES TO COURSE-CANDIDATE-SCORE(2)
                   EXIT PARAGRAPH
               END-IF

               IF SCORES > COURSE-CANDIDATE-SCORE(2) OR
                  SID < COURSE-SID(2) THEN
                   MOVE 2 TO IDX
                   PERFORM SWAP-CANDIDATE
                   PERFORM INSERT-CANDIDATE
                   EXIT PARAGRAPH
               END-IF
           END-IF.
           IF SCORES >= COURSE-CANDIDATE-SCORE(3) THEN
               IF COURSE-CANDIDATE-SCORE(3) = 0 THEN
                   MOVE SID TO COURSE-SID(3)
                   MOVE SCORES TO COURSE-CANDIDATE-SCORE(3)
                   EXIT PARAGRAPH
               END-IF

               IF SCORES > COURSE-CANDIDATE-SCORE(3) OR
                  SID < COURSE-SID(3) THEN
                   MOVE 3 TO IDX
                   PERFORM SWAP-CANDIDATE
                   PERFORM INSERT-CANDIDATE
                   EXIT PARAGRAPH
               END-IF
           END-IF.

      *Swap a higher score candidate with an inserted lower score
      *candidate
       SWAP-CANDIDATE.
           MOVE COURSE-SID(IDX) TO COURSE-SID(4).
           MOVE COURSE-CANDIDATE-SCORE(IDX) TO 
                COURSE-CANDIDATE-SCORE(4).
           MOVE SID TO COURSE-SID(IDX).
           MOVE SCORES TO COURSE-CANDIDATE-SCORE(IDX).
           MOVE COURSE-SID(4) TO SID.
           MOVE COURSE-CANDIDATE-SCORE(4) TO SCORES.

       WRITE-TO-OUTPUT.
           MOVE COURSE-ID TO RESULT-COURSE-ID.
           MOVE COURSE-SIDS TO RESULT-SIDS.
           MOVE X'0A' to RESULT-EOL.
           OPEN EXTEND OUTPUT-FILE.
               WRITE RESULT
               END-WRITE.
           CLOSE OUTPUT-FILE.
