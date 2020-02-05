#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INSTRUCTOR_LINE_SIZE  126
#define COURSE_SLOT_SIZE 5
#define COURSE_STRING_SIZE 6
#define CANDIDATE_LINE_SIZE  147
#define SKILL_SLOT_SIZE  15
#define SKILL_STRING_SIZE  16
#define SID_SLOT_SIZE  11
#define SID_STRING_SIZE  12
#define MAX_REQUIRED_SKILLS  3
#define MAX_OPTIONAL_SKILLS  5
#define MAX_CANDIDATE_SKILLS  8
#define MAX_PREFERENCE  3
#define MAX_RANK  3
#define PREFERENCE1_SCORE  1.5f
#define PREFERENCE2_SCORE  1.0f
#define PREFERENCE3_SCORE  0.5f
#define NO_CANDIDATE  "0000000000 "

typedef struct Instructors {
    int id;
    const char *required_skills[MAX_REQUIRED_SKILLS];
    const char *optional_skills[MAX_OPTIONAL_SKILLS];
    float candidate_score[MAX_RANK];
    char candidate_sid[MAX_RANK][SID_STRING_SIZE];
} Instructors;

typedef struct Candidate {
    int sid;
    char skills[MAX_CANDIDATE_SKILLS * SKILL_SLOT_SIZE + 1];
    int preference[MAX_PREFERENCE];
} Candidate;

FILE *open_file(const char *name, const char *mode, const char *error_message);

Instructors **read_instructors_file();

struct Instructors *parse_instructor_line(char *ptr);

Instructors *initialize_instructor();

const char *parse_instructor_skill(char **ptr);

Candidate **read_candidates_file();

Candidate *parse_candidate_line(char *ptr);

const char *parse_candidate_skills(char **ptr);

int parse_number(char **ptr);

char *copy_from(const char *source, int size);

void rank_candidates(Instructors *course, const Candidate **candidates);

int is_satisfy_required_skills(Instructors *course, const Candidate *candidate);

float calculate_score(const Instructors *course, const Candidate *candidate);

void insert_candidate(Instructors *course, float score, char *sid);

char *get_rank_result(Instructors *course);

void write_output_file(Instructors **courses);

// Global variables
int number_of_course, number_of_candidate;

int main() {
    Instructors **courses = NULL;
    Candidate **candidates = NULL;

    courses = read_instructors_file();
    candidates = read_candidates_file();

    // Rank the candidates for each course
    for (int i = 0; i < number_of_course; ++i) {
        rank_candidates(courses[i], (const Candidate **) candidates);
    }

    write_output_file(courses);

    return 0;
}

// Read a file and return the file pointer
FILE *open_file(const char *name, const char *mode, const char *error_message) {
    FILE *file = fopen(name, mode);

    if (file == NULL) {
        printf("%s", error_message);
        exit(-1);
    }

    return file;
}

// Read the instructors.txt and return an array of Instructors
Instructors **read_instructors_file() {
    // TODO: Remove .. when submit
    FILE *file = open_file("instructors.txt", "r", "non-existing file!");

    char line[INSTRUCTOR_LINE_SIZE];
    Instructors **courses = NULL;

    while (fgets(line, sizeof(line), file)) {
        // Encounter carriage return (Windows)
        if (line[0] == '\r' && line[1] == '\n') {
            continue;
        }

        number_of_course++;
        courses = realloc(courses, number_of_course * sizeof(Instructors *));
        courses[number_of_course - 1] = parse_instructor_line(line);
    }

    fclose(file);

    return courses;
}

// Parse a line and return an Instructors struct
Instructors *parse_instructor_line(char *ptr) {
    Instructors *course = initialize_instructor();

    course->id = parse_number(&ptr);
    for (int i = 0; i < MAX_REQUIRED_SKILLS; ++i) {
        course->required_skills[i] = parse_instructor_skill(&ptr);
    }
    for (int i = 0; i < MAX_OPTIONAL_SKILLS; ++i) {
        course->optional_skills[i] = parse_instructor_skill(&ptr);
    }

    return course;
}

// Initialize an Instructor and the Rank-k TA
Instructors *initialize_instructor() {
    Instructors *course = malloc(sizeof(Instructors));
    for (int i = 0; i < MAX_RANK; ++i) {
        strcpy(course->candidate_sid[i], NO_CANDIDATE);
    }

    return course;
}

// Parse one candidate skill and return a string
const char *parse_instructor_skill(char **ptr) {
    const char *skill = copy_from((*ptr), SKILL_STRING_SIZE);
    (*ptr) += SKILL_STRING_SIZE - 1;

    return skill;
}

// Read the candidates.txt and return an array of Candidate
Candidate **read_candidates_file() {
    FILE *file = open_file("candidates.txt", "r", "non-existing file!");

    char line[CANDIDATE_LINE_SIZE];
    Candidate **candidates = NULL;

    while (fgets(line, sizeof(line), file)) {
        // Encounter carriage return (Windows)
        if (line[0] == '\r' && line[1] == '\n') {
            continue;
        }

        number_of_candidate++;
        candidates = realloc(candidates, number_of_candidate * sizeof(Candidate *));
        candidates[number_of_candidate - 1] = parse_candidate_line(line);
    }

    fclose(file);

    return candidates;
}

// Parse a line and return a Candidate struct
Candidate *parse_candidate_line(char *ptr) {
    Candidate *candidate = malloc(sizeof(Candidate));

    candidate->sid = parse_number(&ptr);

    const char *skills = parse_candidate_skills(&ptr);
    strcpy(candidate->skills, skills);
    free(skills);

    for (int i = 0; i < MAX_PREFERENCE; ++i) {
        candidate->preference[i] = parse_number(&ptr);
    }

    return candidate;
}

// Parse all candidate skills and return in a single string
const char *parse_candidate_skills(char **ptr) {
    const int CHARS_TO_READ = MAX_CANDIDATE_SKILLS * SKILL_SLOT_SIZE + 1;

    const char *skills = copy_from((*ptr), CHARS_TO_READ);
    (*ptr) += CHARS_TO_READ - 1;

    return skills;
}

// Read the a number from a string and move to the next slot
int parse_number(char **ptr) {
    int number = (int) strtoul(*ptr, ptr, 10);
    (*ptr)++; // Skipping the last space

    return number;
}

// Copy a number of characters and pad a null character at the end
char *copy_from(const char *source, int size) {
    char *destination = malloc(sizeof(char) * size);
    strncpy(destination, source, size - 1);
    destination[size] = '\0';

    return destination;
}

// Rank the TA of a course
void rank_candidates(Instructors *course, const Candidate **candidates) {
    for (int i = 0; i < number_of_candidate; ++i) {
        if (is_satisfy_required_skills(course, candidates[i])) {
            // Get the TA SID and score
            float score = calculate_score(course, candidates[i]);
            char sid[SID_STRING_SIZE];
            sprintf(sid, "%d ", candidates[i]->sid);

            insert_candidate(course, score, sid);
        }
    }
}

// Check if a candidate satisfy all required skills
int is_satisfy_required_skills(Instructors *course, const Candidate *candidate) {
    // Check if all required skills are substrings of candidate's skills
    if (strstr(candidate->skills, course->required_skills[0]) != NULL &&
        strstr(candidate->skills, course->required_skills[1]) != NULL &&
        strstr(candidate->skills, course->required_skills[2]) != NULL) {
        return 1;
    }

    return 0;
}

// Calculate the score of a candidate with reference to a course
float calculate_score(const Instructors *course, const Candidate *candidate) {
    float score = 1.0f;

    // Check optional skills
    for (int i = 0; i < MAX_OPTIONAL_SKILLS; ++i) {
        if (strstr(candidate->skills, course->optional_skills[i]) != NULL) {
            score++;
        }
    }

    // Check preference
    if (course->id == candidate->preference[0]) {
        score += PREFERENCE1_SCORE;
    } else if (course->id == candidate->preference[1]) {
        score += PREFERENCE2_SCORE;
    } else if (course->id == candidate->preference[2]) {
        score += PREFERENCE3_SCORE;
    }

    return score;
}

// Insert a qualified candidate to the right spot in descending order by score
void insert_candidate(Instructors *course, float score, char *sid) {
    for (int i = 0; i < MAX_RANK; ++i) {
        if (score > course->candidate_score[i]) {
            if (course->candidate_score[i] == 0.0f) {
                // Empty slot
                course->candidate_score[i] = score;
                strcpy(course->candidate_sid[i], sid);
            } else {
                // The original TA in this spot has a lower score now
                // Store it in a temporary buffer
                float temp_score = course->candidate_score[i];
                char temp_sid[SID_STRING_SIZE];
                strcpy(temp_sid, course->candidate_sid[i]);

                course->candidate_score[i] = score;
                strcpy(course->candidate_sid[i], sid);

                // Find a new spot for the original TA
                insert_candidate(course, temp_score, temp_sid);
            }

            break;
        }
    }
}

// Get the output string of a course TA ranking result
char *get_rank_result(Instructors *course) {
    char course_id[COURSE_STRING_SIZE];
    sprintf(course_id, "%d ", course->id);
    char *result = malloc(sizeof(char) * (COURSE_SLOT_SIZE + 3 * SID_SLOT_SIZE));
    result[0] = '\0';   // Ensure the memory is an empty string

    strcat(result, course_id);
    strcat(result, course->candidate_sid[0]);
    strcat(result, course->candidate_sid[1]);
    strcat(result, course->candidate_sid[2]);

    return result;
}

// Write the ranking result to output.txt
void write_output_file(Instructors **courses) {
    FILE *file = open_file("output.txt", "w", "error on writing file!");

    for (int i = 0; i < number_of_course; ++i) {
        char *result = get_rank_result(courses[i]);
        fprintf(file, "%s\n", result);
    }

    fclose(file);
}
