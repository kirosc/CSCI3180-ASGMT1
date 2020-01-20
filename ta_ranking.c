#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INSTRUCTOR_LINE_SIZE  126
#define CANDIDATE_LINE_SIZE  147
#define SKILL_SLOT_SIZE  15
#define SKILL_STRING_SIZE  16
#define MAX_REQUIRED_SKILLS  3
#define MAX_OPTIONAL_SKILLS  5
#define MAX_CANDIDATE_SKILLS  8
#define MAX_PREFERENCE  3
#define PREFERENCE1_SCORE  1.5
#define PREFERENCE2_SCORE  1
#define PREFERENCE3_SCORE  0.5

typedef struct Instructors {
    int id;
    const char *required_skills[MAX_REQUIRED_SKILLS];
    const char *optional_skills[MAX_OPTIONAL_SKILLS];
} Instructors;

typedef struct Candidate {
    int sid;
    char skills[MAX_CANDIDATE_SKILLS * SKILL_SLOT_SIZE + 1];
    int preference[MAX_PREFERENCE];
} Candidate;

FILE *read_file(const char *name);

Instructors **read_instructors_file();

struct Instructors *parse_instructor_line(char *ptr);

const char *parse_instructor_skill(char **ptr);

char *copy_from(const char *source, int size);

Candidate **read_candidates_file();

const char *parse_candidate_skills(char **ptr);

Candidate *parse_candidate_line(char *ptr);

int parse_number(char **ptr);

int number_of_course, number_of_candidate;

int main() {
    Instructors **courses = NULL;
    Candidate **candidates = NULL;
    courses = read_instructors_file();
    candidates = read_candidates_file();

    return 0;
}

// Read a file and return the file pointer
FILE *read_file(const char *name) {
    FILE *file = fopen(name, "r");

    if (file == NULL) {
        printf("%s", "non-existing file!");
        exit(-1);
    }

    return file;
}

// Read the instructors.txt and return an array of Instructors
Instructors **read_instructors_file() {
    // TODO: Remove .. when submit
    FILE *file = read_file("../instructors.txt");

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
    Instructors *course = malloc(sizeof(Instructors));

    course->id = parse_number(&ptr);
    for (int i = 0; i < MAX_REQUIRED_SKILLS; ++i) {
        course->required_skills[i] = parse_instructor_skill(&ptr);
    }
    for (int i = 0; i < MAX_OPTIONAL_SKILLS; ++i) {
        course->optional_skills[i] = parse_instructor_skill(&ptr);
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
    FILE *file = read_file("../candidates.txt");

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

    return candidates;
}

// Parse a line and return a Candidate struct
Candidate *parse_candidate_line(char *ptr) {
    Candidate *candidate = malloc(sizeof(Candidate));

    candidate->sid = parse_number(&ptr);

    strcpy(candidate->skills, parse_candidate_skills(&ptr));

    for (int i = 0; i < MAX_CANDIDATE_SKILLS; ++i) {
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

// Copy a number of characters and pad a null character
char *copy_from(const char *source, int size) {
    char *destination = malloc(sizeof(char) * size);
    strncpy(destination, source, size - 1);
    destination[size] = '\0';

    return destination;
}