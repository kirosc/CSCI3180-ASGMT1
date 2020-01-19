#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INSTRUCTOR_LINE_SIZE  126
#define CANDIDATE_LINE_SIZE  146
#define SKILL_STRING_SIZE  16
#define MAX_REQUIRED_SKILLS  3
#define MAX_OPTIONAL_SKILLS  5
#define MAX_CANDIDATE_SKILLS  8
#define MAX_PREFERENCE  3
#define PREFERENCE1_SCORE  1.5
#define PREFERENCE2_SCORE  1
#define PREFERENCE3_SCORE  0.5

typedef struct Instructor {
    int id;
    const char *required_skills[MAX_REQUIRED_SKILLS];
    const char *optional_skills[MAX_OPTIONAL_SKILLS];
} Instructor;

typedef struct Candidate {
    int id;
    const char *skills[MAX_CANDIDATE_SKILLS];
    int preference[MAX_PREFERENCE];
} Candidate;

FILE *read_file(const char* name);

Instructor **read_instructors_file();

struct Instructor *parse_instructors_line(char *line);

int parse_course_id(char **ptr);

const char *parse_skill(char **ptr);

char *copy_from(const char *source, int size);

Candidate **read_candidates_file();

Candidate *parse_candidate_line(char *line);

int number_of_course, number_of_candidate;

int main() {
    Instructor **courses = NULL;
    Candidate **candidates = NULL;
    courses = read_instructors_file();
    candidates = read_candidates_file();

    return 0;
}

FILE *read_file(const char* name) {
    FILE *file = fopen(name, "r");

    if (file == NULL) {
        printf("%s", "non-existing file!");
        exit(-1);
    }

    return file;
}

Instructor **read_instructors_file() {
    // TODO: Remove .. when submit
    FILE *file = read_file("../instructors.txt");

    char line[INSTRUCTOR_LINE_SIZE];
    Instructor **courses = NULL;

    while (fgets(line, sizeof(line), file)) {
        // Encounter carriage return (Windows)
        if (line[0] == '\r' && line[1] == '\n') {
            continue;
        }

        number_of_course++;
        courses = realloc(courses, number_of_course * sizeof(Instructor *));
        courses[number_of_course - 1] = parse_instructors_line(line);
    }

    fclose(file);

    return courses;
}

Instructor *parse_instructors_line(char *line) {
    Instructor *course = malloc(sizeof(Instructor));
    char *ptr = line;

    int id = parse_course_id(&ptr);

    course->id = id;
    for (int i = 0; i < MAX_REQUIRED_SKILLS; ++i) {
        course->required_skills[i] = parse_skill(&ptr);
    }
    for (int i = 0; i < MAX_OPTIONAL_SKILLS; ++i) {
        course->optional_skills[i] = parse_skill(&ptr);
    }

    return course;
}

Candidate **read_candidates_file() {
    // TODO: Remove .. when submit
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
        candidates[number_of_course - 1] = parse_candidate_line(line);
    }

    fclose(file);

    return candidates;
}

Candidate *parse_candidate_line(char *line) {
    return NULL;
}


// Read the Course ID and move to the next slot
int parse_course_id(char **ptr) {
    int course_id = (int) strtoul(*ptr, ptr, 10);
    (*ptr)++;

    return course_id;
}

const char *parse_skill(char **ptr) {
    const char *skill = copy_from((*ptr), SKILL_STRING_SIZE);
    (*ptr) += SKILL_STRING_SIZE - 1;

    return skill;
}

// Copy string and pad a terminator
char *copy_from(const char *source, int size) {
    char *destination = malloc(sizeof(char) * size);
    strncpy(destination, source, size - 1);
    destination[size] = '\0';

    return destination;
}