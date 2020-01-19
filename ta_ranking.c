#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INSTRUCTOR_LINE_SIZE  126
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
    char *required_skills[MAX_REQUIRED_SKILLS];
    char *optional_skills[MAX_OPTIONAL_SKILLS];
    struct Instructors *next;
} Instructors;

typedef struct Candidate {
    int id;
    char skills[MAX_CANDIDATE_SKILLS][SKILL_STRING_SIZE];
    int preference[MAX_PREFERENCE];
} Candidate;

void read_instructors_file();
struct Instructors * parse_instructors_line(const char line[]);
int parse_course_id(char **ptr);
char * parse_skill(char **ptr);
char * copy_from(const char *source, int size);

int main() {
    read_instructors_file();
    return 0;
}

void read_file() {

}

void read_instructors_file() {
    // TODO: Remove .. when submit
    FILE *file = fopen("../instructors.txt", "r");

    if (file == NULL) {
        printf("%s", "non-existing file!");
        exit(-1);
    }

    char line[INSTRUCTOR_LINE_SIZE];
    int number_of_course;
    Instructors** course = NULL;

    while (fgets(line, sizeof(line), file)) {
        // Encounter carriage return (Windows)
        if (line[0] == '\r' && line[1] == '\n') {
            continue;
        }

        number_of_course++;
        course = realloc(course, number_of_course * sizeof(Instructors*));
        *(course + number_of_course - 1) = parse_instructors_line(line);
    }

    fclose(file);
}

struct Instructors * parse_instructors_line(const char *line) {
    printf("Parsing Line!\n");
    Instructors *course = malloc(sizeof(Instructors));
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

// Read the Course ID and move to the next slot
int parse_course_id(char **ptr) {
    int course_id = (int) strtoul(*ptr, ptr, 10);
    (*ptr)++;

    return course_id;
}

char * parse_skill(char **ptr) {
    const char *skill = copy_from((*ptr), SKILL_STRING_SIZE);
    (*ptr) += SKILL_STRING_SIZE - 1;

    return skill;
}

// Copy string and pad a terminator
char * copy_from(const char *source, int size) {
    char * destination = malloc(sizeof(char) * size);
    strncpy(destination, source, size - 1);
    destination[size] = (char) "\0";

    return destination;
}