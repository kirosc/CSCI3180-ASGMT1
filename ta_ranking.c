#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INSTRUCTOR_LINE_SIZE  126
#define SKILL_SIZE  16
#define MAX_REQUIRED_SKILLS  3
#define MAX_OPTIONAL_SKILLS  5
#define MAX_CANDIDATE_SKILLS  8
#define MAX_PREFERENCE  3
#define PREFERENCE1_SCORE  1.5
#define PREFERENCE2_SCORE  1
#define PREFERENCE3_SCORE  0.5

typedef struct Instructors {
    int id;
    char required_skills[MAX_REQUIRED_SKILLS][SKILL_SIZE];
    char optional_skills[MAX_OPTIONAL_SKILLS][SKILL_SIZE];
    struct Instructors *next;
} Instructors;

typedef struct Candidate {
    int id;
    char skills[MAX_CANDIDATE_SKILLS][SKILL_SIZE];
    int preference[MAX_PREFERENCE];
} Candidate;

void read_instructors_file();
struct Instructors parse_instructors_line(const char line[]);
int parse_course_id(char **ptr);
void parse_skill(char *destination, char **ptr);
void copy(char *destination, const char* source, int size);

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

    while (fgets(line, sizeof(line), file)) {
        // Encounter carriage return (Windows)
        if (line[0] == '\r' && line[1] == '\n') {
            continue;
        }

        parse_instructors_line(line);
        break;
    }

    fclose(file);
}

struct Instructors parse_instructors_line(const char *line) {
    printf("Parsing Line!\n");
//    Instructors *course = malloc(sizeof(Instructors));
    char *ptr = line;

    int id = parse_course_id(&ptr);
    printf("foo");
    char skill1[SKILL_SIZE];
    parse_skill(skill1, &ptr);
    printf("%s", skill1);
}

// Read the Course ID and move to the next slot
int parse_course_id(char **ptr) {
    int course_id = (int) strtoul(*ptr, ptr, 10);
    (*ptr)++;

    return course_id;
}

void parse_skill(char *destination, char **ptr) {
    copy(destination, (*ptr), SKILL_SIZE);
    // Pointer change after return?
    printf("voo");
//    (*ptr) += 15;
}

// Copy string and pad a terminator
void copy(char *destination, const char* source, int size) {
    strncpy(destination, source, size - 1);
    printf("voo");
    destination[size] = (char) "\0";
}