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
char* parse_skill(char **ptr);

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

        char *ptr = line;
        parse_instructors_line(line);
        int id = parse_course_id(&ptr);
        char skill[15];
        // TODO: Add'\0'
        strcpy(skill, parse_skill(&ptr));
        char skill2[15];
        strcpy(skill2, parse_skill(&ptr));
        printf("hey");
        
        break;
    }

    fclose(file);
}

struct Instructors parse_instructors_line(const char *line) {
    printf("Parsing Line!\n");
//    Instructors *course = malloc(sizeof(Instructors));
}

// Read the Course ID and move to the next slot
int parse_course_id(char **ptr) {
    int course_id = (int) strtoul(*ptr, ptr, 10);
    (*ptr)++;

    return course_id;
}

char* parse_skill(char **ptr) {
    char *temp_ptr = (*ptr);
    (*ptr) += 15;

    return temp_ptr;
}
