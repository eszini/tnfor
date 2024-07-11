#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LENGTH 1024

int is_programmer_comment(const char *line) {
    const char *keywords[] = {
        "change log", "update", "todo", "msg", "fix", "bug", "issue",
        "initials", "date", "file", "author", "version", "note", "warn", "error"
    };
    size_t keywords_count = sizeof(keywords) / sizeof(keywords[0]);

    // Convert line to lower case for case-insensitive comparison
    char lower_line[MAX_LINE_LENGTH];
    for (int i = 0; line[i]; i++) {
        lower_line[i] = tolower(line[i]);
    }
    lower_line[strlen(line)] = '\0';

    for (size_t i = 0; i < keywords_count; i++) {
        if (strstr(lower_line, keywords[i])) {
            return 1; // It's a programmer comment
        }
    }
    return 0; // It's not a programmer comment
}

int main() {
    FILE *input_file = fopen("z4", "r");
    if (!input_file) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), input_file)) {
        // Trim leading spaces
        char *trimmed_line = line;
        while (isspace(*trimmed_line)) trimmed_line++;

        // Skip empty lines
        if (*trimmed_line == '\0') continue;

        // Determine if the line is a programmer comment or code comment
        if (is_programmer_comment(trimmed_line)) {
            printf("Programmer comment: %s", line);
        } else {
            printf("Code comment: %s", line);
        }
    }

    fclose(input_file);
    return 0;
}

