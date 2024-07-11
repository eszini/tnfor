#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

// Function to count special characters in a string
int count_special_chars(const char *str) {
    int count = 0;
    while (*str) {
        if (!isalnum(*str)) {
            count++;
        }
        str++;
    }
    return count;
}

// Function to split string into substrings using special characters as delimiters
char **split_into_substrings(const char *str, int *count) {
    char *temp = strdup(str);
    char *token = strtok(temp, "_=+/*");
    char **substrings = NULL;
    *count = 0;

    while (token) {
        substrings = realloc(substrings, sizeof(char*) * (*count + 1));
        substrings[*count] = strdup(token);
        (*count)++;
        token = strtok(NULL, "_=+/*");
    }
    free(temp);
    return substrings;
}

// Function to calculate the similarity score between two strings
double calculate_similarity(const char *str1, const char *str2) {
    // Weights for different conditions
    double weight_length_difference = 0.3;
    double weight_special_chars = 0.3;
    double weight_substrings = 0.4;

    // Length difference
    int len1 = strlen(str1);
    int len2 = strlen(str2);
    double length_difference_score = 1.0 - (double)abs(len1 - len2) / ((len1 + len2) / 2.0);

    // Special characters comparison
    int special_count1 = count_special_chars(str1);
    int special_count2 = count_special_chars(str2);
    double special_chars_score = 1.0 - (double)abs(special_count1 - special_count2) / ((special_count1 + special_count2) / 2.0);

    // Substrings comparison
    int substr_count1, substr_count2;
    char **substrings1 = split_into_substrings(str1, &substr_count1);
    char **substrings2 = split_into_substrings(str2, &substr_count2);
    int matching_substrings = 0;
    for (int i = 0; i < substr_count1 && i < substr_count2; i++) {
        if (strcmp(substrings1[i], substrings2[i]) == 0) {
            matching_substrings++;
        }
    }
    double substrings_score = (double)matching_substrings / ((substr_count1 + substr_count2) / 2.0);

    // Free allocated memory
    for (int i = 0; i < substr_count1; i++) {
        free(substrings1[i]);
    }
    for (int i = 0; i < substr_count2; i++) {
        free(substrings2[i]);
    }
    free(substrings1);
    free(substrings2);

    // Calculate total similarity score
    double total_score = (weight_length_difference * length_difference_score) +
                         (weight_special_chars * special_chars_score) +
                         (weight_substrings * substrings_score);

    return total_score;
}

int main() {
    const char *str1 = "esto_es_una_variable";
    const char *str2 = "esto_es_una_variable2";

    double similarity = calculate_similarity(str1, str2);
    printf("Similarity score: %.2f\n", similarity);

    return 0;
}

