#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define MAX_FEATURES 100
#define MAX_CLASSES 100

typedef struct {
    int num_features;
    int num_classes;
    int num_samples;
    int **data;
    int *labels;
    char **feature_names;
    char **class_names;
} Dataset;

typedef struct Node {
    int feature_index;
    int class_label;
    struct Node **children;
    int num_children;
} Node;

double calculate_entropy(int *labels, int num_samples, int num_classes) {
    int class_counts[MAX_CLASSES] = {0};
    for (int i = 0; i < num_samples; i++) {
        class_counts[labels[i]]++;
    }
    double entropy = 0.0;
    for (int i = 0; i < num_classes; i++) {
        if (class_counts[i] > 0) {
            double p = (double)class_counts[i] / num_samples;
            entropy -= p * log2(p);
        }
    }
    return entropy;
}

double calculate_information_gain(Dataset *dataset, int *indices, int num_samples, int feature_index) {
    int left_counts[MAX_CLASSES] = {0};
    int right_counts[MAX_CLASSES] = {0};
    int left_total = 0, right_total = 0;
    
    for (int i = 0; i < num_samples; i++) {
        if (dataset->data[indices[i]][feature_index] == 0) {
            left_counts[dataset->labels[indices[i]]]++;
            left_total++;
        } else {
            right_counts[dataset->labels[indices[i]]]++;
            right_total++;
        }
    }
    
    double total_entropy = calculate_entropy(dataset->labels, num_samples, dataset->num_classes);
    double left_entropy = calculate_entropy(left_counts, left_total, dataset->num_classes);
    double right_entropy = calculate_entropy(right_counts, right_total, dataset->num_classes);
    
    double left_weight = (double)left_total / num_samples;
    double right_weight = (double)right_total / num_samples;
    
    double information_gain = total_entropy - (left_weight * left_entropy + right_weight * right_entropy);
    return information_gain;
}

Node* id3(Dataset *dataset, int *indices, int num_samples, int num_features) {
    Node *node = (Node*)malloc(sizeof(Node));
    
    // Check if all samples have the same label
    int first_label = dataset->labels[indices[0]];
    int all_same_label = 1;
    for (int i = 1; i < num_samples; i++) {
        if (dataset->labels[indices[i]] != first_label) {
            all_same_label = 0;
            break;
        }
    }
    if (all_same_label) {
        node->feature_index = -1;
        node->class_label = first_label;
        node->children = NULL;
        node->num_children = 0;
        return node;
    }
    
    // Find the best feature to split on
    int best_feature = -1;
    double best_gain = 0.0;
    for (int i = 0; i < num_features; i++) {
        double gain = calculate_information_gain(dataset, indices, num_samples, i);
        if (gain > best_gain) {
            best_gain = gain;
            best_feature = i;
        }
    }
    
    if (best_feature == -1) {
        // No feature provides any gain, return a leaf node with the most common class label
        int class_counts[MAX_CLASSES] = {0};
        for (int i = 0; i < num_samples; i++) {
            class_counts[dataset->labels[indices[i]]]++;
        }
        int max_count = 0;
        for (int i = 0; i < dataset->num_classes; i++) {
            if (class_counts[i] > max_count) {
                max_count = class_counts[i];
                node->class_label = i;
            }
        }
        node->feature_index = -1;
        node->children = NULL;
        node->num_children = 0;
        return node;
    }
    
    // Split the dataset and create child nodes
    node->feature_index = best_feature;
    node->children = (Node**)malloc(2 * sizeof(Node*));
    node->num_children = 2;
    
    int *left_indices = (int*)malloc(num_samples * sizeof(int));
    int *right_indices = (int*)malloc(num_samples * sizeof(int));
    int left_count = 0, right_count = 0;
    
    for (int i = 0; i < num_samples; i++) {
        if (dataset->data[indices[i]][best_feature] == 0) {
            left_indices[left_count++] = indices[i];
        } else {
            right_indices[right_count++] = indices[i];
        }
    }
    
    node->children[0] = id3(dataset, left_indices, left_count, num_features);
    node->children[1] = id3(dataset, right_indices, right_count, num_features);
    
    free(left_indices);
    free(right_indices);
    
    return node;
}

void print_tree(Node *node, Dataset *dataset, int depth) {
    if (node->feature_index == -1) {
        printf("%*sClass: %s\n", depth * 2, "", dataset->class_names[node->class_label]);
    } else {
        printf("%*sFeature: %s\n", depth * 2, "", dataset->feature_names[node->feature_index]);
        for (int i = 0; i < node->num_children; i++) {
            print_tree(node->children[i], dataset, depth + 1);
        }
    }
}

int main() {
    // Initialize a simple dataset (this part should be replaced with actual data loading)
    Dataset dataset;
    dataset.num_features = 2;
    dataset.num_classes = 2;
    dataset.num_samples = 4;
    
    dataset.data = (int**)malloc(dataset.num_samples * sizeof(int*));
    for (int i = 0; i < dataset.num_samples; i++) {
        dataset.data[i] = (int*)malloc(dataset.num_features * sizeof(int));
    }
    dataset.data[0][0] = 0; dataset.data[0][1] = 0; // Feature values for sample 1
    dataset.data[1][0] = 0; dataset.data[1][1] = 1; // Feature values for sample 2
    dataset.data[2][0] = 1; dataset.data[2][1] = 0; // Feature values for sample 3
    dataset.data[3][0] = 1; dataset.data[3][1] = 1; // Feature values for sample 4
    
    dataset.labels = (int*)malloc(dataset.num_samples * sizeof(int));
    dataset.labels[0] = 0; // Class label for sample 1
    dataset.labels[1] = 0; // Class label for sample 2
    dataset.labels[2] = 1; // Class label for sample 3
    dataset.labels[3] = 1; // Class label for sample 4
    
    dataset.feature_names = (char**)malloc(dataset.num_features * sizeof(char*));
    dataset.feature_names[0] = "Feature 1";
    dataset.feature_names[1] = "Feature 2";
    
    dataset.class_names = (char**)malloc(dataset.num_classes * sizeof(char*));
    dataset.class_names[0] = "Class 0";
    dataset.class_names[1] = "Class 1";
    
    int *indices = (int*)malloc(dataset.num_samples * sizeof(int));
    for (int i = 0; i < dataset.num_samples; i++) {
        indices[i] = i;
    }
    
    Node *root = id3(&dataset, indices, dataset.num_samples, dataset.num_features);
    print_tree(root, &dataset, 0);
    
    // Free allocated memory (add appropriate memory management here)
    
    return 0;
}




