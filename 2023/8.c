// gcc -std=c11 -o 8 8.c -pthread -lm -O3 -march=native -mtune=native
#define VECTOR_IMPLEMENTATION
#include "vector.h"
#define FMT_LOCKED_DEFAULT_PRINTERS
#define FMT_IMPLEMENTATION
#include "fmt.h"
#include <pthread.h>

enum direction {
    LEFT,
    RIGHT,
} __attribute__((packed));

struct step_iterator {
    VECTOR(enum direction) steps;
    size_t state;
};

struct step_iterator step_iterator_new(VECTOR(char) steps_text) {
    size_t count = vector__size(steps_text);
    VECTOR(enum direction) steps = vector_create(enum direction, count);
    vector__size(steps) = count;
    for (size_t i = 0; i < count; i++) {
        steps[i] = steps_text[i] == 'L' ? LEFT : RIGHT;
    }
    return (struct step_iterator) {
        .steps = steps,
        .state = 0,
    };
}

/** Creates a new step iterator referencing the step sequence of SELF and
    cloning its current state. */
struct step_iterator step_iterator_ref(struct step_iterator *self) {
    return (struct step_iterator) {
        .steps = self->steps,
        .state = self->state,
    };
}

void step_iterator_drop(struct step_iterator *self) {
    vector_free(self->steps);
}

enum direction step_iterator_next(struct step_iterator *self) {
    return self->steps[self->state++ % vector__size(self->steps)];
}

struct tag {
    char tag[3];
};

struct tag tag_new(const char *chars) {
    return (struct tag) {
        .tag = {chars[0], chars[1], chars[2]},
    };
}

bool tag_eq(struct tag self, struct tag other) {
    return self.tag[0] == other.tag[0] &&
           self.tag[1] == other.tag[1] &&
           self.tag[2] == other.tag[2];
}

bool tag_ends_with(struct tag self, char ch) {
    return self.tag[2] == ch;
}

bool tag_is_empty(struct tag tag) {
    static const struct tag EMPTY = {{'\0', '\0', '\0'}};
    return tag_eq(tag, EMPTY);
}

struct node {
    struct node *conns[2];
    struct tag tag;
};

struct node node_new(struct tag tag) {
    return (struct node) {
        .tag = tag,
        .conns = {NULL, NULL},
    };
}

struct fmt_String_Take node_format(const struct node *self) {
    return fmt_format(
        "node({:.3}, left={:.3}, right={:.3})",
        self->tag.tag,
        self->conns[LEFT]->tag.tag,
        self->conns[RIGHT]->tag.tag
    ).take;
}

struct node_map {
    struct node slots[26*26*26];
};

struct node_map * node_map_new() {
    struct node_map *self = malloc(sizeof(struct node_map));
    memset(self, 0, sizeof(struct node_map));
    return self;
}

void node_map_drop(struct node_map *self) {
    free(self);
}

struct node * node_map_slot(struct node_map *self, struct tag tag) {
    const size_t index = (tag.tag[0] - 'A') * 26 * 26 +
                         (tag.tag[1] - 'A') * 26 +
                         (tag.tag[2] - 'A');
    return &self->slots[index];
}

struct node * node_map_get(struct node_map *self, struct tag tag) {
    struct node *const maybe_node = node_map_slot(self, tag);
    if (tag_is_empty(maybe_node->tag)) {
        return NULL;
    } else {
        return maybe_node;
    }
}

bool node_map_contains(struct node_map *self, struct tag tag) {
    return node_map_get(self, tag) != NULL;
}

struct node * node_map_insert(struct node_map *self, struct node node) {
    struct node *const slot = node_map_slot(self, node.tag);
    *slot = node;
    return slot;
}

struct parse_node_result {
    struct tag node;
    struct tag left;
    struct tag right;
};

struct parse_node_result parse_node(VECTOR(char) line) {
    struct parse_node_result result;
    result.node = tag_new(line);
    result.left = tag_new(line + 7);
    result.right = tag_new(line + 12);
    return result;
}

bool read_line (FILE *fp, VECTOR(char) *line) {
  vector_clear (*line);
  bool eof = false;
  for (;;) {
    const char ch = fgetc (fp);
    if (ch == EOF) {
      eof = vector_empty (*line);
      break;
    } else if (ch == '\n') {
      break;
    } else {
      vector_push (*line, ch);
    }
  }
  vector_push (*line, '\0');
  --vector__size (*line);
  return !eof;
}

size_t count_steps_until(
    struct node *start,
    struct tag end_tag,
    struct step_iterator steps
) {
    struct node *node = start;
    for (;;) {
        node = node->conns[step_iterator_next(&steps)];
        if (tag_eq(node->tag, end_tag)) {
            break;
        }
    }
    return steps.state;
}

size_t count_steps_until_any(struct node *start,
    char end_tag_suffix,
    struct step_iterator steps
) {
    struct node *node = start;
    for (;;) {
        node = node->conns[step_iterator_next(&steps)];
        if (tag_ends_with(node->tag, end_tag_suffix)) {
            return steps.state;
        }
    }
}

struct worker_args {
    struct node *start;
    struct step_iterator steps;
};

void * count_steps_until_z_worker(void *args) {
    struct worker_args *const a = args;
    const size_t result = count_steps_until_any(a->start, 'Z', a->steps);
    free(a);
    return (void *)result;
}

size_t lcm(size_t a, size_t b) {
    size_t x = a;
    size_t y = b;
    while (x != y) {
        if (x < y) {
            x += a;
        } else {
            y += b;
        }
    }
    return x;
}

int main(void) {
    fmt_init_threading();
    VECTOR(char) input = vector_create(char, 290);

    read_line(stdin, &input);
    struct step_iterator steps = step_iterator_new(input);
    // skip empty line
    (void)fgetc(stdin);

    // nodes for part 2
    VECTOR(struct node *) nodes = vector_create(struct node *, 16);

    struct node_map *nodemap = node_map_new();
    VECTOR(struct parse_node_result) node_descriptions
        = vector_create(struct parse_node_result, 100);
    while (read_line(stdin, &input)) {
        struct parse_node_result result = parse_node(input);
        struct node *node = node_map_insert(nodemap, node_new(result.node));
        vector_push(node_descriptions, result);
        if (tag_ends_with(result.node, 'A')) {
            vector_push(nodes, node);
        }
    }
    vector_for_each(node_descriptions, it) {
        struct node *node = node_map_get(nodemap, it->node);
        node->conns[LEFT] = node_map_get(nodemap, it->left);
        node->conns[RIGHT] = node_map_get(nodemap, it->right);
    }
    vector_free(input);
    vector_free(node_descriptions);

    const size_t x = count_steps_until(
        node_map_get(nodemap, tag_new("AAA")),
        tag_new("ZZZ"),
        step_iterator_ref(&steps)
    );
    fmt_println("Went from AAA to ZZZ in \x1b[92m{}\x1b[m steps", x);

    VECTOR(pthread_t) threads = vector_create(pthread_t, vector__size(nodes));
    vector_for_each(nodes, it) {
        struct worker_args *args = malloc(sizeof(struct worker_args));
        args->start = *it;
        args->steps = step_iterator_ref(&steps);
        pthread_t tid = 0;
        pthread_create(&tid, NULL, count_steps_until_z_worker, args);
        vector_push(threads, tid);
    }
    size_t total_steps = 0;
    vector_for_each(threads, it) {
        size_t steps;
        pthread_join(*it, (void **)&steps);
        if (total_steps) {
            total_steps = lcm(total_steps, steps);
        } else {
            total_steps = steps;
        }
    }
    fmt_println("It took \x1b[92m{}\x1b[m steps for all nodes", total_steps);

    vector_free(threads);
    vector_free(nodes);
    node_map_drop(nodemap);
    step_iterator_drop(&steps);
}

// gcc -std=c11 -o 8 8.c -pthread -lm -O3 -march=native -mtune=native
