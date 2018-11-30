#pragma once
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

// Vector is an ordered dynamic array of pointers.
typedef struct {
	void **data;
	int capacity;
	int len;		// number of elements in this vector.
} Vector;

bool streql(char * s1, char * s2);
bool startswith(char * s1, char * s2);

bool is_alpha(char c);

bool is_alnum(char c);
bool is_whitespace(char c);
void str_tolower(char *s);
bool is_in(char *s, char c);

Vector *new_vec();
void vec_put(Vector *v, void *elem);
void vec_appendv(Vector * v1, Vector * v2);
void *vec_get(Vector *v, int index);

// Map is a struct that maps strings(keys) to values(pointers). 
// A map cannot contain duplicate keys; each key can map to at most one value.
typedef struct {
	Vector *keys;
	Vector *values;
} Map;

void vec_remove(Vector * v, int index);

Map *new_map();
void map_put(Map *map, char *key, void *value);
void *map_get(Map *map, char *key);

// StringBuilder
typedef struct {
	char *data;
	int capacity;
	int len;
} StringBuilder;

StringBuilder *new_sb();
void sb_append(StringBuilder *sb, char *s, int n);
char *sb_get(StringBuilder *sb);
void free_sb(StringBuilder *sb);

/* SrcFile is a struct represents c source file.
 The contents if buffered and terminated by '\0'. 
 A newline is added to the end of the file.
 直接修改src->p时不能超过buf字符串的结束位置。
 */
typedef struct {
	char *path;		// path of the source file
	char *buf;		// file contents terminated by '\0'
	char *p;		// pointer to the current position of file contents(index of buf)
} SrcFile;

SrcFile *new_srcfile(char *path);
char peekc(SrcFile *src);
char nextc(SrcFile *src);


/* TypeSet is an array of integers. Duplicate element is allowed.*/
typedef struct {
	int *types;
	int len;
} TypeSet;

TypeSet * new_typeset(int num_types, ...);
bool typeset_isin(TypeSet *t, int type);
TypeSet *typeset_union(TypeSet *t1, TypeSet *t2);

bool eq_oneof(int num_vargs, int a, ...);

char * stringf(char * fmt, ...);


