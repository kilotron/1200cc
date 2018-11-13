#include "util.h"

bool streql(char *s1, char *s2)
{
	while (*s1 != '\0' && *s2 != '\0') {
		if (*s1++ != *s2++)
			return false;
	}
	return *s1 == *s2;
}

// Returns true if s2 is a prefix of s1.
bool startswith(char *s1, char *s2)
{
	while (*s1 != '\0' && *s2 != '\0') {
		if (*s1++ != *s2++)
			return false;
	}
	return *s2 == '\0';
}

bool is_alpha(char c)
{
	return isalpha(c) || c == '_';
}

bool is_alnum(char c)
{
	return isalnum(c) || c == '_';
}


/* Returns true if c is space, '\n' or '\t' */
bool is_whitespace(char c)
{
	return c == ' ' || c == '\n' || c == '\t';
}

void str_tolower(char * s)
{
	while (*s != '\0') {
		*s = tolower(*s);
		s++;
	}
}

bool is_in(char *s, char c)
{
	return strchr(s, c) != NULL;
}

Vector * new_vec()
{
	Vector *v = malloc(sizeof(Vector));
	v->capacity = 16;
	v->data = malloc(sizeof(void *) * v->capacity);
	v->len = 0;	// number of elements in the vector.
	return v;
}

/* Pre-conditions: v != NULL, elem != NULL.
 * Post-conditions: elem is placed at the end of vector v.*/
void vec_put(Vector * v, void * elem)
{
	if (v->capacity == v->len) {
		v->capacity *= 2;
		v->data = realloc(v->data, sizeof(void *) * v->capacity);
	}
	v->data[v->len++] = elem;
}

/* DEPRECATED! 
 * Pre-conditions: v != NULL
 * Post-conditions: If elem is in v returns the index of the first occurence of elem
 * else returns -1.
 */
int vec_indexof(Vector * v, void * elem)
{
	for (int i = 0; i < v->len; i++)
		if (v->data[i] == elem)
			return i;
	return -1;
}

Map * new_map()
{
	Map *map = malloc(sizeof(Map));
	map->keys = new_vec();
	map->values = new_vec();
	return map;
}

/* Associates the specified value with the specified key in this map.
 * Pre-conditions: map != NULL, key != NULL, value != NULL.
 * Post-conditions: If the map previously contained a mapping for the key,
 * the old value is replaced. */
void map_put(Map * map, char * key, void * value)
{
	int i;
	for (i = 0; i < map->keys->len; i++) {
		if (streql(key, map->keys->data[i])) {
			map->values->data[i] = value;
			return;
		}
	}
	vec_put(map->keys, key);
	vec_put(map->values, value);
}

void * map_get(Map * map, char * key)
{
	int i;
	for (i = 0; i < map->keys->len; i++) {
		if (streql(key, map->keys->data[i]))
			return map->values->data[i];
	}
	return NULL;
}

StringBuilder * new_sb()
{
	StringBuilder *sb = malloc(sizeof(StringBuilder));
	sb->capacity = 16;
	sb->data = malloc(sizeof (char) * sb->capacity);
	sb->len = 0;
	return sb;
}

/* Pre-conditions: sb != NULL, s != NULL, 0<= n <= strlen(s).
 * Post-conditions: The string start from s 
 * with length n is append to sb.*/
void sb_append(StringBuilder * sb, char * s, int n)
{
	while (sb->len + n > sb->capacity) {
		sb->capacity *= 2;
	}
	sb->data = realloc(sb->data, sb->capacity);
	memcpy(sb->data + sb->len, s, n);
	sb->len += n;
}

// Returns a copy of the string.
char * sb_get(StringBuilder * sb)
{
	char *s = (char *)malloc(sb->len + 1);
	memcpy(s, sb->data, sb->len);
	s[sb->len] = '\0';
	return s;
}

void free_sb(StringBuilder * sb)
{
	free(sb->data);
	free(sb);
}

SrcFile * new_srcfile(char * path)
{
	SrcFile *src = malloc(sizeof(SrcFile));
	char buf[4096];
	StringBuilder *sb = new_sb();
	int n;
	src->path = path;
	src->fp = fopen(path, "r");
	if (src->fp == NULL) {
		fprintf(stderr, "Cannot open file: %s.", path);
		exit(-1);
	}
	do {
		n = fread(buf, sizeof(char), sizeof(buf), src->fp);
		sb_append(sb, buf, n);
	} while (n != 0);
	sb_append(sb, "\n", 1);	// make sure a source file ends with a newline.
	src->buf = sb_get(sb);
	free_sb(sb);
	src->p = src->buf;
	src->line = 1;
	src->col = 1;
	return src;
}

/* Pre-conditions: src is not NULL.
Post-conditions: Return the next character with internal state of src unchanged.
If pointer p reaches end of the file returns '\0'. */
char peekc(SrcFile * src)
{
	if (*src->p == '\0')
		return '\0';
	return *(src->p);
}

/* Pre-conditions: src is not NULL.
Post-conditions: If pointer p reaches end of the file returns '\0' else
returns the next character, increments the internal pointer by 1,
and modifies src->line and src->col to keep consistency with src->p.*/
char nextc(SrcFile * src)
{
	if (*src->p == '\0')
		return '\0';
	char c = *src->p++;
	src->col++;
	if (c == '\n') {
		src->line++;
		src->col = 1;
	}
	return c;
}


