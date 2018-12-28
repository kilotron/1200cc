#include "util.h"

bool streql(const char * s1, const char * s2)
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
	v->data = (void *)malloc(sizeof(void *) * v->capacity);
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

/* Pre-conditions: v1 != NULL, v2 != NULL
Post-conditions: the elements of v2 is appended to the end of v1 and 
v2 is left unchanged. */
void vec_appendv(Vector *v1, Vector *v2)
{
	for (int i = 0; i < v2->len; i++) {
		vec_put(v1, v2->data[i]);
	}
}

Vector *vec_clone(Vector *v)
{
	Vector *v0 = new_vec();
	for (int i = 0; i < v->len; i++) {
		vec_put(v0, v->data[i]);
	}
	return v0;
}

/* Post-conditions: If v is NULL or index is out of bounds returns NULL
else returns v[index].*/
void * vec_get(Vector * v, int index)
{	
	if (v == NULL || index < 0 || index >= v->len)
		return NULL;
	return v->data[index];
}

/* 0 <= index < v->len */
void vec_remove(Vector *v, int index)
{
	for (int i = index; i < v->len - 1; i++)
		v->data[i] = v->data[i + 1];
	v->len--;
}

void vec_remove_elem(Vector *v, void *elem)
{
	for (int i = 0; i < v->len; i++) {
		if (v->data[i] == elem) {
			vec_remove(v, i);
			return;
		}
	}
}

void vec_remove_duplicates(Vector *v)
{
	void *e1, *e2;
	for (int i = 0; i < v->len; i++) {
		e1 = vec_get(v, i);
		for (int j = i + 1; j < v->len; j++) {
			e2 = vec_get(v, j);
			if (e1 == e2)
				vec_remove(v, j--);
		}
	}
}

bool vec_is_in(Vector *v, void *elem)
{
	for (int i = 0; i < v->len; i++) {
		if (v->data[i] == elem)
			return true;
	}
	return false;
}

void vec_put_if_not_in(Vector *v, void *elem)
{
	if (!vec_is_in(v, elem))
		vec_put(v, elem);
}

/* Behaves as if it's a set.*/
Vector *vec_union(Vector *v1, Vector *v2)
{
	Vector *v = new_vec();
	void *elem;
	for (int i = 0; i < v1->len; i++) {
		vec_put(v, v1->data[i]);
	}
	for (int i = 0; i < v2->len; i++) {
		elem = v2->data[i];
		if (!vec_is_in(v, elem))
			vec_put(v, elem);
	}
	return v;
}

/* v1 - v2 */
Vector *vec_except(Vector *v1, Vector *v2)
{
	Vector *v = new_vec();
	for (int i = 0; i < v1->len; i++) {
		if (!vec_is_in(v2, v1->data[i]))
			vec_put(v, v1->data[i]);
	}
	return v;
}

/* Pre-conditions: No duplicates in v1 or v2.
Examine every element of v1 and v2 and see if the two are exactly the same. */
bool vec_is_different(Vector *v1, Vector *v2)
{
	if (v1->len != v2->len)
		return true;
	for (int i = 0; i < v1->len; i++) {
		if (!vec_is_in(v2, v1->data[i]))
			return true;
	}
	return false;
}

Vector_Iterator * vec_itr(Vector * v)
{
	Vector_Iterator * itr = calloc(1, sizeof(Vector_Iterator));
	itr->v = v;
	itr->p = 0;
	return itr;
}

bool vec_has_next(Vector_Iterator *itr)
{
	return (itr->p < itr->v->len);
}

void *vec_next(Vector_Iterator *itr)
{
	return vec_get(itr->v, itr->p++);
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
	FILE *fp = fopen(path, "r");
	if (fp == NULL) {
		fprintf(stderr, "Cannot open file: %s.", path);
		exit(-1);
	}
	do {
		n = fread(buf, sizeof(char), sizeof(buf), fp);
		sb_append(sb, buf, n);
	} while (n != 0);
	sb_append(sb, "\n", 1);	// make sure a source file ends with a newline.
	src->buf = sb_get(sb);
	src->path = path;
	src->p = src->buf;
	free_sb(sb);
	return src;
}

/* Pre-conditions: src is not NULL.
Post-conditions: Return the next character with internal state of src unchanged.
If pointer p reaches end of the file returns '\0'. */
char peekc(SrcFile * src)
{
	return *(src->p);
}

/* Pre-conditions: src is not NULL.
Post-conditions: If pointer p reaches end of the file returns '\0' else
returns the next character, increments the internal pointer by 1*/
char nextc(SrcFile * src)
{
	if (*src->p == '\0')
		return '\0';
	return *src->p++;
}

/*Pre-conditions: num_types is the number of variable arguments.*/
TypeSet *new_typeset(int num_types, ...)
{
	TypeSet * t = calloc(1, sizeof(TypeSet));
	t->types = malloc(sizeof(int) * num_types);
	t->len = num_types;
	va_list va;
	va_start(va, num_types);
	for (int i = 0; i < num_types; i++) {
		t->types[i] = va_arg(va, int);
	}
	va_end(va);
	return t;
}

bool typeset_isin(TypeSet * t, int type)
{
	for (int i = 0; i < t->len; i++) {
		if (type == t->types[i])
			return true;
	}
	return false;
}

/* Pre-conditions: t1 != NULL, t2 != NULL. 
Post-condtions: Returns a TypeSet t containing elements in both t1 and t2.
There might be duplicates in t.*/
TypeSet * typeset_union(TypeSet * t1, TypeSet * t2)
{
	TypeSet *t = calloc(1, sizeof(TypeSet));
	t->types = malloc(t1->len + t2->len);
	t->len = t1->len + t2->len;
	memcpy(t->types, t1->types, sizeof(int) * t1->len);
	memcpy(t->types + sizeof(int) * t1->len, t2->types, sizeof(int) * t2->len);
	return t;
}

/*Pre-conditions: param cnt is number of variable argument.
a is the number to be compared.
Post-conditions: Returns true if a equals one of the variable argument
and false otherwise. */
bool eq_oneof(int num_vargs, int a, ...)
{
	va_list va;
	va_start(va, a);
	for (int i = 0; i < num_vargs; i++) {
		if (a == va_arg(va, int))
			return true;
	}
	va_end(va);
	return false;
}

/* The length of string is less than 2048.*/
char *stringf(const char *fmt, ...)
{
	va_list va;
	va_start(va, fmt);
	char buf[2048];
	vsnprintf(buf, 2048, fmt, va);
	va_end(va);
	char *s = malloc(strlen(buf) + 1);
	strcpy(s, buf);
	return s;
}

char *get_dir(const char *path)
{
	char *p = stringf("%s", path);
	char cwd[PATH_LEN];
	int i;
	for (i = strlen(p) - 1; i >= 0; i--)
		if (p[i] == '\\') {
			p[i + 1] = '\0';
			return p;
		}
	// if path is a relative path
	_getcwd(cwd, PATH_LEN);
	return stringf("%s\\", cwd);
}

/* Without extension.*/
char *get_filename(const char *path)
{
	char *p1 = stringf(path), *p2;
	bool met_dot = false;
	for (p2 = p1 + strlen(p1) - 1; p2 >= p1 && *p2 != '\\'; p2--) {
		if (!met_dot && *p2 == '.') {
			met_dot = true;
			*p2 = '\0';
		}
	}
	return p2 + 1;
}
