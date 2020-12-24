#include "yaml.h"
#include <stdio.h>

yaml_parser_t* hs_init_yaml_parser(){
    yaml_parser_t* p = (yaml_parser_t*)malloc(sizeof(yaml_parser_t));
    if (p != NULL) {
        yaml_parser_initialize(p);
    }
    return p;
}

void hs_free_yaml_parser(yaml_parser_t* p){
    yaml_parser_delete(p);
    free(p);
}

typedef struct hs_yaml_buf_s {
    unsigned char *buf;
    size_t size;
    size_t used;
} hs_yaml_buf_t;

int buffer_append(void *ext, unsigned char *str, size_t size)
{
    hs_yaml_buf_t *b = ext;
    int new_size, new_used;
    unsigned char *tmp;

    new_used = b->used + size;
    for (new_size = b->size ? b->size : 120; new_size < new_used; new_size *= 2);

    if (new_size != b->size) {
        tmp = realloc(b->buf, new_size);
        if (!tmp) return 0;
        b->buf = tmp;
        b->size = new_size;
    }

    memcpy(b->buf + b->used, str, size);
    b->used = new_used;

    return 1;
}

yaml_emitter_t* hs_init_yaml_emitter(int canonical, int indent, int width)
{
    yaml_emitter_t* emitter = (yaml_emitter_t*)malloc(sizeof(yaml_emitter_t));
    if (!yaml_emitter_initialize(emitter)) return NULL;

    hs_yaml_buf_t* buf = (hs_yaml_buf_t*)malloc(sizeof(hs_yaml_buf_t));
    if (!buf) {
        free(emitter);
        return NULL;
    } else {
        buf->buf = 0;
        buf->size = buf->used = 0;
    }

    yaml_emitter_set_output(emitter, buffer_append, buf);
    yaml_emitter_set_encoding(emitter, YAML_UTF8_ENCODING);
    yaml_emitter_set_canonical(emitter, canonical);
    yaml_emitter_set_width(emitter, width);
    yaml_emitter_set_indent(emitter, indent);

    return emitter;
}

void hs_free_yaml_emitter(yaml_emitter_t* emitter){
    hs_yaml_buf_t *b = (hs_yaml_buf_t*)emitter->write_handler_data;
    free(b->buf);
    free(b);
    yaml_emitter_delete(emitter);
    free(emitter);
}

yaml_emitter_t* hs_init_yaml_emitter_file(FILE* f, int canonical, int indent, int width)
{
    yaml_emitter_t* emitter = (yaml_emitter_t*)malloc(sizeof(yaml_emitter_t));
    if (!yaml_emitter_initialize(emitter)) return NULL;

    yaml_emitter_set_output_file(emitter, f);
    yaml_emitter_set_encoding(emitter, YAML_UTF8_ENCODING);
    yaml_emitter_set_canonical(emitter, canonical);
    yaml_emitter_set_width(emitter, width);
    yaml_emitter_set_indent(emitter, indent);

    return emitter;
}

void hs_free_yaml_emitter_file(yaml_emitter_t* emitter){
    yaml_emitter_delete(emitter);
    free(emitter);
}

size_t hs_get_yaml_emitter_length(yaml_emitter_t* emitter){
    hs_yaml_buf_t *b = (hs_yaml_buf_t*)emitter->write_handler_data;
    return b->used;
}

void hs_copy_yaml_emitter_result(yaml_emitter_t* emitter, unsigned char* ptr, size_t len){
    hs_yaml_buf_t *b = (hs_yaml_buf_t*)emitter->write_handler_data;
    memcpy(ptr, b->buf, len);
}


int hs_yaml_sequence_start_event_initialize(yaml_event_t *event,
    const yaml_char_t *anchor, const yaml_char_t *tag, int implicit,
    yaml_sequence_style_t style) {

    if (*anchor == 0) anchor = NULL;
    if (*tag == 0) tag = NULL;
    return yaml_sequence_start_event_initialize(event, anchor, tag, implicit, style);
}


int hs_yaml_scalar_event_initialize(yaml_event_t *event,
    const yaml_char_t *anchor, const yaml_char_t *tag,
    const yaml_char_t *value, int off, int length, 
    int plain_implicit, int quoted_implicit,
    yaml_scalar_style_t style){

    if (*anchor == 0) anchor = NULL;
    if (*tag == 0) tag = NULL;
    return yaml_scalar_event_initialize(event, anchor, tag, value+off, length, plain_implicit, quoted_implicit, style);
}


int hs_yaml_mapping_start_event_initialize(yaml_event_t *event,
    const yaml_char_t *anchor, const yaml_char_t *tag, int implicit,
    yaml_mapping_style_t style) {

    if (*anchor == 0) anchor = NULL;
    if (*tag == 0) tag = NULL;
    return yaml_mapping_start_event_initialize(event, anchor, tag, implicit, style);
}

int hs_yaml_document_start(yaml_event_t *e) {
    return yaml_document_start_event_initialize(e, 0, 0, 0, 1);
}
