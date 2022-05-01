#include <stdlib.h>
#include <stdio.h>
#include "remix.h"

// Reobjects
// =========================================
ReObject new_object(ReType type)
{
    unsigned long long size;

    switch (type)
    {
    case 0:
        size = sizeof(ReInt);
        break;
    case 1:
        size = sizeof(ReFloat);
        break;
    case 2:
        size = sizeof(ReString);
        break;
    case 3:
        size = sizeof(ReList);
        break;
    case 4:
        size = sizeof(ReBool);
        break;
    case 5:
        size = sizeof(ReRange);
        break;
    case 6:
        size = sizeof(ReIterator);
        break;
    case 7:
        size = sizeof(ReNone);
        break;
    default:
        printf("unknown type in new_object");
        exit(-1);
    }

    ReObjects data;

    ReObject object;
    object.object = data;
    object.size = size;
    object.refcount = size;
    object.determinant = type;

    return object;
}

void create_reference(ReObject *object)
{
    object->refcount += 1;
}

// Lists
// =========================================
void extend(ReObject *list, ReObject *object)
{
}

ReObject pop(ReObject *list)
{
    return new_object(renone);
}

// Operators
// =========================================
ReObject o_sub(ReObject a, ReObject b)
{
    return new_object(renone);
}
ReObject o_mult(ReObject a, ReObject b)
{
    return new_object(renone);
}
ReObject o_div(ReObject a, ReObject b)
{
    return new_object(renone);
}
ReObject o_rpow(ReObject a, ReObject b)
{
    return new_object(renone);
}

// Builtins
// =========================================
ReObject b_show(ReObject string)
{
    if (string.determinant != restring)
    {
        printf("tried to show a variable that wasn't a string");
        exit(-1);
    }
    printf("%.*s", string.object.string.size, string.object.string.data);
    return new_object(renone);
}

// Constructors
// =========================================

ReObject new_string(char *data, unsigned int size)
{
    ReObject str = new_object(restring);
    str.object.string.data = data;
    str.object.string.size = size;
    return str;
}

ReObject new_list()
{
    return new_list_cap(LIST_INIT_CAP);
}

ReObject new_list_cap(unsigned int cap)
{
    ReObject list = new_object(relist);
    list.object.list.data = malloc(sizeof(ReObject) * cap);
    if (list.object.list.data == NULL)
    {
        exit(-1);
    }
    list.object.list.len = 0;
    list.object.list.cap = cap;
    return list;
}