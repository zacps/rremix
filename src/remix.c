#include <stdlib.h>
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
    case 1:
        size = sizeof(ReFloat);
    case 2:
        size = sizeof(ReString);
    case 3:
        size = sizeof(ReList);
    case 4:
        size = sizeof(ReBool);
    case 5:
        size = sizeof(ReRange);
    case 6:
        size = sizeof(ReIterator);
    default:
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
}

// Operators
// =========================================
// ReObject add(ReObject a, ReObject b)
// {
//     switch (a.determinant)
//     {
//     case reint:
//         if (b.determinant == reint)
//         {
//             ReObject obj = new_object(reint);
//             obj.object = (ReObjects){{a.object._int.data + b.object._int.data}};
//             return obj;
//         }
//         exit(-1);
//     case refloat:
//         if (b.determinant == refloat)
//         {
//             ReObject obj = new_object(refloat);
//             obj.object = (ReObjects){{a.object._float.data + b.object._float.data}};
//             return obj;
//         }
//         exit(-1);
//     case restring:
//         if (b.determinant == restring)
//         {
//             return new_string(
//                 a.object.string.data + b.object.string.data,
//                 a.object.string.size + b.object.string.size);
//         }
//         break;
//     case relist:
//         /* code */
//         break;
//     default:
//         exit(-1);
//     }
// }
ReObject sub(ReObject a, ReObject b) {}
ReObject mult(ReObject a, ReObject b) {}
ReObject div(ReObject a, ReObject b) {}
ReObject pow(ReObject a, ReObject b) {}

// Builtins
// =========================================
void show(ReObject string)
{
    if (string.determinant != restring)
    {
        exit(-1);
    }
    printf("%.*s", string.object.string.size, string.object.string.data);
}