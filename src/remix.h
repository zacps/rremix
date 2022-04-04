// Builtin Types
// =========================================

struct ReObject;

typedef struct ReString
{
    char *data;
    unsigned int size;
} ReString;

typedef struct ReInt
{
    int data;
} ReInt;

typedef struct ReFloat
{
    double data;
} ReFloat;

typedef unsigned short ReBool;

typedef struct ReRange
{
    int start;
    int finish;
} ReRange;

typedef struct ReIterator
{
    ReObject *list;
    unsigned int current;
} ReIterator;

typedef enum ReType
{
    reint = 0,
    refloat = 1,
    restring = 2,
    relist = 3,
    rebool = 4,
    range = 5,
    iterator = 6
} ReType;

typedef struct ReList
{
    ReObject *data;
    unsigned int size;
} ReList;

typedef union ReObjects
{
    ReList list;
    ReString string;
    ReInt _int;
    ReFloat _float;
    ReBool _bool;
    ReRange range;
    ReIterator iterator;
} ReObjects;

/// Represents a remix object such as a string, integer, list, range, or boolean
typedef struct ReObject
{
    ReType determinant;
    unsigned int size;
    unsigned int refcount;
    ReObjects object;
} ReObject;

// Internal functions
// =========================================

// Reobjects
ReObject new_object(ReType type);
void free_object(ReObject *object);
void create_reference(ReObject *object);
void destroy_reference(ReObject *object);

// Lists
void extend(ReObject *list, ReObject *object);
ReObject pop(ReObject *list);

ReObject new_string(char *data, unsigned int size);

// Operators
// =========================================
ReObject add(ReObject a, ReObject b);
ReObject sub(ReObject a, ReObject b);
ReObject mult(ReObject a, ReObject b);
ReObject div(ReObject a, ReObject b);
ReObject pow(ReObject a, ReObject b);

// Builtin Functions
// =========================================

void show(ReObject string);
ReObject ask(ReObject string);
ReType type_of(ReObject object);
ReObject convert_to_int(ReObject string);
ReObject convert_to_string(ReObject _int);
void wait(ReObject seconds);
ReObject length_of(ReObject list);
ReObject range(ReObject start, ReObject finish);
ReObject list_get(ReObject list, ReObject index);
ReObject list_set(ReObject list, ReObject index, ReObject value);
ReObject randomize();
ReObject randomize(ReObject seed);
ReObject random(ReObject max);
ReObject sin(ReObject degrees);
ReObject cos(ReObject degrees);
ReObject arctangent(ReObject change_y, ReObject change_x);
ReObject sqrt(ReObject value);
