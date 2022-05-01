// Builtin Types
// =========================================

// Forward declaration
typedef struct ReObject ReObject;

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
    iterator = 6,
    renone = 7,
} ReType;

typedef struct ReList
{
    ReObject *data;
    unsigned int len;
    unsigned int cap;
} ReList;

typedef struct ReNone
{
} ReNone;

typedef union ReObjects
{
    ReList list;
    ReString string;
    ReInt _int;
    ReFloat _float;
    ReBool _bool;
    ReRange range;
    ReIterator iterator;
    ReNone none;
} ReObjects;

/// Represents a remix object such as a string, integer, list, range, or boolean
typedef struct ReObject
{
    ReType determinant;
    unsigned int size;
    unsigned int refcount;
    ReObjects object;
} ReObject;

// Constructors
// =========================================

ReObject new_object(ReType type);
ReObject new_string(char *data, unsigned int size);
ReObject new_list();
ReObject new_list_cap(unsigned int cap);

// Internal functions
// =========================================

// Reobjects
void free_object(ReObject *object);
void create_reference(ReObject *object);
void destroy_reference(ReObject *object);

// Lists
void extend(ReObject *list, ReObject *object);
void push(ReObject *list, ReObject *object);
ReObject pop(ReObject *list);

// Operators
// =========================================
ReObject o_add(ReObject a, ReObject b);
ReObject o_sub(ReObject a, ReObject b);
ReObject o_mult(ReObject a, ReObject b);
ReObject o_div(ReObject a, ReObject b);
ReObject o_pow(ReObject a, ReObject b);

// Builtin Functions
// =========================================

ReObject b_show(ReObject string);
ReObject b_ask(ReObject string);
ReType b_type_of(ReObject object);
ReObject b_convert_to_int(ReObject string);
ReObject b_convert_to_string(ReObject _int);
ReObject b_wait(ReObject seconds);
ReObject b_length_of(ReObject list);
ReObject b_range(ReObject start, ReObject finish);
ReObject b_list_get(ReObject list, ReObject index);
ReObject b_list_set(ReObject list, ReObject index, ReObject value);
ReObject b_randomize();
ReObject b_randomize(ReObject seed);
ReObject b_random(ReObject max);
ReObject b_sin(ReObject degrees);
ReObject b_cos(ReObject degrees);
ReObject b_arctangent(ReObject change_y, ReObject change_x);
ReObject b_sqrt(ReObject value);

// Constants
// =========================================
static const unsigned int LIST_INIT_CAP = 8;