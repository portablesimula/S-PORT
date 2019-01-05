/******************************************************************************/
/**                                                                          **/
/**  COPYRIGHT 1991 by                                                       **/
/**  Simula a.s.                                                             **/
/**  Oslo , Norway                                                           **/
/**                                                                          **/
/**                                                                          **/
/**                     P O R T A B L E     S I M U L A                      **/
/**                                                                          **/
/**                          C - I N T E R F A C E                           **/
/**                                                                          **/
/******************************************************************************/

/******************************************************************************/
/**  ACCESS TO PARAMETERS WHEN ROUTINE HAS BEEN DECLARED "EXTERNAL LIBRARY"  **/
/**  ======================================================================  **/
/**                                                                          **/
/**  Simula Parameter     C-Declaration      Access in C                     **/
/**  (Value/Default)                                                         **/
/**                                                                          **/
/**  boolean b;          boolean b;          b                               **/
/**  character c;        character c;        c                               **/
/**  short integer s;    short_integer s;    s                               **/
/**  integer i;          integer i;          i                               **/
/**  real r;             real r;             r                               **/
/**  long real lr;       long_real lr;       lr                              **/
/**  text t;             text t;             (*t.ent).cha[t.sp+j]            **/
/**  label L;            labqnt L;           L.pad                           **/
/**                                                                          **/
/**                                                                          **/
/**  Simula Parameter     C-Declaration     Example Access in C              **/
/**  (by Name)                                                               **/
/**                                                                          **/
/**  boolean b;          name nb;           VAR(boolean,nb)                  **/
/**  character c;        name nc;           VAR(character,nc)                **/
/**  short integer s;    name ns;           VAR(short_integer,ns)            **/
/**  integer i;          name ni;           VAR(integer,ni)                  **/
/**  real r;             name nr;           VAR(real,nr)                     **/
/**  long real lr;       name nlr;          VAR(long_real,nlr)               **/
/**  text t;             name nt;           TXT(nt).cha[i]                   **/
/**                                         VAR(text,nt).sp  .. etc.         **/
/**                                                                          **/
/**                                                                          **/
/**  Array Parameters (one dimension):                                       **/
/**                                                                          **/
/**  Simula (Value/Default)   C-Declaration    Element Access in C           **/
/**                                                                          **/
/**  boolean array ba;         array1 ba       (*ba).body.BoolElt[j]         **/
/**  character array ca;       array1 ca       (*ca).body.CharElt[j]         **/
/**  short integer array sa;   array1 sa       (*sa).body.SintElt[j]         **/
/**  integer array ia;         array1 ia       (*ia).body.IntElt[j]          **/
/**  real array ra;            array1 ra       (*ra).body.RealElt[j]         **/
/**  long real array lra;      array1 lra      (*lra).body.LrealElt[j]       **/
/**  text array ta;            array1 ta       (*ta).body.TextElt[j]         **/
/**  ref(C) array rfa;         array1 rfa      (*rfa).body.RefElt[j]         **/
/**                                                                          **/
/**                                                                          **/
/**  Simula (by Name)         C-Declaration    Element Access in C           **/
/**                                                                          **/
/**  boolean array nba;        name nba;       ARR1(nba).body.BoolElt[j]     **/
/**  character array nca;      name nca;       ARR1(nca).body.CharElt[j]     **/
/**  short integer array nsa;  name nsa;       ARR1(nsa).body.SintElt[j]     **/
/**  integer array nia;        name nia;       ARR1(nia).body.IntElt[j]      **/
/**  real array nra;           name nra;       ARR1(nra).body.RealElt[j]     **/
/**  long real array nlra;     name nlra;      ARR1(nlra).body.LrealElt[j]   **/
/**  text array nta;           name nta;       ARR1(nta).body.TextElt[j]     **/
/**  ref(C) array nrfa;        name nrfa;      ARR1(nrfa).body.RefElt[j]     **/
/**                                                                          **/
/**  ======================================================================  **/
/******************************************************************************/


#define MAX_KIND   7   /** PARAMETER KIND CODES                             **/
#define K_SMP      0   /** simple kind                                      **/
#define K_PRO      1   /** procedure kind                                   **/
#define K_ARR      2   /** array kind                                       **/
#define K_LAB      3   /** label kind                                       **/
#define K_SWT      4   /** switch kind                                      **/
#define K_CLA      5   /** class kind (SIMOB only)                          **/
/**     K_REP      6       repetition - not fully implemented               **/
/**     K_REC      7       record - not fully implemented                   **/

#define MAX_MODE   5   /** PARAMETER MODE CODES                             **/
#define M_REF      0   /** reference parameter                              **/
#define M_VALUE    1   /** value parameter                                  **/
#define M_NAME     2   /** name parameter                                   **/
#define M_LOCAL    3   /** local quantity                                   **/
#define M_EXTR     4   /** external quantity (SIMOB only)                   **/
/**     M_LIT      5       literal - not fully implemented                  **/

#define MAX_TYPE   9   /** TYPE CODES                                       **/
#define T_NOTYPE   0   /** no type                                          **/
#define T_BOO      1   /** boolean                                          **/
#define T_CHA      2   /** character                                        **/
#define T_SIN      3   /** short_integer                                    **/
#define T_INT      4   /** integer                                          **/
#define T_REA      5   /** real                                             **/
#define T_LRL      6   /** long real                                        **/
#define T_REF      7   /** reference quantity                               **/
#define T_TXT      8   /** text descriptor                                  **/
/**     T_PTR      9       pointer quantity - not fully implemented         **/

#define MAX_SORT  22   /** ENTITY SORT CODES                                **/
#define S_NOSORT   0   /** no sort                                          **/
/**     instance sort codes                                                 **/
#define S_SUB      1   /** subblock                                         **/
#define S_PRO      2   /** procedure                                        **/
#define S_ATT      3   /** attached instance                                **/
#define S_DET      4   /** detached instance                                **/
#define S_RES      5   /** resumed  instance                                **/
#define S_TRM      6   /** terminated instance                              **/
#define S_PRE      7   /** prefixed block                                   **/
#define S_THK      8   /** thunk                                            **/
/**     special  sort codes                                                 **/
#define S_SAV      9   /** save object                                      **/
#define S_ALLOC   10   /** I-O buffer                                       **/
#define S_GAP     11   /** Storage Gap - should never occur                 **/
/**     entity   sort codes                                                 **/
#define S_TXTENT  12   /** Text Entity                                      **/
#define S_ARHEAD  13   /** Array Head Entity                                **/
#define S_ARBODY  14   /** Array Body Entity      (3 or more dimensions)    **/
#define S_ARBREF  15   /** ref-Array Body Entity  (3 or more dimensions)    **/
#define S_ARBTXT  16   /** text-Array Body Entity (3 or more dimensions)    **/
#define S_ARENT2  17   /** Array Body Entity      (2 dimensions)            **/
#define S_ARREF2  18   /** ref-Array Body Entity  (2 dimensions)            **/
#define S_ARTXT2  19   /** text-Array Body Entity (2 dimensions)            **/
#define S_ARENT1  20   /** Array Body Entity      (1 dimension)             **/
#define S_ARREF1  21   /** ref-Array Body Entity  (1 dimension)             **/
#define S_ARTXT1  22   /** text-Array Body Entity (1 dimension)             **/

#define VAR(t,n)  (*((t *)(n.obj+n.ofst)))
#define TXT(n)    (*VAR(text,n).ent)
#define ARR1(n)   (*VAR(array1,n))
#define ARR2(n)   (*VAR(array2,n))
#define ARR(n)    (*VAR(array,n))


typedef unsigned char character;
typedef unsigned char boolean;
typedef short int     short_integer;
typedef long int      integer;
typedef float         real;
typedef long float    long_real;
typedef int          *pointer;     /** Unqualified Reference **/


typedef union
{ pointer prot;         /** Pointer to Prototype (sort code < S_SAV) **/
  unsigned short ncha;  /** Number of chars (sort code S_TXTENT)     **/
  unsigned short lng;   /** Length of entity (sort code > S_TXTENT)  **/
} ppinfo;

typedef struct          /** ENTITY SYSTEM HEAD            **/
{ pointer sl;           /** Static Link                   **/
  unsigned char sort;   /** Entity Sort Code              **/
  unsigned char misc;   /** (type code for array entity)  **/
  ppinfo  pp;           /** Prototype Information         **/
} entity;

typedef struct          /** INSTANCE SYSTEM HEAD                 **/
{ entity enthd;         /** Entity system head                   **/
  pointer gcl;          /** Garbage Collector's Link             **/
  struct entity *dl;    /** Dynamic Link                         **/
  pointer lsc;          /** Reactivation point (program address) **/
} inst;


struct txtent             /** TEXT ENTITY                 **/
{ entity enthd;           /** Entity system head          **/
  unsigned char cha[10];  /** The individuals characters  **/
};

typedef struct        /** TEXT QUANTITY          **/
{ struct txtent *ent; /** Pointer to Text Entity **/
  unsigned short cp;  /** Current Position       **/
  unsigned short sp;  /** Start Position         **/
  unsigned short lp;  /** Last Position + 1      **/
} text;

union ArBody
{ boolean       BoolElt[10];
  character     CharElt[10];
  short_integer SintElt[10];
  integer       IntElt[10];
  real          RealElt[10];
  long_real     LrealElt[10];
  pointer       RefElt[10];
  text          TextElt[10];
};

struct arent1           /** ONE-DIMENSIONAL ARRAY       **/
{ entity enthd;         /** Entity system head          **/
  integer lb;           /** Lower Bound                 **/
  integer ub;           /** Upper Bound                 **/
  union ArBody body;    /** The individuals elements    **/
};


struct arent2           /** TWO-DIMENSIONAL ARRAY       **/
{ entity enthd;         /** Entity system head          **/
  integer lb_1;
  integer ub_1;
  integer dope;
  integer lb_2;
  integer ub_2;
  integer neg_base;
  union ArBody body;    /** The individuals elements    **/
};


/*** MANY-DIMENSIONAL ARRAYS ***/

struct arrbnd
{ integer   lb;
  integer   ub;
  union
  { integer   dope;
    integer   neg_base;
  } bndval;
};

struct arhead
{ entity enthd;
  unsigned char   ndim;
  integer         nelt;
  struct arrbnd   bound[10];
};

struct arent            /** MANY-DIMENSIONAL ARRAY      **/
{ entity enthd;         /** Entity system head          **/
  struct arhead *head;  /** Pointer to Array Head       **/
  union ArBody body;    /** The individuals elements    **/
};



typedef struct arent1 *array1; /** One Dimension         **/
typedef struct arent2 *array2; /** Two Dimensions        **/
typedef struct arent  *array;  /** 3 or more Dimensions  **/

typedef struct        /** LABEL QUANTITY         **/
{ pointer sl;         /** Static Link            **/
  pointer pad;        /** Program Address        **/
  unsigned char clv;  /** Connection Level       **/
} labqnt;

typedef struct         /** NAME PARAMETER        **/
{ unsigned short ofst; /** Attribute Offset      **/
  char *obj;           /** Pointer to Entity     **/
} name;


