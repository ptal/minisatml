#ifndef Vec_h
#define Vec_h

#include <cstdlib>
#include <cassert>
#include <new>

//=================================================================================================
// Automatically resizable arrays
//
// NOTE! Don't use this vector on datatypes that cannot be re-located in memory (with realloc)

template<class T>
class vec {
    T*  data;
    int sz;
    int cap;

    void     init(int size, const T& pad);
    void     grow(int min_cap);

    // Don't allow copying (error prone):
    vec<T>&  operator = (vec<T>& other) { assert(0); return *this; }
             vec        (vec<T>& other) { assert(0); }

    static inline int imin(int x, int y) {
        int mask = (x-y) >> (sizeof(int)*8-1);
        return (x&mask) + (y&(~mask)); }

    static inline int imax(int x, int y) {
        int mask = (y-x) >> (sizeof(int)*8-1);
        return (x&mask) + (y&(~mask)); }

public:
    // Types:
    typedef int Key;
    typedef T   Datum;

    // Constructors:
    vec(void)                   : data(NULL) , sz(0)   , cap(0)    { }
    vec(int size)               : data(NULL) , sz(0)   , cap(0)    { growTo(size); }
    vec(int size, const T& pad) : data(NULL) , sz(0)   , cap(0)    { growTo(size, pad); }
    vec(T* array, int size)     : data(array), sz(size), cap(size) { }      // (takes ownership of array -- will be deallocated with 'free()')
   ~vec(void)                                                      { clear(true); }

    // Ownership of underlying array:
    T*       release  (void)           { T* ret = data; data = NULL; sz = 0; cap = 0; return ret; }
    operator T*       (void)           { return data; }     // (unsafe but convenient)
    operator const T* (void) const     { return data; }

    // Size operations:
    int      size   (void) const       { return sz; }
    void     shrink (int nelems)       { assert(nelems <= sz); for (int i = 0; i < nelems; i++) sz--, data[sz].~T(); }
    void     shrink_(int nelems)       { assert(nelems <= sz); sz -= nelems; }
    void     pop    (void)             { sz--, data[sz].~T(); }
    void     growTo (int size);
    void     growTo (int size, const T& pad);
    void     clear  (bool dealloc = false);
    void     capacity (int size) { grow(size); }

    // Stack interface:
#if 1
    void     push  (void)              { if (sz == cap) { cap = imax(2, (cap*3+1)>>1); data = (T*)realloc(data, cap * sizeof(T)); } new (&data[sz]) T(); sz++; }
    //void     push  (const T& elem)     { if (sz == cap) { cap = imax(2, (cap*3+1)>>1); data = (T*)realloc(data, cap * sizeof(T)); } new (&data[sz]) T(elem); sz++; }
    void     push  (const T& elem)     { if (sz == cap) { cap = imax(2, (cap*3+1)>>1); data = (T*)realloc(data, cap * sizeof(T)); } data[sz++] = elem; }
    void     push_ (const T& elem)     { assert(sz < cap); data[sz++] = elem; }
#else
    void     push  (void)              { if (sz == cap) grow(sz+1); new (&data[sz]) T()    ; sz++; }
    void     push  (const T& elem)     { if (sz == cap) grow(sz+1); new (&data[sz]) T(elem); sz++; }
#endif

    const T& last  (void) const        { return data[sz-1]; }
    T&       last  (void)              { return data[sz-1]; }

    // Vector interface:
    const T& operator [] (int index) const  { return data[index]; }
    T&       operator [] (int index)        { return data[index]; }


    // Duplicatation (preferred instead):
    void copyTo(vec<T>& copy) const { copy.clear(); copy.growTo(sz); for (int i = 0; i < sz; i++) new (&copy[i]) T(data[i]); }
    void moveTo(vec<T>& dest) { dest.clear(true); dest.data = data; dest.sz = sz; dest.cap = cap; data = NULL; sz = 0; cap = 0; }
};

template<class T>
void vec<T>::grow(int min_cap) {
    if (min_cap <= cap) return;
    if (cap == 0) cap = (min_cap >= 2) ? min_cap : 2;
    else          do cap = (cap*3+1) >> 1; while (cap < min_cap);
    data = (T*)realloc(data, cap * sizeof(T)); }

template<class T>
void vec<T>::growTo(int size, const T& pad) {
    if (sz >= size) return;
    grow(size);
    for (int i = sz; i < size; i++) new (&data[i]) T(pad);
    sz = size; }

template<class T>
void vec<T>::growTo(int size) {
    if (sz >= size) return;
    grow(size);
    for (int i = sz; i < size; i++) new (&data[i]) T();
    sz = size; }

template<class T>
void vec<T>::clear(bool dealloc) {
    if (data != NULL){
        for (int i = 0; i < sz; i++) data[i].~T();
        sz = 0;
        if (dealloc) free(data), data = NULL, cap = 0; } }


#endif
