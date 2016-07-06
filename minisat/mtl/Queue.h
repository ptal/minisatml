#ifndef Queue_h
#define Queue_h
#include "Vec.h"

//=================================================================================================

template <class T>
class Queue {
    vec<T>  elems;
    int     first;

public:
    Queue(void) : first(0) { }

    void insert(T x)   { elems.push(x); }
    T    peek  () const { return elems[first]; }
    void pop   () { first++; }

    void clear(bool dealloc = false)   { elems.clear(dealloc); first = 0; }
    int  size(void)    { return elems.size() - first; }

    //bool has(T x) { for (int i = first; i < elems.size(); i++) if (elems[i] == x) return true; return false; }

    const T& operator [] (int index) const  { return elems[first + index]; }

};

//template<class T>
//class Queue {
//    vec<T>  buf;
//    int     first;
//    int     end;
//
//public:
//    typedef T Key;
//
//    Queue() : buf(1), first(0), end(0) {}
//
//    void clear () { buf.shrinkTo(1); first = end = 0; }
//    int  size  () { return (end >= first) ? end - first : end - first + buf.size(); }
//
//    T    peek  () { assert(first != end); return buf[first]; }
//    void pop   () { assert(first != end); first++; if (first == buf.size()) first = 0; }
//    void insert(T elem) {   // INVARIANT: buf[end] is always unused
//        buf[end++] = elem;
//        if (end == buf.size()) end = 0;
//        if (first == end){  // Resize:
//            vec<T>  tmp((buf.size()*3 + 1) >> 1);
//            //**/printf("queue alloc: %d elems (%.1f MB)\n", tmp.size(), tmp.size() * sizeof(T) / 1000000.0);
//            int     i = 0;
//            for (int j = first; j < buf.size(); j++) tmp[i++] = buf[j];
//            for (int j = 0    ; j < end       ; j++) tmp[i++] = buf[j];
//            first = 0;
//            end   = buf.size();
//            tmp.moveTo(buf);
//        }
//    }
//};

//=================================================================================================
#endif
