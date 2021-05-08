signature DOUBLY_LINKED_LIST =
sig
    exception E
    type ’a reptype (* <-- INTERNAL REPRESENTATION *)
    val new: ’a reptype
    val push: ’a -> ’a reptype -> ’a reptype
    val pop: ’a reptype -> ’a reptype
    val top: ’a reptype -> ’a
end

structure DoublyLinkedList : STACK =
struct
    exception E
    datatype ’a reptype = ’a list 
    val new = [] 
    fun push x s = x::s 
    fun split( h::t ) = ( h , t )
      | split _ = raise E 
    fun pop s = #2( split s ) 
    fun top s = #1( split s ) 
end