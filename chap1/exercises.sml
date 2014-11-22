type key = string

datatype tree = Leaf
              | Tree of tree * key * tree

val empty = Leaf

fun insert key Leaf          = Tree (Leaf, key, Leaf)
  | insert key (Tree(l,k,r)) = if key < k then
                                   Tree(insert key l, k, r)
                               else if key > k then
                                   Tree(l, k, insert key r)
                               else
                                   Tree(l,key,r)

(* Exercise 1.1 a. *)
fun member key Leaf = false
  | member key (Tree(l,k,r)) = if key < k then
                                   member key l
                               else if key > k then
                                   member key r
                               else
                                   key = k

(* Exercise 1.1 b. *)
exception KeyNotFound (* Ugly, ugly hack *)

datatype 'a tree2 = L
                  | T of 'a tree2 * key * 'a * 'a tree2

fun insert' k v L = T(L, k, v, L)
  | insert' k v (T(l,k',v',r)) = if k = k' then
                                        T(l,k',v,r)
                                    else if k < k' then
                                        T(insert' k v l, k', v', r)
                                    else
                                        T(l, k', v', insert' k v r)
fun lookup k L = raise KeyNotFound
  | lookup k (T(l,k',v,r)) = if k < k' then
                                   lookup k l
                               else if k > k' then
                                   lookup k r
                               else
                                  v

(* Exercise 1.1 c. *)
val t = insert "t"
          (insert "s"
             (insert "b"
                (insert "f"
                   (insert "p"
                      (insert "i"
                         (insert "p"
                            (insert "s"
                               (insert "t" Leaf))))))))
val t = insert "i"
          (insert "h"
             (insert "g"
                     (insert "f" Leaf)));

(* Exercise 1.1 d. *)

(* For self-balancing trees that balance on insert, not search, you could use:
   red-black tree, 2-3 search trees, etc *)
