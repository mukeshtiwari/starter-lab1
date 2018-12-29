

(* assume the int is height of avl tree *)
type ('k, 'v) avlnode =
  | Leaf
  | Node of int * 'k * 'v * ('k, 'v) avlnode * ('k, 'v) avlnode

let rec get (n : ('k, 'v) avlnode) (key : 'k) : 'v option =
  match n with
    | Leaf -> None
    | Node(_, k, v, left, right) ->
      if k = key then Some v
      else if key < k then (get left key)
      else (get right key)


(* height is stored at each node. *)
let height (n : ('k, 'v) avlnode) : int =
  match n with
  | Leaf -> 0
  | Node (n, _, _, _, _) -> n

(* https://upload.wikimedia.org/wikipedia/commons/c/c4/Tree_Rebalancing.gif *)
let left_left_case (n : ('k, 'v) avlnode) : ('k, 'v) avlnode =
  match n with
  | Node (nroot, kroot, vroot,
          (Node (nleft, kleft, vleft, left_left_tree, left_right_tree)),
          right_tree) ->
     let nright_new = 1 + max (height left_right_tree) (height right_tree) in
     let nroot_new = 1 + max nright_new (height left_left_tree) in 
     Node (nroot_new, kleft, vleft, left_left_tree,
           Node (nright_new, kroot, vroot, left_right_tree, right_tree))
 
  | _ -> assert false (* This should not happen. Use Coq *)


let right_right_case (n : ('k, 'v) avlnode) : ('k, 'v) avlnode =
  match n with
  | Node (nroot, kroot, vroot, left_tree,
          Node (nright, kright, vright, right_left_tree, right_right_tree)) ->
     let nleft_new = 1 + max (height left_tree) (height right_left_tree) in
     let nroot_new = 1 + max nleft_new (height right_right_tree) in
     Node (nroot_new, kright, vright,
           Node (nleft_new, kroot, vroot, left_tree, right_left_tree),
           right_right_tree)
  | _ -> assert false


let left_right_case (n : ('k, 'v) avlnode) : ('k, 'v) avlnode =
  match n with
  | Node (nroot, kroot, vroot, left_tree, right_tree) ->
     let left_tree' = right_right_case left_tree in
     left_left_case (Node (nroot, kroot, vroot, left_tree', right_tree))
  | _ -> assert false


let right_left_case (n : ('k, 'v) avlnode) : ('k, 'v) avlnode =
  match n with
  | Node (nroot, kroot, vroot, left_tree, right_tree) ->
     let right_tree' = left_left_case right_tree in
     right_right_case (Node (nroot, kroot, vroot, left_tree, right_tree'))
  | _ -> assert false 

let balance_factor (n : ('k, 'v) avlnode) : int =
  match n with
  | Leaf -> 0
  | Node (_, _, _, left_tree, right_tree) ->
     height right_tree - height left_tree 
     
(* A useful helper function (you may need to write more) *)
(* rotate the tree if balance factor is not +1, 0, -1
   -2 => left heavy  => Two case +1 or -1.
    2 => Right heavy => Two case +1 or -1 *)
let balance (n : ('k, 'v) avlnode) : ('k, 'v) avlnode =
  match n with
  | Leaf -> Leaf
  | Node (nroot, kroot, vroot, left_tree, right_tree) ->
     let balance_of_root = balance_factor n in
     match balance_of_root with
     | 2 (* right heavy *) ->
        if balance_factor right_tree = 1 then right_right_case n
        else right_left_case n
     | -2 (* left heavy *) ->
        if balance_factor left_tree = 1 then left_right_case n
        else left_left_case n
     | _ (* balanced *) -> n
     
  

(* Produce a new avltree that contains the given key.  If the key already
   exists, update the value to the new value *)
let rec set (n : ('k, 'v) avlnode) (key : 'k) (value : 'v) : ('k, 'v) avlnode =
  match n with
  | Leaf -> Node (1, key, value, Leaf, Leaf)
  | Node (nroot, kroot, vroot, left_tree, right_tree) ->
     balance (
         if vroot = value then Node (nroot, kroot, value,
                                     left_tree, right_tree)
         else if value < vroot then Node (nroot, kroot, vroot,
                                          (set left_tree key value),
                                          right_tree)
         else Node (nroot, kroot, vroot, left_tree,
                    (set right_tree key value)))
                                      

                       

(* Return a list of tuples containing the elements of the tree *)
let rec inorder (n : ('k, 'v) avlnode) : ('k * 'v) list =
  match n with
  | Leaf -> []
  | Node (nroot, kroot, vroot, left_tree, right_tree) ->
     inorder left_tree @ [(kroot, vroot)] @ inorder right_tree



(* Write the functions below (contains, height, add_all, sum) without using
any recursion.  Use the functions above, and map/filter/fold if necessary, to
build up the behavior from existing functions. *)

let contains (n : ('k, 'v) avlnode) (key : 'k) : bool = 
  List.exists (fun (key', _) -> key' = key) (inorder n)

(* given an AVL tree and a list of key/value tuples, set all the given keys
(first pair components) to the corresponding value (second pair components) *)
let add_all (n : ('k, 'v) avlnode) (keys : ('k * 'v) list) : ('k, 'v) avlnode =
  List.fold_left (fun tree (key, value) -> set tree key value) n keys 

(* Return the total value of all the integers in a tree that has int values *)
let sum (n : ('k, int) avlnode) : int =
  List.fold_left (fun acc (key, value) -> acc + value) 0 (inorder n)


