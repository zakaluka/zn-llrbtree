namespace ZN.DataStructures

/// Possible colors of nodes, Red and Black.
type Color =
  | R
  | B

/// The Tree that all functions in this module operate on.
type LLRBTree<[<EqualityConditionalOn;ComparisonConditionalOn>] 'a when 'a: comparison and 'a: equality> =
  /// Represents an empty Tree.
  | E
  /// A node in a Tree, with possible children.
  | T of color: Color * left: 'a LLRBTree * 'a * right: 'a LLRBTree

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LLRBTree =
  /// Create an empty tree.
  let public empty = E

  /// Get the color of the node.
  let getColor =
    function
    | E -> B
    | T(c,_,_,_) -> c

  /// Determines if a node represents a leaf (two `E` children)
  let isLeaf =
    function
    | T(_,l,v,r) -> l = E && r = E
    | E -> false

  /// Returns the value of a node, fails for an empty Tree.
  let public getValue =
    function
    | E -> System.Collections.Generic.KeyNotFoundException "Empty tree" |> raise
    | T(_,_,v,_) -> v

  /// Returns the left sub-tree of a node, fails for an empty Tree.
  let public getLeftTree =
    function
    | E -> System.Collections.Generic.KeyNotFoundException "Empty tree" |> raise
    | T(_,l,_,_) -> l

  /// Returns the right sub-tree of a node, fails for an empty Tree.
  let public getRightTree =
    function
    | E -> System.Collections.Generic.KeyNotFoundException "Empty tree" |> raise
    | T(_,_,_,r) -> r

  /// Returns an element's sub-tree if that element is present in the Tree.
  let rec public get elt =
    function
    | E -> None
    | T(_,l,y,r) as node ->
        if elt = y then Some(node)
        else if elt < y then get elt l
        else get elt r

  /// Checks whether an element is present in the Tree.
  let public contains elt t = get elt t |> Option.isSome

  /// Determine the number of values in the Tree.
  let public count t =
    let rec sizeHelp n t =
      match t with
      | E -> n
      | T(_,l,_,r) -> sizeHelp (sizeHelp (n + 1) r) l
    sizeHelp 0 t

  /// Check if a tree is empty.
  let public isEmpty =
    function
    | E -> true
    | T(_) -> false

  /// Balance function for the tree
  let internal balance color left elt right =
    match right with
    | T(rC,rL,rV,rR) when rC = R ->
        match left with
        | T(lC,lL,lV,lR) when lC = R -> T(R,T(B,lL,lV,lR),elt,T(B,rL,rV,rR))
        | _ -> T(color,T(R,left,elt,rL),rV,rR)
    | _ ->
        match left with
        | T(lC,T(llC,llL,llV,llR),lV,lR) when lC = R && llC = R ->
            T(R,T(B,llL,llV,llR),lV,T(B,lR,elt,right))
        | _ -> T(color,left,elt,right)

  /// Insert helper.
  let rec internal insertHelper elt =
    function
    | E -> T(R,E,elt,E)
    | T(nC,nL,nV,nR) as node ->
        if elt < nV then balance nC (insertHelper elt nL) nV nR
        else if elt = nV then node
        else balance nC nL nV (insertHelper elt nR)

  /// Insert a value into a Tree.  No exception is raised if the Tree already
  /// contains the given element.
  let public insert elt t =
    match insertHelper elt t with
    | T(c,l,v,r) when c = R -> T(B,l,v,r)
    | insertedTree -> insertedTree

  let internal moveRedLeft t =
    match t with
    | T(_,T(_,lL,lV,lR),v,T(_,T(rlC,rlL,rlV,rlR),rV,rR)) when rlC = R ->
        T(R,T(B,T(R,lL,lV,lR),v,rlL),rlV,T(B,rlR,rV,rR))
    | T(c,T(_,lL,lV,lR),v,T(_,rL,rV,rR)) ->
        match c with
        | B -> T(B,T(R,lL,lV,lR),v,T(R,rL,rV,rR))
        | R -> T(B,T(R,lL,lV,lR),v,T(R,rL,rV,rR))
    | _ -> t

  let internal moveRedRight t =
    match t with
    | T(_,T(_,T(llC,llL,llV,llR),lV,lR),v,T(_,rL,rV,rR)) when llC = R ->
        T(R,T(B,llL,llV,llR),lV,T(B,lR,v,T(R,rL,rV,rR)))
    | T(c,T(_,lL,lV,lR),v,T(_,rL,rV,rR)) ->
        match c with
        | B -> T(B,T(R,lL,lV,lR),v,T(R,rL,rV,rR))
        | R -> T(B,T(R,lL,lV,lR),v,T(R,rL,rV,rR))
    | _ -> t

  /// Returns the lowest of all elements of the Tree.
  let rec internal getMin t =
    match t with
    | T(_,T(_),_,_) -> getLeftTree t |> getMin
    | _ -> t

  let rec internal removeMin t =
    match t with
    | T(c,T(lC,lL,_,_),v,r) ->
        let l = getLeftTree t
        match lC with
        | B ->
            match lL with
            | T(llC,_,_,_) when llC = R -> T(c,(removeMin l),v,r)
            | _ ->
                match moveRedLeft t with
                | T(nC,nL,nV,nR) -> balance nC (removeMin nL) nV nR
                | E -> E
        | R -> T(c,(removeMin l),v,r)
    | _ -> E

  let internal removeHelpPrepEQGT _elt t c l v r =
    match l with
    | T(lC,lL,lV,lR) when lC = R -> T(c,lL,lV,T(R,lR,v,r))
    | _ ->
        match r with
        | T(rC,T(rlC,_,_,_),_,_) when rC = B && rlC = B -> moveRedRight t
        | T(rC,rL,_,_) when rC = B && rL = E -> moveRedRight t
        | _ -> t

  /// When we find the node we are looking for, we can remove by replacing the
  /// key-value pair with the key-value pair of the left-most node on the right
  /// side (the closest pair).
  let rec internal removeHelpEQGT elt t =
    match t with
    | E -> E
    | T(c,l,v,r) ->
        if elt = v then
          match getMin r with
          | T(_,_,minV,_) -> balance c l minV (removeMin r)
          | E -> E
        else
          balance c l v (removeHelp elt r)

  /// The easiest thing to remove from the tree is a red node. However, when
  /// searching for the node to remove, we have no way of knowing if it will
  /// be red or not. This remove implementation makes sure that the bottom
  /// node is red by moving red colors down the tree through rotation and
  /// color flips. Any violations this will cause, can easily be fixed by
  /// balancing on the way up again.
  /// https://github.com/Skinney/core/blob/master/src/Dict.elm
  and internal removeHelp elt t =
    match t with
    | E -> E
    | T(c,l,v,r) ->
        if elt < v then
          match l with
          | T(lC,lL,_,_) when lC = B ->
              match lL with
              | T(llC,_,_,_) when llC = R -> T(c,(removeHelp elt l),v,r)
              | _ ->
                  match moveRedLeft t with
                  | E -> E
                  | T(nC,nL,nV,nR) -> balance nC (removeHelp elt nL) nV nR
          | _ -> T(c,(removeHelp elt l),v,r)
        else
          removeHelpPrepEQGT elt t c l v r |> removeHelpEQGT elt

  /// Removes a value from the Tree, if it exists.  Otherwise, no action is
  /// taken.
  let public remove elt t =
    match removeHelp elt t with
    | T(c,l,v,r) when c = R -> T(B,l,v,r)
    | x -> x

  /// Creates a singleton Tree, aka a tree with no children.
  let public singleton elt = T(B,E,elt,E)

  /// Fold over the elements in a Tree from lowest to highest. This function is
  /// NOT tail-recursive.
  let rec public fold func acc t =
    match t with
    | E -> acc
    | T(_,l,v,r) -> fold func (func (fold func acc l) v) r

  /// Fold over the elements in a Tree from lowest to highest. This function
  /// is tail recursive and uses the heap to manually track the remaining work
  /// (aka simulating the call stack) - this avoids a Stack Overflow Exception
  /// but could potentially cause an Out Of Memory Exception.
  ///
  /// https://en.wikibooks.org/wiki/F_Sharp_Programming/Mutable_Data
  let public fold' func acc t =
    // Manual stack, stored on the heap
    let stack = ref [ t ]

    // Helper that uses a heap-based manual stack
    let rec helper acc' =
      // Anything left to process?
      match !stack with
      | [] -> acc'
      | hd :: tl ->
          // If current node is `E`, move on to next item in stack
          match hd with
          | E ->
              stack := tl
              helper acc'
          | T(_,l,v,r) ->
              // Is there anything to the left?
              match l with
              | E ->
                  stack := r :: tl
                  helper(func acc' v)
              | T(_) ->
                  stack := l :: (T(R,E,v,r)) :: tl
                  helper acc'
    helper acc

  /// Fold over the elements in a Tree from highest to lowest. This function is
  /// NOT tail-recursive.
  let rec public foldBack func t acc =
    match t with
    | E -> acc
    | T(_,l,v,r) -> foldBack func l (func v (foldBack func r acc))

  /// Keep only the elements that pass the given test.
  let public filter isGood t =
    fold (fun acc elt ->
      if isGood elt then insert elt acc else acc) empty t

  /// Applies the given function to each element of the Tree. Returns the Tree
  /// comprised of the results for each element where the function returns Some.
  let public choose isGood t =
    fold (fun acc elt ->
      match isGood elt with
      | None -> acc
      | Some(x) -> insert x acc) empty t

  /// Partition a Tree according to some test. The first Tree contains all
  /// elements which passed the test, and the second contains the elements that
  /// did not.
  let public partition isGood t =
    let add (t1,t2) elt =
      if isGood elt then (insert elt t1),t2 else t1,insert elt t2
    fold add (empty,empty) t

  /// Convert an Array to a Tree
  let public ofArray(a: 'a array) =
    Array.fold (fun acc elt -> insert elt acc) empty a

  /// Convert a List to a Tree
  let public ofList(l: 'a list) =
    List.fold (fun acc elt -> insert elt acc) empty l

  /// Convert a Sequence to a Tree
  let public ofSeq(s: seq<'a>) =
    Seq.fold (fun acc elt -> insert elt acc) empty s

  /// Convert a Set to a Tree
  let public ofSet(s: Set<'a>) =
    Set.fold (fun acc elt -> insert elt acc) empty s

  /// Convert a Tree to an Array
  let public toArray t =
    fold (fun acc elt -> Array.singleton elt |> Array.append acc) Array.empty t

  /// Convert a Tree to a List
  let public toList t = foldBack (fun elt acc -> elt :: acc) t List.empty

  /// Convert a Tree to a Sequence
  let public toSeq t =
    fold (fun acc elt -> Seq.singleton elt |> Seq.append acc) Seq.empty t

  /// Convert a Tree to a Set
  let public toSet t = fold (fun acc elt -> Set.add elt acc) Set.empty t

  /// Apply a function to all values in a Tree
  let public map func t =
    fold (fun acc elt -> insert (func elt) acc) empty t

  /// Combine two Trees, similar to set union.  Inserts the values from t2 into t1.
  let public union t1 t2 = fold (fun acc elt -> insert elt acc) t1 t2

  /// Keep values that appear in both Trees, similar to set intersection.
  let public intersect t1 t2 = filter (fun elt -> contains elt t2) t1

  /// Returns a new Tree with the elements of the second Tree removed from the
  /// first. I.e. keep an element when it does not appear in the second Tree.
  let public difference t1 t2 = fold (fun acc elt -> remove elt acc) t1 t2

  /// Adds an element to the Tree.  No exception is raised if the Tree already
  /// contains the given element.
  let public add = insert

  /// For each element of the Tree, applies the given function. Concatenates all
  /// the results and return the combined Tree.
  let public collect func t =
    fold (fun acc elt -> func elt |> union acc) empty t

  /// Returns a new Tree that contains the elements of all the input Trees.
  let public concat ts = Seq.fold union empty ts

  /// Tests if any element of the Tree satisfies the given predicate.
  let rec public exists func t =
    fold (fun acc elt -> acc || func elt) false t

  /// Returns the first element for which the given function returns true.
  /// Return None if no such element exists.
  let public tryFind func t =
    fold (fun acc elt ->
      match acc with
      | None ->
          match func elt with
          | true -> Some elt
          | false -> None
      | Some _ -> acc) None t

  /// Returns the first element for which the given function returns true.
  let public find func t =
    match tryFind func t with
    | None -> raise <| System.Collections.Generic.KeyNotFoundException()
    | Some x -> x

  /// Tests if all elements of the collection satisfy the given predicate.
  let public forall func t = fold (fun acc elt -> acc && func elt) true t

  /// Creates a Tree by calling the given generator on each index.  Indexes run
  /// from `0` to `num - 1`.
  let public init num gen =
    let rec initHelper numLeft acc =
      if numLeft < 0 then
        acc
      else
        initHelper (numLeft - 1) (insert (gen numLeft) acc)
    initHelper (num - 1) empty

  /// Applies the given function to each element of the collection.
  let public iter func t = fold (fun _ elt -> func elt) () t

  /// Returns the height of the Tree.
  let public height t =
    let rec heightHelper t =
      match t with
      | E -> 0
      | T(_,l,_,r) -> 1 + max (heightHelper l) (heightHelper r)
    heightHelper t

  /// Returns the lowest element in the Tree, based on the ordering being used
  /// for the Tree.
  let rec public min t =
    match t with
    | E -> System.Collections.Generic.KeyNotFoundException "Empty tree" |> raise
    | T(_,E,v,_) -> v
    | T(_,T(_),_,_) -> getLeftTree t |> min

  /// Returns the lowest element in the Tree, based on the ordering being used
  /// for the Tree.
  let public minElement = min

  /// Returns the highest element in the Tree, based on the ordering being used
  /// for the Tree.
  let rec public max t =
    match t with
    | E -> System.Collections.Generic.KeyNotFoundException "Empty tree" |> raise
    | T(_,_,v,E) -> v
    | T(_,_,_,T(_)) -> getRightTree t |> max

  /// Returns the highest element in the Tree, based on the ordering being used
  /// for the Tree.
  let public maxElement = max

  /// Applies the given function to successive elements, returning the first
  /// result where function returns Some for some value. If no such element
  /// exists then return None.
  let public tryPick func t =
    fold (fun acc elt ->
      match acc with
      | None ->
          match func elt with
          | Some _ as x -> x
          | None -> None
      | Some _ -> acc) None t

  /// Applies the given function to successive elements from lowest to highest,
  /// returning the first result where function returns Some for some value.
  let public pick func t =
    match tryPick func t with
    | None -> raise <| System.Collections.Generic.KeyNotFoundException()
    | Some x -> x
