module PropertyBasedTests

open Expecto
open Hedgehog
open ZN.DataStructures

/// Ensures that the data structure keeps the values sorted / in order.
[<Tests>]
let ``Values are in order`` =
  testCase "Values are in order" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 1 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      Expect.equal (LLRBTree.toList t) (Array.sort g |> Array.toList)
        "Values are in order"
    }
    |> Property.check

/// Ensures that `min` and `max` find the lowest and highest values,
/// respectively.
[<Tests>]
let ``Min and Max`` =
  testCase "Min and Max" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 1 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      Expect.equal (LLRBTree.min t) (Array.min g) "Min"
      Expect.equal (LLRBTree.max t) (Array.max g) "Max"
      if LLRBTree.min t = LLRBTree.max t then
        Expect.equal (Array.min g) (Array.max g) "Min = Max"
      if LLRBTree.min t < LLRBTree.max t then
        Expect.isLessThan (Array.min g) (Array.max g) "Min < Max"
      if LLRBTree.min t > LLRBTree.max t then failwith "Min > Max"
    }
    |> Property.check

/// Tests the `ofArray` function.
[<Tests>]
let Add =
  testCase "Add" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      Expect.equal (LLRBTree.count t) (Array.distinct g |> Array.length)
        "Add equal counts"
      Expect.equal (LLRBTree.toArray t) (Array.distinct g |> Array.sort)
        "Add equal contents"
    }
    |> Property.check

/// Test the `remove` function.
[<Tests>]
let Remove =
  testCase "Remove" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t =
        Array.fold (fun acc elt -> LLRBTree.add elt acc) LLRBTree.empty g
      let t2 = Array.fold (fun acc elt -> LLRBTree.remove elt acc) t g
      Expect.equal (LLRBTree.count t2) 0 "Remove count"
      Expect.equal t2 LLRBTree.empty "Remove tree"
    }
    |> Property.check

[<Tests>]
let ``Add and Remove`` =
  testCase "Add and Remove" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let! h = Gen.array <| Range.exponential 0 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t =
        Array.fold (fun acc elt -> LLRBTree.add elt acc) LLRBTree.empty g
      let t2 = Array.fold (fun acc elt -> LLRBTree.remove elt acc) t h
      Expect.isLessThanOrEqual (LLRBTree.count t2) (LLRBTree.count t)
        "Add and Remove count"
      Expect.equal (LLRBTree.count t2)
        (Set.difference (Set.ofArray g) (Set.ofArray h) |> Set.count)
        "Add and Remove set count"
      Expect.equal (Set.difference (Set.ofArray g) (Set.ofArray h))
        (LLRBTree.toSet t2) "Add and Remove set"
    }
    |> Property.check

[<Tests>]
let ``Difference with itself`` =
  testCase "Difference with itself" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      let t2 = LLRBTree.difference t t
      Expect.equal (LLRBTree.count t2) 0 "Difference with itself count"
      Expect.equal t2 LLRBTree.empty "Difference with itself tree"
    }
    |> Property.check

[<Tests>]
let ``Union with itself`` =
  testCase "Union with itself" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      let t2 = LLRBTree.union t t
      Expect.equal t t2 "Union with itself"
      Expect.equal (LLRBTree.count t) (LLRBTree.count t2)
        "Union with itself count"
    }
    |> Property.check

[<Tests>]
let ``tryPick and Pick`` =
  testCase "tryPick and Pick" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      let rnd = System.Random()

      let picker =
        fun e ->
          if e = rnd.Next() then Some e else None

      let tp = LLRBTree.tryPick picker t
      match tp with
      | None ->
          Expect.throwsT<System.Collections.Generic.KeyNotFoundException> (fun () ->
            LLRBTree.pick picker t |> ignore) "tryPick and Pick"
      | Some(x) ->
          Expect.equal x (LLRBTree.pick picker t) "tryPick and Pick"
    }
    |> Property.check

[<Tests>]
let Forall =
  testCase "Forall" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 1 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      Expect.isTrue
        (LLRBTree.forall (fun e ->
          e >= System.Int32.MinValue && e <= System.Int32.MaxValue) t) "forall"
      Expect.equal (LLRBTree.forall (fun e -> e >= 0) t) (LLRBTree.min t >= 0)
        "forall vs. min"
    }
    |> Property.check

[<Tests>]
let Collect =
  testCase "Collect" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 1 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      Expect.equal (LLRBTree.toList t)
        (LLRBTree.collect LLRBTree.singleton t |> LLRBTree.toList) "collect"
      Expect.equal (LLRBTree.count t)
        (LLRBTree.collect LLRBTree.singleton t |> LLRBTree.count)
        "collect count"
    }
    |> Property.check

[<Tests>]
let ``Intersection with itself`` =
  testCase "Intersection with itself" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      let t2 = LLRBTree.intersect t t
      Expect.equal (LLRBTree.toList t) (LLRBTree.toList t2)
        "Intersection with itself"
      Expect.equal (LLRBTree.count t) (LLRBTree.count t2)
        "Intersection with itself count"
    }
    |> Property.check

[<Tests>]
let ``Fold and FoldBack`` =
  testCase "Fold and FoldBack" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 1 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      let t1 =
        LLRBTree.fold (fun acc e -> LLRBTree.add e acc) LLRBTree.empty t
      let t2 = LLRBTree.foldBack LLRBTree.add t LLRBTree.empty

      Expect.equal (LLRBTree.toList t1) (LLRBTree.toList t2)
        "Fold and FoldBack"
    }
    |> Property.check

/// https://www.geeksforgeeks.org/left-leaning-red-black-tree-insertion/
// [<Tests>]
// let ``Invariant 1 Root node is always black`` =
//   testCase "Invariant 1 Root node is always black" <| fun _ ->
//     property {
//       let! g = Gen.array <| Range.exponential 0 2000
//                <| Gen.int
//                     (Range.constant System.Int32.MinValue System.Int32.MaxValue)

//       Expect.equal (LLRBTree.ofArray g |> LLRBTree.getColor) Color.B
//         "Root node of PB tree is black"
//       Expect.equal (LLRBTree.getColor LLRBTree.empty) Color.B
//         "Root node of empty tree is black"
//       Expect.equal (LLRBTree.singleton 'x' |> LLRBTree.getColor) Color.B
//         "Root of singleton is black"
//     }
//     |> Property.check

/// https://www.geeksforgeeks.org/left-leaning-red-black-tree-insertion/
// [<Tests>]
// let ``Invariant 4 Node cannot have Left Black and Right Red child`` =
//   // True = node does not have left black and right red children
//   let childChecker t =
//     match t with
//     | E -> true
//     | T(_,l,_,r) ->
//         (LLRBTree.getColor l = B && LLRBTree.getColor r = R) |> not

//   // Walks through the tree and performs operations on nodes instead of values
//   let rec foldNodes func acc t =
//     match t with
//     | E -> acc
//     | T(_,l,_,r) -> foldNodes func (foldNodes func (func acc t) l) r

//   testCase "Invariant 4 Node cannot have Left Black and Right Red child" <| fun _ ->
//     property {
//       let! g = Gen.array <| Range.exponential 0 3000
//                <| Gen.int
//                     (Range.constant System.Int32.MinValue System.Int32.MaxValue)
//       let result =
//         LLRBTree.ofArray g
//         |> foldNodes (fun acc e -> acc && (childChecker e)) true

//       Expect.isTrue result "Invariant 4"
//     }
//     |> Property.check

[<Tests>]
let ``fold and fold' have the same behavior`` =
  testCase "fold and fold' for all integers" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 2000
               <| Gen.int
                    (Range.constant System.Int32.MinValue
                       (System.Int32.MaxValue - 10))
      let t = LLRBTree.ofArray g

      let t1 =
        LLRBTree.fold (fun acc e -> LLRBTree.add e acc) LLRBTree.empty t
      let t2 =
        LLRBTree.fold' (fun acc e -> LLRBTree.add e acc) LLRBTree.empty t

      Expect.equal (LLRBTree.toList t1) (LLRBTree.toList t2)
        "List from trees are identical"

      // The following line fails on macOS on .NET Core SDK 3.1.201, passes
      // all other checks (.NET Core 3.1.201 / .NET 4.8 on Linux and Windows,
      // .NET 4.8 on macOS)
      Expect.equal t1 t2 "Trees are identical"

      let t3 = LLRBTree.fold (fun acc e -> (e + 1) :: acc) [] t
      let t4 = LLRBTree.fold' (fun acc e -> (e + 1) :: acc) [] t
      Expect.equal t3 t4 "Lists based on +1 are identical"
    }
    |> Property.check
