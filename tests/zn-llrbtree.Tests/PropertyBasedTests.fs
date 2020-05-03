module PropertyBasedTests

open Expecto
open Hedgehog
open ZN.DataStructures

/// Ensures that the data structure keeps the values sorted / in order.
let ``Values are in order`` =
  testCase "Values are in order" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 1 5000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      Expect.equal (LLRBTree.toList t) (Array.sort g |> Array.toList)
        "Values are in order"
    }
    |> Property.check

/// Ensures that `min` and `max` find the lowest and highest values,
/// respectively.
let ``Min and Max`` =
  testCase "Min and Max" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 1 5000
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
let Add =
  testCase "Add" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 5000
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
let Remove =
  testCase "Remove" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 5000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t =
        Array.fold (fun acc elt -> LLRBTree.add elt acc) LLRBTree.empty g
      let t2 = Array.fold (fun acc elt -> LLRBTree.remove elt acc) t g
      Expect.equal (LLRBTree.count t2) 0 "Remove count"
      Expect.equal t2 LLRBTree.empty "Remove tree"
    }
    |> Property.check

let ``Add and Remove`` =
  testCase "Add and Remove" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 5000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let! h = Gen.array <| Range.exponential 0 5000
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

let ``Difference with itself`` =
  testCase "Difference with itself" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 5000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      let t2 = LLRBTree.difference t t
      Expect.equal (LLRBTree.count t2) 0 "Difference with itself count"
      Expect.equal t2 LLRBTree.empty "Difference with itself tree"
    }
    |> Property.check

let ``Union with itself`` =
  testCase "Union with itself" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 5000
               <| Gen.int
                    (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      let t2 = LLRBTree.union t t
      Expect.equal t t2 "Union with itself"
      Expect.equal (LLRBTree.count t) (LLRBTree.count t2)
        "Union with itself count"
    }
    |> Property.check

let ``tryPick and Pick`` =
  testCase "tryPick and Pick" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 5000
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

let ``Forall`` =
  testCase "Forall" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 1 5000
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

let ``Collect`` =
  testCase "Collect" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 1 5000
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

let ``Intersection with itself`` =
  testCase "Intersection with itself" <| fun _ ->
    property {
      let! g = Gen.array <| Range.exponential 0 5000
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

let ``Fold and FoldBack`` =
  testCase "Fold and FoldBack" <| fun _ ->
    property {
      let! g= Gen.array<| Range.exponential 1 5000 <| Gen.int (Range.constant System.Int32.MinValue System.Int32.MaxValue)
      let t = LLRBTree.ofArray g
      let t1 = LLRBTree.fold (fun acc e -> LLRBTree.add e acc) LLRBTree.empty t
      let t2 = LLRBTree.foldBack LLRBTree.add t LLRBTree.empty

      Expect.equal (LLRBTree.toList t1) (LLRBTree.toList t2) "Fold and FoldBack"
    }
    |> Property.check

[<Tests>]
let addTests =
  testList "Property-Based Tests"
    [
      Add
      ``Add and Remove``
      Collect
      ``Fold and FoldBack``
      Forall
      ``Difference with itself``
      ``Intersection with itself``
      ``Min and Max``
      Remove
      ``tryPick and Pick``
      ``Union with itself``
      ``Values are in order``
       ]
