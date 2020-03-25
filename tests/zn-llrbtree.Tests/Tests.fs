module Tests

open Expecto
open ZN.DataStructures

/// https://github.com/Skinney/core/blob/master/tests/tests/Test/Dict.elm
/// Build Tests with Collections
[<Tests>]
let buildTestsFromCollections =
  testList "Build Tests from Collections"
    [ testCase "Empty 1" <| fun _ -> Expect.equal LLRBTree.empty E "Empty 1"
      testCase "Empty 2"
      <| fun _ ->
        Expect.allEqual
          [ LLRBTree.ofList []
            LLRBTree.ofArray [||]
            LLRBTree.ofSeq Seq.empty
            LLRBTree.ofSet Set.empty ] LLRBTree.empty "Empty 2"
      testCase "IsEmpty" <| fun _ ->
        Expect.allEqual
          [ LLRBTree.ofList [] |> LLRBTree.isEmpty
            LLRBTree.ofArray [||] |> LLRBTree.isEmpty
            LLRBTree.ofSeq Seq.empty |> LLRBTree.isEmpty
            LLRBTree.ofSet Set.empty |> LLRBTree.isEmpty
            LLRBTree.empty |> LLRBTree.isEmpty ] true "IsEmpty"
      testCase "Is Not Empty"
      <| fun _ ->
        Expect.allEqual
          [ LLRBTree.ofList [ "a" ] |> LLRBTree.isEmpty
            LLRBTree.ofArray [| "a" |] |> LLRBTree.isEmpty
            LLRBTree.ofSeq [ "a" ] |> LLRBTree.isEmpty
            Set.singleton "a"
            |> LLRBTree.ofSet
            |> LLRBTree.isEmpty
            LLRBTree.singleton "a" |> LLRBTree.isEmpty ] false "Is Not Empty"
      testCase "Init" <| fun _ ->
        let t = LLRBTree.init 50 id
        let l = List.init 50 id
        let s = Seq.init 50 id
        let a = Array.init 50 id
        Expect.equal (LLRBTree.count t) 50 "Init count"
        Expect.equal (LLRBTree.min t) 0 "Init min"
        Expect.equal (LLRBTree.max t) 49 "Init max"
        Expect.equal (LLRBTree.minElement t) 0 "Init minElement"
        Expect.equal (LLRBTree.maxElement t) 49 "Init maxElement"
        Expect.allEqual
          [ l
            Seq.toList s
            Array.toList a ] (LLRBTree.toList t) "Init vs. list, seq, array"
        ()
      testCase "Singleton"
      <| fun _ ->
        Expect.allEqual
          [ LLRBTree.ofList [ "a" ]
            LLRBTree.ofArray [| "a" |]
            LLRBTree.ofSeq(seq { "a" })
            LLRBTree.ofSet(Set.singleton "a") ] (LLRBTree.singleton "a")
          "Singleton"
      testCase "Insert"
      <| fun _ ->
        Expect.allEqual
          [ LLRBTree.ofList [ "a" ]
            LLRBTree.ofArray [| "a" |]
            LLRBTree.ofSeq(seq { "a" })
            LLRBTree.ofSet(Set.singleton "a") ]
          (LLRBTree.insert "a" LLRBTree.empty) "Insert"
      testCase "Idempotent Add" <| fun _ ->
        let t =
          LLRBTree.empty
          |> LLRBTree.add 5
          |> LLRBTree.add 5
          |> LLRBTree.add 5
          |> LLRBTree.add 5
        Expect.equal (LLRBTree.count t) 1 "Idempotent Add"
        Expect.equal (LLRBTree.getValue t) 5 "Idempotent Add value"
      testCase "Remove"
      <| fun _ ->
        Expect.allEqual
          [ LLRBTree.ofList [ "a" ] |> LLRBTree.remove "a"
            LLRBTree.ofArray [| "a" |] |> LLRBTree.remove "a"
            LLRBTree.ofSeq(seq { "a" }) |> LLRBTree.remove "a"
            LLRBTree.ofSet(Set.singleton "a") |> LLRBTree.remove "a"
            LLRBTree.singleton "a" |> LLRBTree.remove "a"
            LLRBTree.insert "a" LLRBTree.empty |> LLRBTree.remove "a" ]
          LLRBTree.empty "Remove"
      testCase "Idempotent Remove" <| fun _ ->
        let t =
          LLRBTree.empty
          |> LLRBTree.add 5
          |> LLRBTree.add 6
          |> LLRBTree.remove 5
          |> LLRBTree.remove 5
          |> LLRBTree.remove 5
          |> LLRBTree.remove 5
        Expect.equal (LLRBTree.count t) 1 "Idempotent Remove"
        Expect.equal (LLRBTree.getValue t) 6 "Idempotent Remove value"
      testCase "Remove Not Found"
      <| fun _ ->
        Expect.allEqual
          [ LLRBTree.ofList [ "a" ] |> LLRBTree.remove "b"
            LLRBTree.ofArray [| "a" |] |> LLRBTree.remove "b"
            LLRBTree.ofSeq(seq { "a" }) |> LLRBTree.remove "b"
            LLRBTree.ofSet(Set.singleton "a") |> LLRBTree.remove "b"
            LLRBTree.singleton "a" |> LLRBTree.remove "b"
            LLRBTree.insert "a" LLRBTree.empty |> LLRBTree.remove "b" ]
          (LLRBTree.singleton "a") "Remove Not Found"
      testCase "Transition between collections"
      <| fun _ ->
        Expect.equal
          ([ "a";"b";"c" ]
           |> LLRBTree.ofList
           |> LLRBTree.toList
           |> LLRBTree.ofList
           |> LLRBTree.toArray
           |> LLRBTree.ofArray
           |> LLRBTree.toSeq
           |> LLRBTree.ofSeq
           |> LLRBTree.toSet
           |> LLRBTree.ofSet
           |> LLRBTree.toList) [ "a";"b";"c" ] "Transition between collections" ]

/// Query Tests
[<Tests>]
let queryTests =
  let animals = [ "Dog";"Cat";"Mongoose";"Rat" ] |> LLRBTree.ofList
  testList "Query tests"
    [ testCase "Member 1" <| fun _ ->
        Expect.allEqual
          [ LLRBTree.contains "Dog" animals
            LLRBTree.contains "Cat" animals
            LLRBTree.contains "Mongoose" animals
            LLRBTree.contains "Rat" animals ] true "Member 1"
      testCase "Member 2"
      <| fun _ ->
        Expect.allEqual
          [ LLRBTree.contains "Donkey" animals
            LLRBTree.contains "Monkey" animals
            LLRBTree.contains "Elephant" animals
            LLRBTree.contains "Giraffe" animals
            LLRBTree.contains "Zebra" animals ] false "Member 2"
      testCase "Get 1" <| fun _ ->
        Expect.allEqual
          [ LLRBTree.get "Cat" animals
            |> Option.get
            |> LLRBTree.getValue ] "Cat" "Get 1"
      testCase "Get 2" <| fun _ ->
        Expect.allEqual
          [ LLRBTree.get "Dog" animals
            |> Option.get
            |> LLRBTree.getValue ] "Dog" "Get 2"
      testCase "Get 3" <| fun _ ->
        Expect.allEqual
          [ LLRBTree.get "Mongoose" animals
            |> Option.get
            |> LLRBTree.getValue ] "Mongoose" "Get 3"
      testCase "Get 4" <| fun _ ->
        Expect.allEqual
          [ LLRBTree.get "Rat" animals
            |> Option.get
            |> LLRBTree.getValue ] "Rat" "Get 4"
      testCase "Get 5" <| fun _ ->
        Expect.allEqual
          [ LLRBTree.get "Aardvark" animals
            LLRBTree.get "Chicken" animals
            LLRBTree.get "Zebu" animals
            LLRBTree.get "Sailfish" animals
            LLRBTree.get "Narwhal" animals
            LLRBTree.get "Eagle" animals ] None "Get 5" ]

/// Exception tests
[<Tests>]
let exceptionTests =
  testList "Exception Tests"
    [ testCase "getValue"
      <| fun _ ->
        Expect.throwsT<System.Collections.Generic.KeyNotFoundException> (fun () ->
          LLRBTree.getValue LLRBTree.empty |> ignore) "getvalue"
      testCase "getLeftTree"
      <| fun _ ->
        Expect.throwsT<System.Collections.Generic.KeyNotFoundException> (fun () ->
          LLRBTree.getLeftTree LLRBTree.empty |> ignore) "getLeftTree"
      testCase "getRightTree"
      <| fun _ ->
        Expect.throwsT<System.Collections.Generic.KeyNotFoundException> (fun () ->
          LLRBTree.getRightTree LLRBTree.empty |> ignore) "getRightTree"
      testCase "min"
      <| fun _ ->
        Expect.throwsT<System.Collections.Generic.KeyNotFoundException>
          (fun () -> LLRBTree.min LLRBTree.empty) "min"
      testCase "minElement"
      <| fun _ ->
        Expect.throwsT<System.Collections.Generic.KeyNotFoundException>
          (fun () -> LLRBTree.minElement LLRBTree.empty) "minElement"
      testCase "max"
      <| fun _ ->
        Expect.throwsT<System.Collections.Generic.KeyNotFoundException>
          (fun () -> LLRBTree.max LLRBTree.empty) "max"
      testCase "maxElement"
      <| fun _ ->
        Expect.throwsT<System.Collections.Generic.KeyNotFoundException>
          (fun () -> LLRBTree.maxElement LLRBTree.empty) "maxElement"
      testCase "find"
      <| fun _ ->
        Expect.throwsT<System.Collections.Generic.KeyNotFoundException> (fun () ->
          LLRBTree.find (fun _ -> true) LLRBTree.empty |> ignore) "find"
      testCase "pick"
      <| fun _ ->
        Expect.throwsT<System.Collections.Generic.KeyNotFoundException> (fun () ->
          LLRBTree.pick Some LLRBTree.empty |> ignore) "pick" ]

/// Tests for Options
[<Tests>]
let optionTests =
  testList "Option Tests"
    [ testCase "find and tryFind"
      <| fun _ ->
        let t = LLRBTree.singleton 5
        Expect.throwsT<System.Collections.Generic.KeyNotFoundException> (fun () ->
          LLRBTree.find (fun e -> e = 6) t |> ignore) "find not found"
        Expect.equal (LLRBTree.find (fun e -> e = 5) t) 5 "find found"
        Expect.isNone (LLRBTree.tryFind ((=) 6) t) "tryFind not found"
        Expect.equal (LLRBTree.tryFind ((=) 5) t) (Some(5)) "tryFind found"
      testCase "pick and tryPick"
      <| fun _ ->
        let t = LLRBTree.singleton 5
        Expect.throwsT<System.Collections.Generic.KeyNotFoundException> (fun () ->
          LLRBTree.pick (fun _e -> None) t |> ignore) "pick not found"
        Expect.equal (LLRBTree.pick (fun e -> Some(e = 5)) t) true "pick found"
        Expect.isNone (LLRBTree.tryPick Some LLRBTree.empty)
          "tryPick not picked"
        Expect.equal (LLRBTree.tryPick (fun e -> Some(e = 5)) t) (Some(true))
          "tryPick picked" ]

///
[<Tests>]
let miscTests =
  testList "Miscellaneous Tests"
    [ testCase "height" <| fun _ ->
        Expect.equal (LLRBTree.height LLRBTree.empty) 0 "height 0"
        let t = LLRBTree.singleton 5
        Expect.equal (LLRBTree.height t) 1 "height 1"
      testCase "iter"
      <| fun _ ->
        let t = LLRBTree.singleton 5
        Expect.equal (LLRBTree.iter (fun elt -> printf "%A, " elt) t) () "iter"
      testCase "exists" <| fun _ ->
        let t = LLRBTree.singleton 5
        Expect.isFalse (LLRBTree.exists ((=) 6) t) "exists false"
        Expect.isTrue (LLRBTree.exists ((=) 5) t) "exists true"
      testCase "concat" <| fun _ ->
        let l = List.init 50 id
        let t = List.map LLRBTree.singleton l |> LLRBTree.concat
        Expect.equal (List.length l) (LLRBTree.count t) "concat count"
        Expect.equal (LLRBTree.toList t) l "concat vs. list"
      testCase "map" <| fun _ ->
        let l = List.init 50 id
        let t = LLRBTree.ofList l
        let l2 = List.map ((+) 2) l
        let t2 = LLRBTree.map ((+) 2) t
        Expect.equal (List.length l) (LLRBTree.count t) "map count"
        Expect.equal (List.length l2) (LLRBTree.count t2) "map count 2"
        Expect.equal (LLRBTree.toList t) l "map vs. list"
        Expect.equal (LLRBTree.toList t2) l2 "map vs. list 2"
      testCase "choose" <| fun _ ->
        let l = List.init 50 id
        let t = LLRBTree.ofList l

        let t2 =
          LLRBTree.choose (fun e ->
            if e < 25 then Some e else None) t
        Expect.equal (LLRBTree.count t2) 25 "choose count"
        Expect.equal (List.filter (fun e -> e < 25) l) (LLRBTree.toList t2)
          "choose vs. list"
        Expect.equal (LLRBTree.min t2) 0 "choose min"
        Expect.equal (LLRBTree.max t2) 24 "choose max"
      testCase "partition" <| fun _ ->
        let l = List.init 50 id
        let l0 = List.filter (fun e -> e < 25) l
        let l25 = List.filter (fun e -> e > 24) l
        let t = LLRBTree.ofList l
        let t0,t25 = LLRBTree.partition (fun e -> e < 25) t
        Expect.equal (List.length l) (List.length l0 + List.length l25)
          "partition lists by length"
        Expect.equal (List.concat [ l0;l25 ]) l "partition lists are equal"
        Expect.equal (LLRBTree.concat [ t0;t25 ]) t "partition trees are equal"
        Expect.equal (LLRBTree.count t)
          (LLRBTree.count t0 + LLRBTree.count t25) "partition trees by length"
        Expect.equal l0 (LLRBTree.toList t0) "partition t0 vs l0"
        Expect.equal l25 (LLRBTree.toList t25) "partition t25 vs l25"
        Expect.equal (LLRBTree.min t0) 0 "partition t0 min"
        Expect.equal (LLRBTree.max t0) 24 "partition t0 max"
        Expect.equal (LLRBTree.min t25) 25 "partition t25 min"
        Expect.equal (LLRBTree.max t25) 49 "partition t25 max" ]
