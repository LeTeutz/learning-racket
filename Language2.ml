(* ------- TYPES ---------- *)

type exprC =
  | TrueC 
  | FalseC 
  | NilC 
  | NumC of int 
  | IdC of string
  | AppC of exprC * exprC
  | PlusC of exprC * exprC
  | MultC of exprC * exprC
  | LamC of string * exprC
  | IfC of exprC * exprC * exprC
  | EqNumC of exprC * exprC 
  | LtC of exprC * exprC
  | ConsC of exprC * exprC
  | HeadC of exprC
  | TailC of exprC 
  | IsNilC of exprC
  | IsListC of exprC
  | BoxC of exprC
  | UnboxC of exprC
  | SetboxC of exprC * exprC
  | SetC of string * exprC
  | SeqC of exprC * exprC
 
type value = 
  | NumV of int
  | BoolV of bool
  | NilV
  | ConsV of value * value
  | ClosV of string * exprC * int
  | BoxV of int
  | Nothing

type slot = 
  | Slot of string * int

type link = 
  | Link of string * int

type frame = 
  | Frame of link list * slot list

module Heap = Map.Make(struct type t = int let compare = compare end)
module Store = Map.Make(struct type t = int let compare = compare end)

(* ------- HELPERS ---------- *)

let fTuple (a, b) = a
let sTuple (a, b) = b

let frameLinks = function
  | Frame(links, _) -> links

let frameSlots = function
  | Frame(_, slots) -> slots

let slotName = function
  | Slot(name, _) -> name

let slotValue = function
  | Slot(_, value) -> value

let linkLabel = function
  | Link(label, _) -> label

let linkID = function
  | Link(_, id) -> id

let isNumV = function
  | NumV(_)-> true
  | _ -> false

let getNumV = function
  | NumV(value) -> value
  | _ -> failwith "Not a NumV"

let closureArg = function
  | ClosV(arg, _, _) -> arg
  | _ -> failwith "Not a Clos"

let closureBody = function
  | ClosV(_, body, _) -> body
  | _ -> failwith "Not a Clos"

let closureFrameId = function
  | ClosV(_, _, frameId) -> frameId
  | _ -> failwith "Not a Clos"

let boolToString = function 
  | true -> "true"
  | false -> "false"  

let getBoxLocation = function
  | BoxV(location) -> location
  | _ -> failwith "Not a Box"

let printValueUnit = function
  | BoolV b -> Printf.printf "Boolean: %s" (boolToString b)
  | NumV v -> Printf.printf "%d \n" v
  | NilV -> Printf.printf "Nil \n"
  | BoxV loc -> Printf.printf "Box loc%d\n" loc
  | ConsV(h, t) -> Printf.printf "Cons\n" 
  | ClosV(arg, body, id) -> Printf.printf "Argument : (%s)\n" arg
  | Nothing -> Printf.printf "Nothing"

let printValueString = function
  | BoolV b -> boolToString b
  | NumV v -> Int.to_string v
  | NilV -> "Nil"
  | BoxV loc -> Int.to_string loc
  | ConsV(h, t) -> "Cons"
  | ClosV(arg, body, id) -> arg
  | Nothing -> "Nothing"

let containsValue = function 
  | None -> Nothing
  | Some x -> x

let containsInt = function
  | None -> -1
  | Some x -> x

  (* ------- DEBUGGING HELPERS ---- *)

let rec printFrameLinks = function
  | [] -> Printf.printf "\n"
  | head :: tail -> (
        Printf.printf "\t\t\t%s -> %d\n" (linkLabel head) (linkID head);
        printFrameLinks tail)

let rec printFrameSlots = function
  | [] -> Printf.printf "\n"
  | head :: tail -> (
        Printf.printf "\t\t\t%s -> %d\n " (slotName head) (slotValue head);
        printFrameSlots tail)  

let printCurrentFrame = function
  | Frame(links, slots) -> 
    Printf.printf "\t\tLinks:\n";
    printFrameLinks links;
    Printf.printf "\t\t------\n";
    Printf.printf "\t\tSlots:\n";
    printFrameSlots slots;
    Printf.printf "\n"

let rec printCurrentHeap heap = 
  match heap with
    | (key, frame) :: tail -> 
        Printf.printf "\tKey: %d\n" key;
        Printf.printf "\t\tFrame: \n";
        printCurrentFrame frame;
        printCurrentHeap tail
    | _ -> Printf.printf "\n"

let rec printCurrentStore store = 
  match store with
    | (loc, v) :: tail -> 
        Printf.printf "Loc: %d, Value: %s\n" loc (printValueString v);
        printCurrentStore tail
    | _ -> Printf.printf "\n"

let printHeapStore heap store = 
  Printf.printf "Heap:\n\n";
  printCurrentHeap (Heap.bindings heap);
  Printf.printf "----------------\n";    
  Printf.printf "Store:\n";
  printCurrentStore (Store.bindings store);
  Printf.printf "****************\n" 

(* ------- HEAP METHODS ---------- *)

let initFrame heap = 
  let frameID = Heap.cardinal heap in
  let frame = Frame([], []) in
  (Heap.add frameID frame heap, frameID)

let getFrame frameId heap = 
  try Heap.find frameId heap with Not_found -> (Printf.printf "Frame with id %d not found\n" frameId; failwith "NOT FOUND")

let setFrame frameId frame heap = 
  Heap.add frameId frame heap

let rec findSlot slots slotName = 
  match slots with 
  | [] -> None
  | Slot(name, value) :: rest -> 
    if name = slotName then Some(value)
    else findSlot rest name   

let getSlotValue frameId slotName heap = 
  let frame = getFrame frameId heap in
  let slots = frameSlots frame in
  findSlot slots slotName

let updateSlotValue frameId slotName newValue heap = 
  let frame = getFrame frameId heap in
  let slots = frameSlots frame in
  if (findSlot slots slotName) = None then
    let newSlots = Slot(slotName, newValue) :: slots in
    let newFrame = Frame(frameLinks frame, newSlots) in
    setFrame frameId newFrame heap
  else
    let slots = List.map (function
      | Slot(name, value) -> 
        if name = slotName then Slot(name, newValue) else Slot(name, value)) slots in
    let newFrame = Frame(frameLinks frame, slots) in
    setFrame frameId newFrame heap

let rec findLink links linkLabel = 
  match links with
  | [] -> None
  | Link(label, id) :: rest -> 
    if label = linkLabel then Some(id)
    else findLink rest label

let int_of_intoption = function None -> 0 | Some n -> n

let getLink frameId linkLabel heap = 
  let frame = getFrame frameId heap in
  let links = frameLinks frame in
  containsInt (findLink links linkLabel)

let createLink childFrameId parentFrameId linkLabel heap = 
  let frame = getFrame childFrameId heap in
  let links = frameLinks frame in
  let link = Link(linkLabel, parentFrameId) in
  let newLinks = link :: links in
  let newFrame = Frame(newLinks, frameSlots frame) in
  setFrame childFrameId newFrame heap

let rec lookup name frameId heap = 
  let frame = getFrame frameId heap in
  let slot = getSlotValue frameId name heap in
  let links = frameLinks frame in

  match slot with
    | None -> 
      if List.length links > 0 then 
        lookup name (getLink frameId "P" heap) heap
      else 
        (printCurrentFrame frame;
            Printf.printf "Error: %s not found in frame %d \n" name frameId;
        failwith "lookup: no slot")
    | Some v -> v

(* ------- STORE METHODS ---------- *)

let initStore =
  let store = Store.empty in
  store

let fetch loc sto = 
  try Store.find loc sto with Not_found -> (Printf.printf "Location %d not found\n" loc; failwith "NOT FOUND")
  
let updateCell loc v sto = 
  let newSto = Store.add loc v sto in
  newSto

let newLocation sto = 
  Store.cardinal sto

let extendStore v sto = 
  let newLoc = Store.cardinal sto in 
  let newSto = Store.add newLoc v sto in
  (newSto, newLoc)

(* let addBoxToStore s *)

(* ------- INTERPRETOR METHODS ---------- *)

let numPlus l r = 
  if isNumV(l) && isNumV(r) then
    NumV(getNumV(l) + getNumV(r))
  else
    failwith "One argument was not a number"

let numMult l r = 
  if isNumV(l) && isNumV(r) then
    NumV(getNumV(l) * getNumV(r))
  else
    failwith "One argument was not a number"

let ifc c t e = 
  match c with
    | BoolV(true) -> t
    | BoolV(false) -> e
    | _ -> failwith "If: invalid args"

let eqc l r = 
  if isNumV(l) && isNumV(r) then
    BoolV(getNumV(l) = getNumV(r))
  else
    failwith "Eq: invalid args"

let ltc l r = 
  if isNumV(l) && isNumV(r) then
    BoolV(getNumV(l) < getNumV(r))
  else
    failwith "Lt: invalid args"

let headc l = 
  match l with
    | ConsV(h, t) -> h
    | _ -> failwith "Head: invalid args"

let tailc l = 
  match l with
    | ConsV(h, t) -> t
    | _ -> failwith "Tail: invalid args"

let isheadc l = 
  match l with
    | ConsV(h, t) -> BoolV(true)
    | _ -> BoolV(false)

let istailc l = 
  match l with
    | ConsV(h, t) -> BoolV(false)
    | _ -> BoolV(true)

(* ------- INTERPRETOR ---------- *)

let heap = ref Heap.empty
let store: value Store.t ref = ref Store.empty
(* let store = ref Store.empty *)

let rec interp expr frID = 
  match expr with
  | TrueC -> BoolV true
  | FalseC -> BoolV false
  | NumC n -> NumV n
  | NilC -> NilV
  | PlusC(left, right) -> numPlus (interp left frID) (interp right frID)
  | MultC(left, right) -> numMult (interp left frID) (interp right frID)
  | IfC(cond, thn, els) -> ifc (interp cond frID) (interp thn frID) (interp els frID)
  | EqNumC(left, right) -> eqc (interp left frID) (interp right frID)
  | LtC(left, right) -> ltc (interp left frID) (interp right frID)
  | ConsC(head, tail) -> ConsV((interp head frID), (interp tail frID))
  | HeadC cons -> headc (interp cons frID)
  | TailC cons -> tailc (interp cons frID)
  | IsNilC cons -> isheadc (interp cons frID)
  | IsListC cons -> istailc (interp cons frID)
  | IdC name -> 
      (let location = lookup name frID !heap in
      fetch location !store)
  | LamC(sym, e) -> ClosV(sym, e, frID)
  | BoxC(e) -> 
      (let ex = interp e frID in
       let newLoc = Store.cardinal !store in
       let newSto = Store.add newLoc ex !store in
       store := newSto;
       BoxV(newLoc)) 
  | UnboxC(box) -> 
      (let b = interp box frID in
      let v = fetch (getBoxLocation b) !store in
      v)
  | SetboxC(b, expr) -> 
      (let box = interp b frID in
      match box with 
      | BoxV loc ->
        let v = interp expr frID in
        store := updateCell loc v !store;
        v
      | _ -> failwith "Setbox: invalid args")
  | SetC(name, expr) -> 
      (let loc = lookup name frID !heap in
      let v = interp expr frID in
      store := updateCell loc v !store;
      v)
  | SeqC(left, right) -> 
      (interp left frID;
       interp right frID)
  | AppC(f, arg) ->
      (match interp f frID with
        | ClosV(x, expr, clFrID) -> 
            let v = interp arg frID in
            let (updatedHeap, newID) = initFrame !heap in
            heap := createLink newID clFrID "P" updatedHeap;
            let newLoc = newLocation !store in
            heap := updateSlotValue newID x newLoc !heap;
            store := updateCell newLoc v !store;

            (* printHeapStore !heap !store; *)

            interp expr newID
        | _ -> failwith "App: Error!")
  
  
  (* add support for multiple in app args *)

(* ------------ TESTS ----------- *)

let test input = 
    let (hp, newID) = initFrame (Heap.empty) in
    store := Store.empty;
    heap := hp;
    Printf.printf "Result: \n\t";
    printValueUnit (interp input newID);
    Printf.printf "\n";
    printHeapStore !heap !store

let test1 = test (PlusC( NumC 24, AppC( LamC( "x", NumC 5), NumC 22)))

let test2 = test (AppC( LamC( "a", AppC( 
                                    LamC( "b", 
                                        PlusC(IdC "a", IdC "b"))
                                    , NumC 4))
                    , NumC 3))
let test3 = test (AppC( LamC( "sq", MultC(NumC 5, NumC 5)), NumC 3))

let test4 = test (AppC( LamC( "sq", AppC( 
                                    LamC( "x", 
                                        MultC(IdC "x", IdC "x"))
                                    , IdC "sq"))
                    , NumC 3))

let test5 = test (AppC(
                    LamC(
                        "a", 
                        AppC(
                            LamC(
                                "b",
                                SeqC(
                                    SetC("a", NumC 1),
                                    MultC(
                                        IdC "a", 
                                        IdC "b")
                                )), 
                            NumC 4)), 
                    NumC 3))
