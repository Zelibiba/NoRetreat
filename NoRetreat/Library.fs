namespace NoRetreat

open System.Collections.Generic

module Library =

    let inline dispatchwithIndex (dispatch: 'msg -> unit) wrapMsg index (msg: 'innerMsg) =
        (index, msg) |> wrapMsg |> dispatch

    let memoize f =
      let cache = Dictionary()
      fun x ->
        match cache.TryGetValue(x) with
        | true, v -> v
        | _ -> 
          let v = f x
          cache.Add(x, v)
          v

    //let inline updateElementIn<'state, 'idx, 'item, 'msg
    //    when 'state: (member get_Item: 'idx -> 'item) and 'state: (member set_Item: 'idx * 'item -> unit)>
    //    (state: 'state)
    //    fUpdate
    //    (msg : 'msg)
    //    index
    //    =
    //    index
    //    |> state.get_Item
    //    |> fUpdate msg
    //    |> (fun item' -> (index, item'))
    //    |> state.set_Item

module Tuple =
    let inline map f1 f2 (item1, item2) = (f1 item1, f2 item2)