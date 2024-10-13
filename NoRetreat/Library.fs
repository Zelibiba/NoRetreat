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

module Tuple =
    let mapBoth f (a, b) = (f a, f b)