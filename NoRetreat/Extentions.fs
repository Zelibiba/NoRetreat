namespace NoRetreat

module Extentions =
    open System
    open System.IO
    open Avalonia.Media.Imaging
    open Avalonia.Platform

    type Stream with
        static member create(str: string) =
            Uri(str, UriKind.RelativeOrAbsolute)
            |> AssetLoader.Open

    type Bitmap with
        static member create(s: string) : Bitmap =
            new Bitmap(Stream.create s)

    
