namespace NoRetreat

module Extentions =
    open System
    open System.IO
    open Avalonia.Media.Imaging
    open Avalonia.Platform

    type Bitmap with
        static member Create(s: string) : Bitmap =
            let uri =
                if s.StartsWith("/") then
                    Uri(s, UriKind.Relative)
                else
                    Uri(s, UriKind.RelativeOrAbsolute)

            if uri.IsAbsoluteUri && uri.IsFile then
                new Bitmap(uri.LocalPath)
            else
                new Bitmap(AssetLoader.Open(uri))

    type Stream with
        static member create(str: string) =
            Uri(str, UriKind.RelativeOrAbsolute)
            |> AssetLoader.Open
