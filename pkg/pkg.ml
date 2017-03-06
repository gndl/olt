#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "olt" @@ fun c ->
  Ok [ Pkg.mllib "src/olt.mllib";
       Pkg.test "test/test"; ]
