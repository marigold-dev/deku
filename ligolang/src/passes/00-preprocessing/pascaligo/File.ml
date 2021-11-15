(* File extension for PascaLIGO source files *)

module Ext =
  struct
    let extension = ".ligo"
  end

include Ext

module type S = module type of Ext
