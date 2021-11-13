(* File extension for ReasonLIGO source files *)

module Ext =
  struct
    let extension = ".religo"
  end

include Ext

module type S = module type of Ext
