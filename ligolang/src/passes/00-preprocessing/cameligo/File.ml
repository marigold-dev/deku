(* File extension for CameLIGO source files *)

module Ext =
  struct
    let extension = ".mligo"
  end

include Ext

module type S = module type of Ext
