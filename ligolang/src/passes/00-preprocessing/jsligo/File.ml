(* File extension for JsLIGO source files *)

module Ext =
  struct
    let extension = ".jsligo"
  end

include Ext

module type S = module type of Ext
