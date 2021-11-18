(* open Database_plugins
 * open Ast_typed.Types
 * module AllPlugins = PluginFields(functor (Plugin : Plugin) -> struct type t = int Plugin.t end) *)

include Database_plugins.All_plugins
