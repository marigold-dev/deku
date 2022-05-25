let src = Logs.Src.create "deku"

module Log = (val Logs.src_log src : Logs.LOG)

let error f = Format.kasprintf (fun s -> Logs.err (fun e -> e "%s" s)) f

let warn f = Format.kasprintf (fun s -> Logs.warn (fun e -> e "%s" s)) f

let info f = Format.kasprintf (fun s -> Logs.info (fun e -> e "%s" s)) f
