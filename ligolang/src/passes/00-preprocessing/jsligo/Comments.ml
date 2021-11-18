(* Comments for JsLIGO *)

module C =
  struct
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    let block =
      object
        method opening = "/*"
        method closing = "*/"
      end

    let block = Some block
    let line  = Some "//"
  end

include C

module type S = module type of C
