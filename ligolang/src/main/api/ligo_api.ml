open Api_helpers



let dump_changelog display_format =
    let value = Changelog.changelog in
    let format = Formatter.changelog_format in
    format_result ~display_format format (fun _ -> []) (fun ~raise:_ -> value)




module Compile = Compile
module Transpile = Transpile
module Run = Run
module Info = Info
module Print = Print
module Mutate = Mutate
