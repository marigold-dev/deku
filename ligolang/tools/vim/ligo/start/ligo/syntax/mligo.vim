if exists("b:current_syntax")
    finish
endif

" string
syntax region string start="\"" end="\"" 
highlight link string String 

" comment
syntax match comment "\/\/.*$" 
syntax region comment start="(\*" end="\*)" 
highlight link comment Comment 

" identifierconstructor
syntax match identifierconstructor "\<\([A-Z][a-zA-Z0-9_$]*\)\s\+" 
highlight link identifierconstructor Label 

" module
syntax match module_ "[a-z_][a-zA-Z0-9_$]*" contained 
highlight link module_ Identifier 
syntax match module "\<\([A-Z][a-zA-Z0-9_$]*\)\." nextgroup=module_ 
highlight link module Structure 

" typedefinition
syntax match typedefinition "\(type\)\>" 
highlight link typedefinition Type 

" lambda
syntax region lambda matchgroup=lambda_ start="\(fun\)\W" matchgroup=lambda__ end="\(->\)" 
highlight link lambda_ Statement 
highlight link lambda__ Operator 

" operators
syntax match operators "\<\(::\|-\|+\|/\|mod\|land\|lor\|lxor\|lsl\|lsr\|&&\|||\|<\|>\|<>\|<=\|>=\)\>" 
highlight link operators Operator 

" numericliterals
syntax match numericliterals "\<[0-9]+\(n\|tz\|tez\|mutez\|\)\>" 
highlight link numericliterals Number 

" letbinding
syntax match letbinding__ "[a-zA-Z$_][a-zA-Z0-9$_]*" contained 
highlight link letbinding__ Statement 
syntax match letbinding_ "rec\W\|" contained nextgroup=letbinding__ 
highlight link letbinding_ StorageClass 
syntax match letbinding "\(let\)\W" nextgroup=letbinding_ 
highlight link letbinding Keyword 

" controlkeywords
syntax match controlkeywords "\<\(match\|with\|if\|then\|else\|assert\|failwith\|begin\|end\|in\)\>" 
highlight link controlkeywords Conditional 

" macro
syntax match macro "^\#[a-zA-Z]\+" 
highlight link macro PreProc 

" attribute
syntax match attribute "\[@.*\]" 
highlight link attribute PreProc 

let b:current_syntax = "mligo"