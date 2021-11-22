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

" constorvar
syntax match constorvar "\<\(const\|var\)\>" 
highlight link constorvar Keyword 

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

" operators
syntax match operators "\<\(-\|+\|/\|mod\|land\|lor\|lxor\|lsl\|lsr\|&&\|||\|<\|>\|=/=\|<=\|>=\)\>" 
highlight link operators Operator 

" numericliterals
syntax match numericliterals "\<[0-9]+\(n\|tz\|tez\|mutez\|\)\>" 
highlight link numericliterals Number 

" function
syntax match function_ "[a-zA-Z$_][a-zA-Z0-9$_]*" contained 
highlight link function_ Statement 
syntax match function "\(function\)\W" nextgroup=function_ 
highlight link function Keyword 

" controlkeywords
syntax match controlkeywords "\<\(case\|with\|if\|then\|else\|assert\|failwith\|begin\|end\|in\|is\|from\|skip\|block\|contains\|to\|step\|of\|while\|for\)\>" 
highlight link controlkeywords Conditional 

" macro
syntax match macro "^\#[a-zA-Z]\+" 
highlight link macro PreProc 

" attribute
syntax match attribute "\[@.*\]" 
highlight link attribute PreProc 

let b:current_syntax = "ligo"