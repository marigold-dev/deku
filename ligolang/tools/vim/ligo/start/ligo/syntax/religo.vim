if exists("b:current_syntax")
    finish
endif

" Keywords
syntax keyword ligoKeyword begin end
syntax keyword ligoKeyword is
syntax keyword ligoKeyword fun let type
syntax keyword ligoConditional if then else switch with
syntax keyword ligoRepeat for to in set while
syntax match ligoOperator "\v[-+*/=]"
syntax match ligoOperator "=>"
syntax match ligoOperator "\.\.\."
syntax match ligoOtherSymbol "[(),;]"

highlight link ligoKeyword Keyword
highlight link ligoConditional Conditional
highlight link ligoRepeat Repeat
highlight link ligoOperator Operator

" Constants
syntax keyword ligoBoolean True False true false
syntax match ligoNumber "\v<\d+[a-z]*>"
syntax match ligoNumber "\v<0x[a-fA-F0-9]+>"
syntax region ligoString start=/\v"/ skip=/\v\\./ end=/\v"/

highlight link ligoBoolean Boolean
highlight link ligoNumber Number
highlight link ligoString String

" Comments
syntax region ligoComment start="\v/\*" end="\v\*/" contains=ligoComment
syntax match ligoComment "\v//.*$"
highlight link ligoComment Comment

" Types
syntax region ligoParenTypeExpr start=/\v\(/ end=/\v\)/
            \ contains=ligoParenTypeExpr,ligoComment
            \ contained

" In religo, we require type annotations to start with nonword
" symbol followed by a colon instead of just colon. This simplifies
" the highlighting logic – otherwise, to discriminate `{ field : type }`
" and `{ field: value}` we would have to describe the whole grammar
" in Vim syntax, and lay out the nesting rules. It is possible in theory
" but since LIGO is under active development, the maintenance burden
" would overweigh the advantages. Since space-colon is a recommended
" formatting, the highlighting behavior is predictable.
syntax region ligoTypeAnnotation 
            \ matchgroup=ligoKeyword start=/\v(\W)@<=:/
            \ matchgroup=ligoKeyword end=/=/
            \ matchgroup=ligoKeyword end=/=>/
            \ matchgroup=ligoOtherSymbol end=/,/
            \ matchgroup=ligoOtherSymbol end=/;/
            \ matchgroup=ligoOtherSymbol end=/)/
            \ contains=ligoParenTypeExpr,ligoComment
highlight link ligoTypeAnnotation Type
highlight link ligoParenTypeExpr Type

" Macros
syntax match p_include "#\s*include"
syntax match p_define  "#\s*\(define\|undef\)"
syntax match p_if      "#\s*\(if\|elif\|else\|endif\)\(n\?def\)\?"
syntax match p_message "#\s*\(error\|warning\)"

highlight link p_include Include
highlight link p_define  Define
highlight link p_if      PreCondit
highlight link p_message Macro

let b:current_syntax = "religo"

