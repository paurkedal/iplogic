" Vim syntax file
" Language:	iplogic
" Maintainer:	Petter A. Urkedal

syn match iplError /\S/

" Operators
syn keyword iplOperator or in
syn match iplOperator '[:=!&|,+\-*/~\\]'
syn match iplOperator '[.,][.,]\+'
syn region iplParen matchgroup=iplOperator transparent fold start='(' end=')'
syn region iplParen matchgroup=iplOperator transparent fold start='\[' end='\]'
syn region iplParen matchgroup=iplOperator transparent fold start='{' end='}'

" Options, Labels, Numbers, Booleans
syn match iplOption '-[a-zA-Z0-9-]\+'
syn match iplNumber '\<\d\+\>'
syn match iplName '\<\I\i*\(:\d\+\)\?\>'
syn keyword iplConstant true false

" Addresses
syn match iplHostname '\<\([a-zA-Z0-9]\+\(-\+[a-zA-Z0-9]\)*\.\)\+[a-zA-Z]\{2,\}\>'
syn match iplAddress '\(\<\x\+\(:\x\+\)*\)\?::\(\x\+\(:\x\+\)*\>\)\?'
  \ contains=iplAddressSeparator nextgroup=iplAddressPrefixLength
syn match iplAddress '::\<\(ffff\|FFFF\):\d\+\(\.\d\+\)\{3\}\>'
  \ contains=iplAddressSeparator nextgroup=iplAddressPrefixLength
syn match iplAddress '\<\d\+\(\.\d\+\)\{3\}\>'
  \ contains=iplAddressSeparator nextgroup=iplAddressPrefixLength
syn match iplAddress '\<\x\+\(:\x\+\)\{7\}\>'
  \ contains=iplAddressSeparator nextgroup=iplAddressPrefixLength
syn match iplAddressSeparator contained '[:.]'
syn match iplAddressPrefixLength contained '/\d\+\>'

" Type Names
syn keyword iplType string ints nets device protocol

" Directives and Flow Control
syn keyword iplInclude include
syn keyword iplDirective val con autochain
syn match iplDirective '\<chain\>!\?' skipwhite nextgroup=iplChainName
syn keyword iplFinal is
syn keyword iplFinal alter accept drop reject fail return
syn match iplFinal '\<goto\>!\?' skipwhite nextgroup=iplChainName
syn keyword iplAction log bug
syn keyword iplAction call skipwhite nextgroup=iplChainName
syn keyword iplConditional if case default
syn match iplChainName contained '\(\I\i*\.\)\?\I\i*' contains=iplTableName
syn match iplTableName contained '\I\i*\.'

" Strings and Comments
syn region iplString matchgroup=iplStringEscape start='"' skip='\\.' end='"'
  \ contains=iplStringEscape,iplStringInterp
syn match iplStringEscape contained '\\.'
syn region iplStringInterp matchgroup=iplStringEscape start='$(' end=')'
  \ contains=TOP
syn match iplComment '#.*'

" Highligthing Defaults
hi def link iplDirective Statement
hi def link iplInclude iplDirective
hi def link iplConditional Conditional
hi def link iplFinal Statement
hi def link iplAction Function
"hi def link iplTableName Macro
"hi def link iplChainName Macro
hi def link iplOperator Operator
hi def link iplComment Comment
hi def link iplType Type
hi def link iplOption Special
hi def link iplNumber Number
hi def link iplString String
hi def link iplStringEscape Special
hi def link iplConstant Constant
hi def link iplHostname Constant
hi def link iplAddress Constant
hi def link iplAddressSeparator Constant
hi def link iplAddressPrefixLength Special
hi def link iplError Error
