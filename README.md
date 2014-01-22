# DiscussionSupportSystem
=======================

## 概要

議論を支援する専用の言語と言うかシステムみたいなものです  
そのままで議論してもよし、自然言語に変換してもよし  

## 仕様

*考え中の仕様です*。仕様というか文法です今後どうなるかわからんのであくまで参考までに  

* `discussion ::= basiss claim`
* `basiss ::= basis`
* `basiss ::= basiss basis`
* `basis ::= url`
* `basis ::= book`
* `basis ::= quote_discussion`
* `url ::= "url" URL`
* `book ::= "ISBN" ISBN pages`
* `pages ::= "pages" "(" ")"`
* `pages ::= "pages" "(" page_numbers ")"`
* `page_numbers ::= page_number`
* `page_numbers ::= page_numbers "," page_number`
* `page_number ::= DIGIT`
* `page_number ::= page_number DIGIT`
* `page_number ::= page_number DIGIT`
* `quote_discussion ::= "text" STRING`
* `claim ::= "claim" STRING`