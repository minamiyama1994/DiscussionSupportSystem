# DiscussionSupportSystem
=======================

## 概要

議論を支援する専用の言語と言うかシステムみたいなものです  
そのままで議論してもよし、自然言語に変換してもよし  

## 仕様

*考え中の仕様です*、今後どうなるかわからんのであくまで参考までに  

* discussion ::= claim-discussion | opinion-discussion | consent-discussion
* claim-discussion ::= declare-claim claim-body
* declare-claim ::= "claim" discussion-id
* claim-body ::= content-claim
* content-claim ::= reasons claim
* reasons ::= reason
* reasons ::= reasons reason
* opinion-discussion ::= declare-opinion opinion-body
* declare-opinion ::= "opinion" discussion-id
* opinion-body ::= targets
* consent-discussion ::= declare-consent consent-body
* declare-consent ::= "consent" discussion-id
* consent-body ::= target
* targets ::= target
* targets ::= targets target
* target ::= "target" discussion-id