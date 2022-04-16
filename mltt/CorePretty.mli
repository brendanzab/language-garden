module Syntax = CoreSyntax

val term : ?indent:int -> Syntax.term -> 'a Pp.t
