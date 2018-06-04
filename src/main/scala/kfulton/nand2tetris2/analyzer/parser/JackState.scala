package kfulton.nand2tetris2.analyzer.parser

import kfulton.nand2tetris2.analyzer.tokenizer.tokens.Token

class JackState {
  type Tokens = List[Token]
  type ParseOrError[T] = Either[String, T]
  //type JackState[T] = StateT[ParseOrError[T], Tokens, T]

}
