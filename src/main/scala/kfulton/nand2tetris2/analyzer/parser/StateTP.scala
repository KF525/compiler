package kfulton.nand2tetris2.analyzer.parser

case class StateTP[S, A](run: S => (A, S)) {
  def map[B](f: A => B): StateTP[S, B] =
    StateTP(s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })

  def flatMap[B](f: A => StateTP[S, B]): StateTP[S, B] =
    StateTP(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

  def eval(s: S): A = run(s)._1
}


object StateTP {
  def get[S, A](f: S => A): StateTP[S, A] = StateTP(s => (f(s), s))

  def mod[S](f: S => S): StateTP[S, Unit] = StateTP(s => ((), f(s)))
}