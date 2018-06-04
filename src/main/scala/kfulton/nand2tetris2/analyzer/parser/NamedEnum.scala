package kfulton.nand2tetris2.analyzer.parser

trait CanonicalName {
  val canonical: String
}

trait NamedEnum[T <: CanonicalName] {
  def values: Vector[T]

  def forName(name: String): Option[T] =
    values.find(_.canonical.equalsIgnoreCase(name.trim))
}