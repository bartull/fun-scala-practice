package examples

import java.io.File

import dogs.{Async, Foldable, Monoid, Process}
import dogs.Async.Async

import scala.io.Source

object FileProcessing {

  def processFile[A](file: File, p: Process[String, A])
                    (implicit monoid: Monoid[A]): Async[A] = Async {
    val source = Source.fromFile(file)
    try {
      Foldable[Stream].reduce(p(source.getLines().toStream))
    } finally source.close()
  }
}
