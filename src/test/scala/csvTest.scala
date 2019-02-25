import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class csvTest extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val automat: NBA[AlphabetLetter] = CSVtoNBA.parse(
    "C:\\Users\\GTX\\IdeaProjects\\nba_to_dma_transformation\\src\\main\\scala\\exampleAutomatonsCsv\\simpleNBA.csv")
  print(
    automat
      .readLetter(AlphabetLetter('a'))
      .readLetter(AlphabetLetter('a'))
      .readLetter(AlphabetLetter('a'))
      .readLetter(AlphabetLetter('a'))
      .acceptanceRates)

}
