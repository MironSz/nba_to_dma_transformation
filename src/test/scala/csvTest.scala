
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}


class csvTest extends PropSpec with TableDrivenPropertyChecks with Matchers {
	CSVtoNBA.parse("simpleNBA.csv")
}