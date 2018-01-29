package observatory

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CapstoneSuite
  extends ModelsTestTrait
    with ExtractionTestTrait
    with VisualizationTestTrait
    with InteractionTestTrait
    with ManipulationTest
    with Visualization2Test
    with Interaction2Test

