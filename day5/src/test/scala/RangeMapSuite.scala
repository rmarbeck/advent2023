import munit.IgnoreSuite

@IgnoreSuite
class RangeMapSuite extends munit.FunSuite:
  test("Simple Range Map Test"):
    val range1 = RangeMap.from(destinationOfStart = 50, start = 98, length = 2)
    val range2 = RangeMap.from(destinationOfStart = 52, start = 50,  length = 48)

    val layer = SingleLayerRangeSet("firstTest", List(range1, range2))

    assertEquals(layer.solve(10l), 10l)
    assertEquals(layer.solve(98l), 50l)
    assertEquals(layer.solve(99l), 51l)

    assertEquals(layer.solveRange(10l, 99l), 10l)


  test("More complex Range Map Test"):
    val range1 = RangeMap.from(destinationOfStart = 49, start = 53, length = 8)
    val range2 = RangeMap.from(destinationOfStart = 0, start = 11, length = 42)
    val range3 = RangeMap.from(destinationOfStart = 42, start = 0, length = 7)
    val range4 = RangeMap.from(destinationOfStart = 57, start = 7, length = 4)

    val layer = SingleLayerRangeSet("secondTest", List(range1, range2, range3, range4))

    assertEquals(layer.solve(-12l), -12l)
    assertEquals(layer.solve(2l), 44l)
    assertEquals(layer.solve(53l), 49l)
    assertEquals(layer.solveRange(55l, 60l), 51l)
    assertEquals(layer.solveRange(51l, 60l), 40l)
    assertEquals(layer.solveRange(62l, 80l), 62l)


  test("Merging Test"):
    val rangeTop1 = RangeMap.from(destinationOfStart = 50, start = 98, length = 2)
    val rangeTop2 = RangeMap.from(destinationOfStart = 52, start = 50, length = 48)

    val topLayer = SingleLayerRangeSet("firstTest", List(rangeTop1, rangeTop2))

    val rangeNextLevel1 = RangeMap.from(destinationOfStart = 49, start = 53, length = 8)
    val rangeNextLevel2 = RangeMap.from(destinationOfStart = 0, start = 11, length = 42)
    val rangeNextLevel3 = RangeMap.from(destinationOfStart = 42, start = 0, length = 7)
    val rangeNextLevel4 = RangeMap.from(destinationOfStart = 57, start = 7, length = 4)

    val nextLevelLayer = SingleLayerRangeSet("secondTest", List(rangeNextLevel1, rangeNextLevel2, rangeNextLevel3, rangeNextLevel4))

    val resultingLayer = MultiLayerSolver(List(topLayer, nextLevelLayer))

    assertEquals(resultingLayer.solve(0l), 42l)
    assertEquals(resultingLayer.solve(10l), 60l)
    assertEquals(resultingLayer.solve(51l), 49l)
    assertEquals(resultingLayer.solve(81l), 83l)
    assertEquals(resultingLayer.solve(99l), 40l)

    assertEquals(resultingLayer.solve(102l), 102l)

  test("Merging Test 1 and 2"):
    val rangeTop1 = RangeMap.from(destinationOfStart = 50, start = 98, length = 2)
    val rangeTop2 = RangeMap.from(destinationOfStart = 52, start = 50, length = 48)

    val topLayer = SingleLayerRangeSet("firstTest", List(rangeTop1, rangeTop2))

    val rangeNextLevel1 = RangeMap.from(destinationOfStart = 0, start = 15, length = 37)
    val rangeNextLevel2 = RangeMap.from(destinationOfStart = 37, start = 52, length = 2)
    val rangeNextLevel3 = RangeMap.from(destinationOfStart = 39, start = 0, length = 15)

    val nextLevelLayer = SingleLayerRangeSet("secondTest", List(rangeNextLevel1, rangeNextLevel2, rangeNextLevel3))

    val resultingLayer = MultiLayerSolver(List(topLayer, nextLevelLayer))

    assertEquals(resultingLayer.solve(79), 81l)
    assertEquals(resultingLayer.solve(14), 53l)
    assertEquals(resultingLayer.solve(55), 57l)
    assertEquals(resultingLayer.solve(13), 52l)


  test("Merging Test 1 to 3"):
    val rangeTop1 = RangeMap.from(destinationOfStart = 50, start = 98, length = 2)
    val rangeTop2 = RangeMap.from(destinationOfStart = 52, start = 50, length = 48)

    val topLayer = SingleLayerRangeSet("initial", List(rangeTop1, rangeTop2))

    val rangeLevel21 = RangeMap.from(destinationOfStart = 0, start = 15, length = 37)
    val rangeLevel22 = RangeMap.from(destinationOfStart = 37, start = 52, length = 2)
    val rangeLevel23 = RangeMap.from(destinationOfStart = 39, start = 0, length = 15)

    val level2 = SingleLayerRangeSet("level2", List(rangeLevel21, rangeLevel22, rangeLevel23))


    val rangeLevel31 = RangeMap.from(destinationOfStart = 49, start = 53, length = 8)
    val rangeLevel32 = RangeMap.from(destinationOfStart = 0, start = 11, length = 42)
    val rangeLevel33 = RangeMap.from(destinationOfStart = 42, start = 0, length = 7)
    val rangeLevel34 = RangeMap.from(destinationOfStart = 57, start = 7, length = 4)

    val level3 = SingleLayerRangeSet("level3", List(rangeLevel31, rangeLevel32, rangeLevel33, rangeLevel34))

    val resultingLayer = MultiLayerSolver(List(topLayer, level2, level3))

    assertEquals(resultingLayer.solve(79), 81l)
    assertEquals(resultingLayer.solve(14), 49l)
    assertEquals(resultingLayer.solve(55), 53l)
    assertEquals(resultingLayer.solve(13), 41l)

@IgnoreSuite
class RangeMapCriticalSuite extends munit.FunSuite:
  test("Critical test"):

    val topLayer = SingleLayerRangeSet("seed-to-soil-soil-to-fertilizer-fertilizer-to-water-water-to-light",List(RangeMap(0,13,21), RangeMap(14,14,28), RangeMap(15,21,20), RangeMap(22,25,28), RangeMap(26,43,0), RangeMap(44,49,44), RangeMap(50,51,-31), RangeMap(52,58,-9), RangeMap(59,92,-5), RangeMap(93,97,2), RangeMap(98,98,-4), RangeMap(99,99,-81)))

    val rangeLevel21 = RangeMap.from(destinationOfStart = 45, start = 77, length = 23)
    val rangeLevel22 = RangeMap.from(destinationOfStart = 81, start = 45, length = 19)
    val rangeLevel23 = RangeMap.from(destinationOfStart = 68, start = 64, length = 13)

    val level2 = SingleLayerRangeSet("level2", List(rangeLevel21, rangeLevel22, rangeLevel23))

    val resultingLayer = MultiLayerSolver(List(topLayer, level2))

    assertEquals(resultingLayer.solve(79), 78l)
    assertEquals(resultingLayer.solve(14), 42l)
    assertEquals(resultingLayer.solve(55), 82l)
    assertEquals(resultingLayer.solve(13), 34l)

  test("Critical test2"):

    val topLayer = SingleLayerRangeSet("seed-to-soil-soil-to-fertilizer-fertilizer-to-water-water-to-light-light-to-temperature-temperature-to-humidity",List(RangeMap(0,13,22), RangeMap(14,14,29), RangeMap(15,21,21), RangeMap(22,25,64), RangeMap(44,49,13), RangeMap(50,49,-30), RangeMap(50,51,-30), RangeMap(54,58,27), RangeMap(59,68,31), RangeMap(69,69,0), RangeMap(70,70,-70), RangeMap(71,81,-1), RangeMap(82,92,-36), RangeMap(93,97,-29), RangeMap(98,98,-35), RangeMap(99,99,-80)))

    val rangeLevel21 = RangeMap.from(destinationOfStart = 60, start = 56, length = 37)
    val rangeLevel22 = RangeMap.from(destinationOfStart = 56, start = 93, length = 4)

    val level2 = SingleLayerRangeSet("level2", List(rangeLevel21, rangeLevel22))

    val resultingLayer = MultiLayerSolver(List(topLayer, level2))

    assertEquals(resultingLayer.solve(79), 82l)
    assertEquals(resultingLayer.solve(14), 43l)
    assertEquals(resultingLayer.solve(55), 86l)
    assertEquals(resultingLayer.solve(13), 35l)
    assertEquals(resultingLayer.solve(82), 46l)