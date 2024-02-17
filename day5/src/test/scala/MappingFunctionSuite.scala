class MappingFunctionSuite extends munit.FunSuite:
  test("Simple Merge Test"):
    val initialMapping = ClosedMapping(10, 30, 5)

    val nextLevelMapping1 = ClosedMapping(5, 20, 2)

    val merge1 = nextLevelMapping1.mergeFrom(initialMapping)

    val functionFromMerge1 = MappingFunction("test", merge1)

    assert(functionFromMerge1.smallestFrom(10l, 10l) == 17)
    assert(functionFromMerge1.smallestFrom(16l, 19l) == 21)
    assert(functionFromMerge1.smallestFrom(4l, 4l) == 4)
    assert(functionFromMerge1.smallestFrom(6l, 7l) == 8)
    assert(functionFromMerge1.smallestFrom(4l, 50l) == 4)
    assert(merge1.length == 3)

  test("Simple Merge with two next level mappings Test"):
    val initialMapping = ClosedMapping(10, 30, 5)

    val nextLevelMapping1 = ClosedMapping(5, 20, 2)
    val nextLevelMapping2 = ClosedMapping(21, 30, 10)

    val initialMappingFunction = MappingFunction("initial", List(initialMapping))
    val nextLevelMappingFunction = MappingFunction("next", List(nextLevelMapping1))



    val merge1 = initialMappingFunction.digest(nextLevelMappingFunction)
    println(merge1)

    assert(merge1.smallestFrom(10l, 10l) == 17)

