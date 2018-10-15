module test_collection_test
    implicit none
    private

    public :: testGenerator
contains
    pure function testGenerator() result(test_collection)
        use Vegetables_m, only: TestCollection_t, describe, it

        type(TestCollection_t) :: test_collection

        test_collection = describe("TestCollection_t", &
                [it("knows how many test cases it has", suiteKnowsNumCases)])
    end function testGenerator

    pure function suiteKnowsNumCases() result(test_result)
        use Vegetables_m, only: TestResult_t, TestCollection_t, assertEquals

        type(TestResult_t) :: test_result

        type(TestCollection_t) :: test_collection

        test_collection = exampleGenerator()
        test_result = assertEquals(1, test_collection%numCases())
    end function suiteKnowsNumCases

    pure function exampleGenerator() result(test_collection)
        use Vegetables_m, only: TestCollection_t, describe, it

        type(TestCollection_t) :: test_collection

        test_collection = describe("examples", &
                [it("Can compare integers", compareIntegers)])
    end function exampleGenerator

    pure function compareIntegers() result(test_result)
        use Vegetables_m, only: TestResult_t, assertEquals

        type(TestResult_t) :: test_result

        test_result = assertEquals(1, 1)
    end function compareIntegers
end module test_collection_test
