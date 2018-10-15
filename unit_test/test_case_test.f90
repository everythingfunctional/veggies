module test_case_test
    implicit none
    private

    character(len=*), parameter :: EXAMPLE_DESCRIPTION = "Example Case Description"

    public :: testGenerator
contains
    pure function testGenerator() result(test_collection)
        use Vegetables_m, only: TestCollection_t, describe, it

        type(TestCollection_t) :: test_collection

        test_collection = describe("TestCase_t", &
                [it("Can describe itself", describesItself)])
    end function testGenerator

    pure function describesItself() result(test_result)
        use Vegetables_m, only: TestCase_t, TestResult_t, assertEquals

        type(TestResult_t) :: test_result

        type(TestCase_t) :: example_case

        example_case = exampleTestCase()
        test_result = assertEquals(EXAMPLE_DESCRIPTION, example_case%getDescription())
    end function describesItself

    pure function exampleTestCase() result(test_case)
        use Vegetables_m, only: TestCase_t, it

        type(TestCase_t) :: test_case

        test_case = it(EXAMPLE_DESCRIPTION, exampleTest)
    end function exampleTestCase

    pure function exampleTest() result(test_result)
        use Vegetables_m, only: TestResult_t, assertEquals

        type(TestResult_t) :: test_result

        test_result = assertEquals(1, 1)
    end function exampleTest
end module test_case_test
