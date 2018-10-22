module test_collection_test
    implicit none
    private

    character(len=*), parameter :: EXAMPLE_DESCRIPTION1 = "Example Description 1"
    character(len=*), parameter :: EXAMPLE_DESCRIPTION2 = "Example Description 2"
    character(len=*), parameter :: EXAMPLE_DESCRIPTION3 = "Example Description 3"

    public :: test_collection_can_tell_failure, test_collection_properties
contains
    pure function test_collection_properties() result(test)
        use Vegetables_m, only: TestCollection_t, Describe, It

        type(TestCollection_t) :: test

        test = Describe("A test collection", &
                [It("can tell how many tests it has", checkNumCases)])
    end function test_collection_properties

    pure function test_collection_can_tell_failure() result(test)
        use Vegetables_m, only: TestCollection_t, Given, When, Then

        type(TestCollection_t) :: test

        test = Given("A test collection with a failing test", &
                [When("It is run", &
                        [Then("It can tell it failed", checkfailedCollection)])])
    end function test_collection_can_tell_failure

    pure function checkfailedCollection() result(test_result)
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestResult_t, assertNot

        type(Result_t) :: test_result

        type(TestCollection_t) :: failing_collection
        class(TestResult_t), allocatable :: failed_collection

        failing_collection = failingCollection()
        failed_collection = failing_collection%run()

        test_result = assertNot(failed_collection%passed())
    end function checkfailedCollection

    pure function failingCollection() result(collection)
        use Vegetables_m, only: TestCollection_t, Describe, FAILING

        type(TestCollection_t) :: collection

        collection = Describe("Failing", [FAILING()])
    end function failingCollection

    pure function checkNumCases() result(result_)
        use Vegetables_m, only: Result_t, TestCollection_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection

        test_collection = exampleTestCollection()
        result_ = assertEquals(2, test_collection%numCases())
    end function checkNumCases

    pure function exampleTestCase1() result(test_case)
        use Vegetables_m, only: TestCase_t, It, succeed

        type(TestCase_t) :: test_case

        test_case = It(EXAMPLE_DESCRIPTION1, succeed)
    end function exampleTestCase1

    pure function exampleTestCase2() result(test_case)
        use Vegetables_m, only: TestCase_t, It, succeed

        type(TestCase_t) :: test_case

        test_case = It(EXAMPLE_DESCRIPTION2, succeed)
    end function exampleTestCase2

    pure function exampleTestCollection() result(test_collection)
        use Vegetables_m, only: TestCollection_t, Describe

        type(TestCollection_t) :: test_collection

        test_collection = Describe(EXAMPLE_DESCRIPTION2, &
                [exampleTestCase1(), exampleTestCase2()])
    end function exampleTestCollection
end module test_collection_test
