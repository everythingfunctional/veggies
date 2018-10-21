module test_collection_test
    implicit none
    private

    public :: test_collection_can_tell_failure
contains
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
end module test_collection_test
