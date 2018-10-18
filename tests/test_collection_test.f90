module test_collection_test
    implicit none
    private

    public :: test_collection_can_tell_failure
contains
    pure function test_collection_can_tell_failure() result(test)
        use Vegetables_m, only: Test_t, Given, When, Then

        class(Test_t), allocatable :: test

        test = Given("A test collection with a failing test", &
                [When("It is run", &
                        [Then("It can tell it failed", checkfailedCollection)])])
    end function test_collection_can_tell_failure

    pure function checkfailedCollection() result(test_result)
        use Vegetables_m, only: Result_t, fail

        type(Result_t) :: test_result

        test_result = fail("TODO")
    end function checkfailedCollection
end module test_collection_test
