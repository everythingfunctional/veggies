module failing_collection_test
    implicit none
    private

    public :: test_failing_collection_behaviors
contains
    function test_failing_collection_behaviors() result(tests)
        use Vegetables_m, only: TestCollection_t, given, then, when

        type(TestCollection_t) :: tests

        tests = given("a failing test collection", &
                [when("it is run", &
                [then("it knows it failed", checkCollectionFails)])])
    end function test_failing_collection_behaviors

    function checkCollectionFails() result(result_)
        use example_collections_m, only: exampleFailingCollection
        use Vegetables_m, only: &
                Result_t, &
                TestCollection_t, &
                TestCollectionResult_t, &
                operator(.and.), &
                assertNot, &
                assertThat

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertThat(test_results%failed()).and.assertNot(test_results%passed())
    end function checkCollectionFails
end module failing_collection_test
