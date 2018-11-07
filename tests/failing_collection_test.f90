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
                [then("it knows it failed", checkCollectionFails), &
                then("it knows how many cases there were", checkNumCases), &
                then("it knows how many cases passed", checkNumPassingCases), &
                then("it knows how many cases failed", checkNumFailingCases)])])
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

    function checkNumCases() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, NUM_CASES_IN_FAILING
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertEquals(NUM_CASES_IN_FAILING, test_results%numCases())
    end function checkNumCases

    function checkNumPassingCases() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, NUM_PASSING_CASES_IN_FAILING
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertEquals(NUM_PASSING_CASES_IN_FAILING, test_results%numPassingCases())
    end function checkNumPassingCases

    function checkNumFailingCases() result(result_)
        use example_collections_m, only: &
                exampleFailingCollection, NUM_FAILING_CASES
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = exampleFailingCollection()
        test_results = test_collection%run()
        result_ = assertEquals(NUM_FAILING_CASES, test_results%numFailingCases())
    end function checkNumFailingCases
end module failing_collection_test
