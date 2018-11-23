module passing_collection_test
    implicit none
    private

    public :: test_passing_collection_behaviors
contains
    function test_passing_collection_behaviors() result(tests)
        use Vegetables_m, only: TestItem_t, given, then, when

        type(TestItem_t) :: tests

        tests = given("a passing test collection", &
                [when("it is run", &
                        [then("it knows it passed", checkCollectionPasses), &
                        then("it knows how many cases there were", checkNumCases), &
                        then("it knows how many cases passed", checkNumPassingCases), &
                        then("it has no failing cases", checkNumFailingCases)])])
    end function test_passing_collection_behaviors

    function checkCollectionPasses() result(result_)
        use example_collections_m, only: examplePassingCollection
        use Vegetables_m, only: &
                Result_t, &
                TestCollection_t, &
                TestCollectionResult_t, &
                assertNot, &
                assertThat

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = examplePassingCollection()
        test_results = test_collection%run()
        result_ = assertThat(test_results%passed()).and.assertNot(test_results%failed())
    end function checkCollectionPasses

    function checkNumCases() result(result_)
        use example_collections_m, only: &
                examplePassingCollection, NUM_CASES_IN_PASSING
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = examplePassingCollection()
        test_results = test_collection%run()
        result_ = assertEquals(NUM_CASES_IN_PASSING, test_results%numCases())
    end function checkNumCases

    function checkNumPassingCases() result(result_)
        use example_collections_m, only: &
                examplePassingCollection, NUM_CASES_IN_PASSING
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = examplePassingCollection()
        test_results = test_collection%run()
        result_ = assertEquals(NUM_CASES_IN_PASSING, test_results%numPassingCases())
    end function checkNumPassingCases

    function checkNumFailingCases() result(result_)
        use example_collections_m, only: examplePassingCollection
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = examplePassingCollection()
        test_results = test_collection%run()
        result_ = assertEquals(0, test_results%numFailingCases())
    end function checkNumFailingCases
end module passing_collection_test
