module passing_collection_test
    implicit none
    private

    public :: test_passing_collection_behaviors
contains
    function test_passing_collection_behaviors() result(tests)
        use Vegetables_m, only: TestCollection_t, given, then, when

        type(TestCollection_t) :: tests

        tests = given("a passing test collection", &
                [when("it is run", &
                        [then("it knows it passed", checkCollectionPasses), &
                        then("it knows how many cases there were", checkNumCases)])])
    end function test_passing_collection_behaviors

    function checkCollectionPasses() result(result_)
        use example_collections_m, only: examplePassingCollection
        use Vegetables_m, only: &
                Result_t, TestCollection_t, TestCollectionResult_t, assertThat

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection
        type(TestCollectionResult_t) :: test_results

        test_collection = examplePassingCollection()
        test_results = test_collection%run()
        result_ = assertThat(test_results%passed())
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
end module passing_collection_test
