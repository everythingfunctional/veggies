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
                        [then("it knows it passed", checkCollectionPasses)])])
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
end module passing_collection_test
