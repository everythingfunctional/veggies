module collection_properties_test
    implicit none
    private

    public :: test_collection_properties
contains
    function test_collection_properties() result(test)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: test

        test = describe("A test collection", &
                [it("can tell how many tests it has", checkNumCases)])
    end function test_collection_properties

    function checkNumCases() result(result_)
        use example_collections_m, only: &
                examplePassingCollection, NUM_CASES_IN_PASSING
        use Vegetables_m, only: Result_t, TestCollection_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection

        test_collection = examplePassingCollection()
        result_ = assertEquals(NUM_CASES_IN_PASSING, test_collection%numCases())
    end function checkNumCases
end module collection_properties_test
