module collection_properties_test
    implicit none
    private

    public :: test_collection_properties
contains
    function test_collection_properties() result(test)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: test

        test = describe("A test collection", &
                [it("can tell how many tests it has", checkNumCases), &
                it("includes the given description", checkCollectionTopDescription), &
                it("includes the individual test descriptions", checkCollectionDescriptions)])
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

    function checkCollectionTopDescription() result(result_)
        use example_collections_m, only: &
                examplePassingCollection, EXAMPLE_COLLECTION_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCollection_t, assertIncludes

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection

        test_collection = examplePassingCollection()
        result_ = assertIncludes( &
                EXAMPLE_COLLECTION_DESCRIPTION, test_collection%description())
    end function checkCollectionTopDescription

    function checkCollectionDescriptions() result(result_)
        use example_collections_m, only: &
                examplePassingCollection, &
                EXAMPLE_CASE_DESCRIPTION_1, &
                EXAMPLE_CASE_DESCRIPTION_2
        use Vegetables_m, only: &
                Result_t, TestCollection_t, assertIncludes

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection

        test_collection = examplePassingCollection()
        result_ = &
                assertIncludes( &
                        EXAMPLE_CASE_DESCRIPTION_1, test_collection%description()) &
                .and.assertIncludes( &
                        EXAMPLE_CASE_DESCRIPTION_2, test_collection%description())
    end function checkCollectionDescriptions
end module collection_properties_test
