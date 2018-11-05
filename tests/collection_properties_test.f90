module collection_properties_test
    implicit none
    private

    public :: test_collection_properties
contains
    function test_collection_properties() result(test)
        use Vegetables_m, only: TestCollection_t, describe, it

        type(TestCollection_t) :: test

        test = describe("A test collection", &
                [it("can tell how many tests it has", checkNumCases), &
                it("includes the given description", checkCollectionTopDescription), &
                it("includes the individual test descriptions", checkCollectionDescriptions)])
    end function test_collection_properties

    function checkNumCases() result(result_)
        use example_collections_m, only: exampleTestCollection, NUM_CASES
        use Vegetables_m, only: Result_t, TestCollection_t, assertEquals

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection

        test_collection = exampleTestCollection()
        result_ = assertEquals(NUM_CASES, test_collection%numCases())
    end function checkNumCases

    function checkCollectionTopDescription() result(result_)
        use example_collections_m, only: &
                exampleTestCollection, EXAMPLE_COLLECTION_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCollection_t, assertIncludes

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection

        test_collection = exampleTestCollection()
        result_ = assertIncludes( &
                EXAMPLE_COLLECTION_DESCRIPTION, test_collection%description())
    end function checkCollectionTopDescription

    function checkCollectionDescriptions() result(result_)
        use example_collections_m, only: &
                exampleTestCollection, &
                EXAMPLE_CASE_DESCRIPTION_1, &
                EXAMPLE_CASE_DESCRIPTION_2
        use Vegetables_m, only: &
                Result_t, TestCollection_t, assertIncludes, operator(.and.)

        type(Result_t) :: result_

        type(TestCollection_t) :: test_collection

        test_collection = exampleTestCollection()
        result_ = &
                assertIncludes( &
                        EXAMPLE_CASE_DESCRIPTION_1, test_collection%description()) &
                .and.assertIncludes( &
                        EXAMPLE_CASE_DESCRIPTION_2, test_collection%description())
    end function checkCollectionDescriptions
end module collection_properties_test
