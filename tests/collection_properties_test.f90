module collection_properties_test
    implicit none
    private

    public :: test_collection_properties
contains
    function test_collection_properties() result(test)
        use example_collections_m, only: examplePassingCollection
        use Vegetables_m, only: TestCollection_t, TestItem_t, describe, it_

        type(TestItem_t) :: test

        type(TestCollection_t) :: example_collection

        example_collection = examplePassingCollection()
        test = describe("A test collection", example_collection, &
                [it_("can tell how many tests it has", checkNumCases), &
                it_("includes the given description", checkCollectionTopDescription), &
                it_("includes the individual test descriptions", checkCollectionDescriptions)])
    end function test_collection_properties

    function checkNumCases(example_collection) result(result_)
        use example_collections_m, only: NUM_CASES_IN_PASSING
        use Vegetables_m, only: Result_t, TestCollection_t, assertEquals, fail

        class(*), intent(in) :: example_collection
        type(Result_t) :: result_

        select type (example_collection)
        type is (TestCollection_t)
            result_ = assertEquals(NUM_CASES_IN_PASSING, example_collection%numCases())
        class default
            result_ = fail("Expected to get a TestCollection_t")
        end select
    end function checkNumCases

    function checkCollectionTopDescription(example_collection) result(result_)
        use example_collections_m, only: EXAMPLE_COLLECTION_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCollection_t, assertIncludes, fail

        class(*), intent(in) :: example_collection
        type(Result_t) :: result_

        select type (example_collection)
        type is (TestCollection_t)
            result_ = assertIncludes( &
                    EXAMPLE_COLLECTION_DESCRIPTION, example_collection%description())
        class default
            result_ = fail("Expected to get a TestCollection_t")
        end select
    end function checkCollectionTopDescription

    function checkCollectionDescriptions(example_collection) result(result_)
        use example_collections_m, only: &
                EXAMPLE_CASE_DESCRIPTION_1, EXAMPLE_CASE_DESCRIPTION_2
        use Vegetables_m, only: &
                Result_t, TestCollection_t, assertIncludes, fail

        class(*), intent(in) :: example_collection
        type(Result_t) :: result_

        select type (example_collection)
        type is (TestCollection_t)
            result_ = &
                    assertIncludes( &
                            EXAMPLE_CASE_DESCRIPTION_1, example_collection%description()) &
                    .and.assertIncludes( &
                            EXAMPLE_CASE_DESCRIPTION_2, example_collection%description())
        class default
            result_ = fail("Expected to get a TestCollection_t")
        end select
    end function checkCollectionDescriptions
end module collection_properties_test
