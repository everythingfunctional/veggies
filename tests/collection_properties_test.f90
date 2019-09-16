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
        type(TestItem_t) :: individual_tests(4)

        example_collection = examplePassingCollection()
        individual_tests(1) = it_("can tell how many tests it has", checkNumCases)
        individual_tests(2) = it_("includes the given description", checkCollectionTopDescription)
        individual_tests(3) = it_("includes the individual test descriptions", checkCollectionDescriptions)
        individual_tests(4) = it_("doesn't take much more time than the individual cases", checkSpeed)
        test = describe("A test collection", example_collection, individual_tests)
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

    function checkSpeed(example_collection) result(result_)
        use Vegetables_m, only: &
                Result_t, TestCollection_t, assertFasterThan, fail

        class(*), intent(in) :: example_collection
        type(Result_t) :: result_

        type(TestCollection_t) :: internal_collection

        select type (example_collection)
        type is (TestCollection_t)
            internal_collection = example_collection
            result_ = assertFasterThan(1.0d-5, runCollection, 100)
        class default
            result_ = fail("Expected to get a TestCollection_t")
        end select
    contains
        subroutine runCollection
            use Vegetables_m, only: TestCollectionResult_t

            type(TestCollectionResult_t) :: internal_result

            internal_result = internal_collection%run()
        end subroutine
    end function checkSpeed
end module collection_properties_test
