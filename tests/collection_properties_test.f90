module collection_properties_test
    use example_collections_m, only: &
            examplePassingCollection, &
            exampleTestCase1, &
            exampleTestCase2, &
            EXAMPLE_CASE_DESCRIPTION_1, &
            EXAMPLE_CASE_DESCRIPTION_2, &
            EXAMPLE_COLLECTION_DESCRIPTION, &
            NUM_CASES_IN_PASSING
    use Helpers_m, only: TestItemInput_t
    use Vegetables_m, only: &
            Input_t, &
            Result_t, &
            TestItem_t, &
            TestResultItem_t, &
            assertEquals, &
            assertFasterThan, &
            assertIncludes, &
            describe, &
            fail, &
            it_

    implicit none
    private

    public :: test_collection_properties
contains
    function test_collection_properties() result(test)
        type(TestItem_t) :: test

        type(TestItemInput_t) :: the_collection
        type(TestItem_t) :: individual_tests(4)

        the_collection%input = examplePassingCollection()
        individual_tests(1) = it_("can tell how many tests it has", checkNumCases)
        individual_tests(2) = it_("includes the given description", checkCollectionTopDescription)
        individual_tests(3) = it_("includes the individual test descriptions", checkCollectionDescriptions)
        individual_tests(4) = it_("Takes less than three times as long as the individual cases", checkSpeed)
        test = describe("A test collection", the_collection, individual_tests)
    end function test_collection_properties

    pure function checkNumCases(example_collection) result(result_)
        class(Input_t), intent(in) :: example_collection
        type(Result_t) :: result_

        select type (example_collection)
        class is (TestItemInput_t)
            result_ = assertEquals(NUM_CASES_IN_PASSING, example_collection%input%numCases())
        class default
            result_ = fail("Expected to get a TestCollection_t")
        end select
    end function checkNumCases

    pure function checkCollectionTopDescription(example_collection) result(result_)
        class(Input_t), intent(in) :: example_collection
        type(Result_t) :: result_

        select type (example_collection)
        class is (TestItemInput_t)
            result_ = assertIncludes( &
                    EXAMPLE_COLLECTION_DESCRIPTION, example_collection%input%description())
        class default
            result_ = fail("Expected to get a TestCollection_t")
        end select
    end function checkCollectionTopDescription

    pure function checkCollectionDescriptions(example_collection) result(result_)
        class(Input_t), intent(in) :: example_collection
        type(Result_t) :: result_

        select type (example_collection)
        class is (TestItemInput_t)
            result_ = &
                    assertIncludes( &
                            EXAMPLE_CASE_DESCRIPTION_1, example_collection%input%description()) &
                    .and.assertIncludes( &
                            EXAMPLE_CASE_DESCRIPTION_2, example_collection%input%description())
        class default
            result_ = fail("Expected to get a TestCollection_t")
        end select
    end function checkCollectionDescriptions

    function checkSpeed(example_collection) result(result_)
        class(Input_t), intent(in) :: example_collection
        type(Result_t) :: result_

        type(TestItem_t) :: internal_collection

        type(TestItem_t) :: the_cases(3)

        the_cases(1) = exampleTestCase1()
        the_cases(2) = exampleTestCase2()
        the_cases(3) = exampleTestCase2()

        select type (example_collection)
        type is (TestItemInput_t)
            internal_collection = example_collection%input
            result_ = assertFasterThan(runCases, runCollection, 100)
        class default
            result_ = fail("Expected to get a TestCollection_t")
        end select
    contains
        subroutine runCollection
            integer :: i
            type(TestResultItem_t) :: internal_result

            do i = 1, 100
                internal_result = internal_collection%run()
            end do
        end subroutine runCollection

        subroutine runCases
            integer :: i
            type(TestResultItem_t) :: the_results(3)

            do i = 1, 300
                the_results(1) = the_cases(1)%run()
                the_results(2) = the_cases(2)%run()
                the_results(3) = the_cases(3)%run()
            end do
        end subroutine runCases
    end function checkSpeed
end module collection_properties_test
