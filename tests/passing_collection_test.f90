module passing_collection_test
    implicit none
    private

    public :: test_passing_collection_behaviors
contains
    function test_passing_collection_behaviors() result(tests)
        use example_collections_m, only: examplePassingCollection, runCollection
        use Vegetables_m, only: TestCollection_t, TestItem_t, given, then_, when

        type(TestItem_t) :: tests

        type(TestCollection_t) :: example_collection

        example_collection = examplePassingCollection()
        tests = given("a passing test collection", example_collection, &
                [when("it is run", runCollection, &
                        [then_("it knows it passed", checkCollectionPasses), &
                        then_("it knows how many cases there were", checkNumCases), &
                        then_("it has no failing cases", checkNumFailingCases), &
                        then_("it's verbose description includes the given description", checkVerboseTopDescription), &
                        then_("it's verbose description includes the individual case descriptions", checkVerboseCaseDescriptions), &
                        then_("it's verbose description includes the assertion message", checkVerboseDescriptionAssertion), &
                        then_("it's failure description is empty", checkFailureDescriptionEmpty), &
                        then_("it knows how many asserts there were", checkNumAsserts), &
                        then_("it has no failing asserts", checkNumFailingAsserts)])])
    end function test_passing_collection_behaviors

    function checkCollectionPasses(example_results) result(result_)
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertThat, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertThat(example_results%passed(), "It passed", "It didn't pass")
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkCollectionPasses

    function checkNumCases(example_results) result(result_)
        use example_collections_m, only: NUM_CASES_IN_PASSING
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertEquals, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertEquals(NUM_CASES_IN_PASSING, example_results%numCases())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkNumCases

    function checkNumFailingCases(example_results) result(result_)
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertEquals, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertEquals(0, example_results%numFailingCases())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkNumFailingCases

    function checkVerboseTopDescription(example_results) result(result_)
        use example_collections_m, only: EXAMPLE_COLLECTION_DESCRIPTION
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertIncludes, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertIncludes( &
                    EXAMPLE_COLLECTION_DESCRIPTION, &
                    example_results%verboseDescription())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkVerboseTopDescription

    function checkVerboseCaseDescriptions(example_results) result(result_)
        use example_collections_m, only: &
                EXAMPLE_CASE_DESCRIPTION_1, EXAMPLE_CASE_DESCRIPTION_2
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertIncludes, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = &
                    assertIncludes( &
                            EXAMPLE_CASE_DESCRIPTION_1, &
                            example_results%verboseDescription()) &
                    .and.assertIncludes( &
                            EXAMPLE_CASE_DESCRIPTION_2, &
                            example_results%verboseDescription())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkVerboseCaseDescriptions

    function checkVerboseDescriptionAssertion(example_results) result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertIncludes, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertIncludes( &
                    SUCCESS_MESSAGE, &
                    example_results%verboseDescription())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkVerboseDescriptionAssertion

    function checkFailureDescriptionEmpty(example_results) result(result_)
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertEmpty, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertEmpty(example_results%failureDescription())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkFailureDescriptionEmpty

    function checkNumAsserts(example_results) result(result_)
        use example_collections_m, only: NUM_ASSERTS_IN_PASSING
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertEquals, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertEquals(NUM_ASSERTS_IN_PASSING, example_results%numAsserts())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkNumAsserts

    function checkNumFailingAsserts(example_results) result(result_)
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertEquals, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertEquals(0, example_results%numFailingAsserts())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkNumFailingAsserts
end module passing_collection_test
