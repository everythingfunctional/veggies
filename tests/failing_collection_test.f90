module failing_collection_test
    implicit none
    private

    public :: test_failing_collection_behaviors
contains
    function test_failing_collection_behaviors() result(tests)
        use example_collections_m, only: exampleFailingCollection, runCollection
        use Vegetables_m, only: TestCollection_t, TestItem_t, given, then_, when

        type(TestItem_t) :: tests

        type(TestCollection_t) :: example_collection

        example_collection = exampleFailingCollection()
        tests = given("a failing test collection", example_collection, &
                [when("it is run", runCollection, &
                        [then_("it knows it failed", checkCollectionFails), &
                        then_("it knows how many cases there were", checkNumCases), &
                        then_("it knows how many cases passed", checkNumPassingCases), &
                        then_("it knows how many cases failed", checkNumFailingCases), &
                        then_("it's verbose description includes the given description", checkVerboseTopDescription), &
                        then_("it's verbose description includes the individual case descriptions", checkVerboseCaseDescriptions), &
                        then_("it's verbose description includes the failure message", checkVerboseForFailureMessage), &
                        then_("it's verbose description includes the success message", checkVerboseForSuccessMessage), &
                        then_("it's failure description includes the given description", checkFailureForTopDescription), &
                        then_("it's failure description includes the failing case description", checkFailureCaseDescription), &
                        then_( &
                                "it's failure description does not include the passing case descriptions", &
                                checkFailureNoPassingDescriptions), &
                        then_("it's failure description includes the failure message", checkFailureForMessage), &
                        then_( &
                                "it's failure description does not include the success message", &
                                checkFailureNoSuccessMessage), &
                        then_("it's failure description does not include blank lines", checkFailureNoBlankLines), &
                        then_("it knows how many asserts there were", checkNumAsserts), &
                        then_("it knows how many asserts failed", checkNumFailingAsserts), &
                        then_("it knows how many asserts passed", checkNumPassingAsserts)])])
    end function test_failing_collection_behaviors

    function checkCollectionFails(example_results) result(result_)
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertNot, assertThat, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertThat(example_results%failed()).and.assertNot(example_results%passed())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkCollectionFails

    function checkNumCases(example_results) result(result_)
        use example_collections_m, only: NUM_CASES_IN_FAILING
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertEquals, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertEquals(NUM_CASES_IN_FAILING, example_results%numCases())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkNumCases

    function checkNumPassingCases(example_results) result(result_)
        use example_collections_m, only: NUM_PASSING_CASES_IN_FAILING
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertEquals, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertEquals(NUM_PASSING_CASES_IN_FAILING, example_results%numPassingCases())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkNumPassingCases

    function checkNumFailingCases(example_results) result(result_)
        use example_collections_m, only: NUM_FAILING_CASES
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertEquals, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertEquals(NUM_FAILING_CASES, example_results%numFailingCases())
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
                EXAMPLE_CASE_DESCRIPTION_1, &
                EXAMPLE_CASE_DESCRIPTION_2, &
                EXAMPLE_FAILING_CASE_DESCRIPTION
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
                            example_results%verboseDescription()) &
                    .and.assertIncludes( &
                            EXAMPLE_FAILING_CASE_DESCRIPTION, &
                            example_results%verboseDescription())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkVerboseCaseDescriptions

    function checkVerboseForFailureMessage(example_results) result(result_)
        use example_collections_m, only: FAILURE_MESSAGE
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertIncludes, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertIncludes( &
                    FAILURE_MESSAGE, &
                    example_results%verboseDescription())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkVerboseForFailureMessage

    function checkVerboseForSuccessMessage(example_results) result(result_)
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
    end function checkVerboseForSuccessMessage

    function checkFailureForTopDescription(example_results) result(result_)
        use example_collections_m, only: EXAMPLE_COLLECTION_DESCRIPTION
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertIncludes, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertIncludes( &
                    EXAMPLE_COLLECTION_DESCRIPTION, &
                    example_results%failureDescription())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkFailureForTopDescription

    function checkFailureCaseDescription(example_results) result(result_)
        use example_collections_m, only: EXAMPLE_FAILING_CASE_DESCRIPTION
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertIncludes, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertIncludes( &
                    EXAMPLE_FAILING_CASE_DESCRIPTION, &
                    example_results%failureDescription())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkFailureCaseDescription

    function checkFailureNoPassingDescriptions(example_results) result(result_)
        use example_collections_m, only: &
                EXAMPLE_CASE_DESCRIPTION_1, EXAMPLE_CASE_DESCRIPTION_2
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertDoesntInclude, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = &
                    assertDoesntInclude( &
                            EXAMPLE_CASE_DESCRIPTION_1, &
                            example_results%failureDescription()) &
                    .and.assertDoesntInclude( &
                            EXAMPLE_CASE_DESCRIPTION_2, &
                            example_results%failureDescription())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkFailureNoPassingDescriptions

    function checkFailureForMessage(example_results) result(result_)
        use example_collections_m, only: FAILURE_MESSAGE
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertIncludes, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertIncludes( &
                    FAILURE_MESSAGE, &
                    example_results%failureDescription())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkFailureForMessage

    function checkFailureNoSuccessMessage(example_results) result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertDoesntInclude, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertDoesntInclude( &
                    SUCCESS_MESSAGE, &
                    example_results%failureDescription())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkFailureNoSuccessMessage

    function checkFailureNoBlankLines(example_results) result(result_)
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertDoesntInclude, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertDoesntInclude( &
                    NEW_LINE('A') // NEW_LINE('A'), &
                    example_results%failureDescription())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkFailureNoBlankLines

    function checkNumAsserts(example_results) result(result_)
        use example_collections_m, only: NUM_ASSERTS_IN_FAILING
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertEquals, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertEquals(NUM_ASSERTS_IN_FAILING, example_results%numAsserts())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkNumAsserts

    function checkNumFailingAsserts(example_results) result(result_)
        use example_collections_m, only: NUM_FAILING_ASSERTS
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertEquals, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertEquals(NUM_FAILING_ASSERTS, example_results%numFailingAsserts())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkNumFailingAsserts

    function checkNumPassingAsserts(example_results) result(result_)
        use example_collections_m, only: NUM_PASSING_ASSERTS_IN_FAILING
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertEquals, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertEquals( &
                    NUM_PASSING_ASSERTS_IN_FAILING, example_results%numPassingAsserts())
        class default
            result_ = fail("Expected to get a TestCollectionResult_t")
        end select
    end function checkNumPassingAsserts
end module failing_collection_test
