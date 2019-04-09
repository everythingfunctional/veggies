module failing_collection_test
    implicit none
    private

    public :: test_failing_collection_behaviors
contains
    function test_failing_collection_behaviors() result(tests)
        use example_collections_m, only: exampleFailingCollection, runCollection
        use Vegetables_m, only: TestCollection_t, TestItem_t, given, then_, when

        type(TestItem_t) :: tests

        type(TestItem_t) :: collection(1)
        type(TestCollection_t) :: example_collection
        type(TestItem_t) :: individual_tests(15)

        example_collection = exampleFailingCollection()
        individual_tests(1) = then_("it knows it failed", checkCollectionFails)
        individual_tests(2) = then_("it knows how many cases there were", checkNumCases)
        individual_tests(3) = then_("it knows how many cases failed", checkNumFailingCases)
        individual_tests(4) = then_("it's verbose description includes the given description", checkVerboseTopDescription)
        individual_tests(5) = then_( &
                "it's verbose description includes the individual case descriptions", &
                checkVerboseCaseDescriptions)
        individual_tests(6) = then_("it's verbose description includes the failure message", checkVerboseForFailureMessage)
        individual_tests(7) = then_("it's verbose description includes the success message", checkVerboseForSuccessMessage)
        individual_tests(8) = then_("it's failure description includes the given description", checkFailureForTopDescription)
        individual_tests(9) = then_("it's failure description includes the failing case description", checkFailureCaseDescription)
        individual_tests(10) = then_( &
                "it's failure description does not include the passing case descriptions", &
                checkFailureNoPassingDescriptions)
        individual_tests(11) = then_("it's failure description includes the failure message", checkFailureForMessage)
        individual_tests(12) = then_( &
                "it's failure description does not include the success message", &
                checkFailureNoSuccessMessage)
        individual_tests(13) = then_("it's failure description does not include blank lines", checkFailureNoBlankLines)
        individual_tests(14) = then_("it knows how many asserts there were", checkNumAsserts)
        individual_tests(15) = then_("it knows how many asserts failed", checkNumFailingAsserts)
        collection(1) = when("it is run", runCollection, individual_tests)
        tests = given("a failing test collection", example_collection, collection)
    end function test_failing_collection_behaviors

    function checkCollectionFails(example_results) result(result_)
        use Vegetables_m, only: &
                Result_t, TestCollectionResult_t, assertNot, fail

        class(*), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestCollectionResult_t)
            result_ = assertNot(example_results%passed(), "It didn't pass", "It passed")
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
end module failing_collection_test
