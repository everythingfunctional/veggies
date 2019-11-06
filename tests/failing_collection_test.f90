module failing_collection_test
    use example_asserts_m, only: SUCCESS_MESSAGE
    use example_collections_m, only: &
            exampleFailingCollection, &
            EXAMPLE_CASE_DESCRIPTION_1, &
            EXAMPLE_CASE_DESCRIPTION_2, &
            EXAMPLE_COLLECTION_DESCRIPTION, &
            EXAMPLE_FAILING_CASE_DESCRIPTION, &
            FAILURE_MESSAGE, &
            NUM_ASSERTS_IN_FAILING, &
            NUM_CASES_IN_FAILING, &
            NUM_FAILING_ASSERTS, &
            NUM_FAILING_CASES
    use Helpers_m, only: TestItemInput_t, TestResultItemInput_t, runTest
    use strff, only: NEWLINE
    use Vegetables_m, only: &
            Input_t, &
            Result_T, &
            TestItem_t, &
            assertDoesntInclude, &
            assertEquals, &
            assertIncludes, &
            assertNot, &
            fail, &
            Given, &
            Then__, &
            When

    implicit none
    private

    public :: test_failing_collection_behaviors
contains
    function test_failing_collection_behaviors() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: collection(1)
        type(TestItemInput_t) :: the_collection
        type(TestItem_t) :: individual_tests(15)

        the_collection%input = exampleFailingCollection()
        individual_tests(1) = Then__("it knows it failed", checkCollectionFails)
        individual_tests(2) = Then__("it knows how many cases there were", checkNumCases)
        individual_tests(3) = Then__("it knows how many cases failed", checkNumFailingCases)
        individual_tests(4) = Then__("it's verbose description includes the given description", checkVerboseTopDescription)
        individual_tests(5) = Then__( &
                "it's verbose description includes the individual case descriptions", &
                checkVerboseCaseDescriptions)
        individual_tests(6) = Then__("it's verbose description includes the failure message", checkVerboseForFailureMessage)
        individual_tests(7) = Then__("it's verbose description includes the success message", checkVerboseForSuccessMessage)
        individual_tests(8) = Then__("it's failure description includes the given description", checkFailureForTopDescription)
        individual_tests(9) = Then__("it's failure description includes the failing case description", checkFailureCaseDescription)
        individual_tests(10) = Then__( &
                "it's failure description does not include the passing case descriptions", &
                checkFailureNoPassingDescriptions)
        individual_tests(11) = Then__("it's failure description includes the failure message", checkFailureForMessage)
        individual_tests(12) = Then__( &
                "it's failure description does not include the success message", &
                checkFailureNoSuccessMessage)
        individual_tests(13) = Then__("it's failure description does not include blank lines", checkFailureNoBlankLines)
        individual_tests(14) = Then__("it knows how many asserts there were", checkNumAsserts)
        individual_tests(15) = Then__("it knows how many asserts failed", checkNumFailingAsserts)
        collection(1) = When("it is run", runTest, individual_tests)
        tests = Given("a failing test collection", the_collection, collection)
    end function test_failing_collection_behaviors

    pure function checkCollectionFails(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertNot(example_results%input%passed())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkCollectionFails

    pure function checkNumCases(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertEquals(NUM_CASES_IN_FAILING, example_results%input%numCases())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumCases

    pure function checkNumFailingCases(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertEquals(NUM_FAILING_CASES, example_results%input%numFailingCases())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumFailingCases

    pure function checkVerboseTopDescription(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertIncludes( &
                    EXAMPLE_COLLECTION_DESCRIPTION, &
                    example_results%input%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkVerboseTopDescription

    pure function checkVerboseCaseDescriptions(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = &
                    assertIncludes( &
                            EXAMPLE_CASE_DESCRIPTION_1, &
                            example_results%input%verboseDescription(.false.)) &
                    .and.assertIncludes( &
                            EXAMPLE_CASE_DESCRIPTION_2, &
                            example_results%input%verboseDescription(.false.)) &
                    .and.assertIncludes( &
                            EXAMPLE_FAILING_CASE_DESCRIPTION, &
                            example_results%input%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkVerboseCaseDescriptions

    pure function checkVerboseForFailureMessage(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertIncludes( &
                    FAILURE_MESSAGE, &
                    example_results%input%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkVerboseForFailureMessage

    pure function checkVerboseForSuccessMessage(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertIncludes( &
                    SUCCESS_MESSAGE, &
                    example_results%input%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkVerboseForSuccessMessage

    pure function checkFailureForTopDescription(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertIncludes( &
                    EXAMPLE_COLLECTION_DESCRIPTION, &
                    example_results%input%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkFailureForTopDescription

    pure function checkFailureCaseDescription(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertIncludes( &
                    EXAMPLE_FAILING_CASE_DESCRIPTION, &
                    example_results%input%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkFailureCaseDescription

    pure function checkFailureNoPassingDescriptions(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = &
                    assertDoesntInclude( &
                            EXAMPLE_CASE_DESCRIPTION_1, &
                            example_results%input%failureDescription(.false.)) &
                    .and.assertDoesntInclude( &
                            EXAMPLE_CASE_DESCRIPTION_2, &
                            example_results%input%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkFailureNoPassingDescriptions

    pure function checkFailureForMessage(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertIncludes( &
                    FAILURE_MESSAGE, &
                    example_results%input%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkFailureForMessage

    pure function checkFailureNoSuccessMessage(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertDoesntInclude( &
                    SUCCESS_MESSAGE, &
                    example_results%input%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkFailureNoSuccessMessage

    pure function checkFailureNoBlankLines(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertDoesntInclude( &
                    NEWLINE // NEWLINE, &
                    example_results%input%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkFailureNoBlankLines

    pure function checkNumAsserts(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertEquals(NUM_ASSERTS_IN_FAILING, example_results%input%numAsserts())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumAsserts

    pure function checkNumFailingAsserts(example_results) result(result_)
        class(Input_t), intent(in) :: example_results
        type(Result_t) :: result_

        select type (example_results)
        type is (TestResultItemInput_t)
            result_ = assertEquals(NUM_FAILING_ASSERTS, example_results%input%numFailingAsserts())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumFailingAsserts
end module failing_collection_test
