module failing_case_test
    use example_asserts_m, only: &
            FAILURE_MESSAGE, &
            SUCCESS_MESSAGE, &
            NUM_ASSERTS_IN_FAILING, &
            NUM_FAILING_ASSERTS_IN_FAILING
    use example_cases_m, only: exampleFailingTestCase, EXAMPLE_DESCRIPTION
    use Helpers_m, only: TestItemInput_t, TestResultItemInput_t, runTest
    use Vegetables_m, only: &
            Input_t, &
            Result_t, &
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

    public :: test_failing_case_behaviors
contains
    function test_failing_case_behaviors() result(test)
        type(TestItem_t) :: test

        type(TestItem_t) :: collection(1)
        type(TestItemInput_t) :: the_case
        type(TestItem_t) :: individual_tests(11)

        the_case%input = exampleFailingTestCase()
        individual_tests(1) = Then__("it knows it failed", checkCaseFails)
        individual_tests(2) = Then__("it has 1 test case", checkNumCases)
        individual_tests(3) = Then__("it has 1 failing case", checkNumFailingCases)
        individual_tests(4) = Then__("it's verbose description includes the given description", checkVerboseForGivenDescription)
        individual_tests(5) = Then__("it's verbose description includes the success message", checkVerboseForSuccessMessage)
        individual_tests(6) = Then__("it's verbose description includes the failure message", checkVerboseForFailureMessage)
        individual_tests(7) = Then__("it's failure description includes the given description", checkFailureForGivenDescription)
        individual_tests(8) = Then__("it's failure description includes the failure message", checkFailureForFailureMessage)
        individual_tests(9) = Then__("it's failure description doesn't include the success message", checkFailureNoSuccessMessage)
        individual_tests(10) = Then__("it knows how many asserts there were", checkNumAsserts)
        individual_tests(11) = Then__("it knows how many asserts failed", checkNumFailingAsserts)
        collection(1) = When("it is run", runTest, individual_tests)
        test = Given("a failing test case", the_case, collection)
    end function test_failing_case_behaviors

    pure function checkCaseFails(example_result) result(result_)
        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertNot(example_result%input%passed())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkCaseFails

    pure function checkNumCases(example_result) result(result_)
        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertEquals(1, example_result%input%numCases())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumCases

    pure function checkNumFailingCases(example_result) result(result_)
        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertEquals(1, example_result%input%numFailingCases())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumFailingCases

    pure function checkVerboseForGivenDescription(example_result) result(result_)
        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertIncludes(EXAMPLE_DESCRIPTION, example_result%input%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkVerboseForGivenDescription

    pure function checkVerboseForSuccessMessage(example_result) result(result_)
        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertIncludes(SUCCESS_MESSAGE, example_result%input%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkVerboseForSuccessMessage

    pure function checkVerboseForFailureMessage(example_result) result(result_)
        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertIncludes(FAILURE_MESSAGE, example_result%input%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkVerboseForFailureMessage

    pure function checkFailureForGivenDescription(example_result) result(result_)
        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertIncludes(EXAMPLE_DESCRIPTION, example_result%input%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkFailureForGivenDescription

    pure function checkFailureForFailureMessage(example_result) result(result_)
        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertIncludes(FAILURE_MESSAGE, example_result%input%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkFailureForFailureMessage

    pure function checkFailureNoSuccessMessage(example_result) result(result_)
        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertDoesntInclude(SUCCESS_MESSAGE, example_result%input%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkFailureNoSuccessMessage

    pure function checkNumAsserts(example_result) result(result_)
        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertEquals(NUM_ASSERTS_IN_FAILING, example_result%input%numAsserts())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumAsserts

    pure function checkNumFailingAsserts(example_result) result(result_)
        class(Input_t), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestResultItemInput_t)
            result_ = assertEquals( &
                    NUM_FAILING_ASSERTS_IN_FAILING, example_result%input%numFailingAsserts())
        class default
            result_ = fail("Expected to get a TestResultItemInput_t")
        end select
    end function checkNumFailingAsserts
end module failing_case_test
