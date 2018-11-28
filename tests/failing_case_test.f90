module failing_case_test
    implicit none
    private

    public :: test_failing_case_behaviors
contains
    function test_failing_case_behaviors() result(test)
        use example_cases_m, only: exampleFailingTestCase, runCase
        use Vegetables_m, only: TestCase_t, TestItem_t, given, then_, when

        type(TestItem_t) :: test

        type(TestCase_t) :: example_case

        example_case = exampleFailingTestCase()
        test = given("a failing test case", example_case, &
                [when("it is run", runCase, &
                        [then_("it knows it failed", checkCaseFails), &
                        then_("it has 1 test case", checkNumCases), &
                        then_("it has no passing case", checkNumPassingCases), &
                        then_("it has 1 failing case", checkNumFailingCases), &
                        then_("it's verbose description includes the given description", checkVerboseForGivenDescription), &
                        then_("it's verbose description includes the success message", checkVerboseForSuccessMessage), &
                        then_("it's verbose description includes the failure message", checkVerboseForFailureMessage), &
                        then_("it's failure description includes the given description", checkFailureForGivenDescription), &
                        then_("it's failure description includes the failure message", checkFailureForFailureMessage), &
                        then_("it's failure description doesn't include the success message", checkFailureNoSuccessMessage), &
                        then_("it knows how many asserts there were", checkNumAsserts), &
                        then_("it knows how many asserts failed", checkNumFailingAsserts), &
                        then_("it knows how many asserts passed", checkNumPassingAsserts)])])
    end function test_failing_case_behaviors

    function checkCaseFails(example_result) result(result_)
        use Vegetables_m, only: &
                Result_t, TestCaseResult_t, assertNot, assertThat, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertThat(example_result%failed()).and.assertNot(example_result%passed())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkCaseFails

    function checkNumCases(example_result) result(result_)
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(1, example_result%numCases())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumCases

    function checkNumPassingCases(example_result) result(result_)
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(0, example_result%numPassingCases())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumPassingCases

    function checkNumFailingCases(example_result) result(result_)
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(1, example_result%numFailingCases())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumFailingCases

    function checkVerboseForGivenDescription(example_result) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(EXAMPLE_DESCRIPTION, example_result%verboseDescription())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkVerboseForGivenDescription

    function checkVerboseForSuccessMessage(example_result) result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(SUCCESS_MESSAGE, example_result%verboseDescription())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkVerboseForSuccessMessage

    function checkVerboseForFailureMessage(example_result) result(result_)
        use example_asserts_m, only: FAILURE_MESSAGE
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(FAILURE_MESSAGE, example_result%verboseDescription())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkVerboseForFailureMessage

    function checkFailureForGivenDescription(example_result) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(EXAMPLE_DESCRIPTION, example_result%failureDescription())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkFailureForGivenDescription

    function checkFailureForFailureMessage(example_result) result(result_)
        use example_asserts_m, only: FAILURE_MESSAGE
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(FAILURE_MESSAGE, example_result%failureDescription())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkFailureForFailureMessage

    function checkFailureNoSuccessMessage(example_result) result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use Vegetables_m, only: &
                Result_t, TestCaseResult_t, assertDoesntInclude, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertDoesntInclude(SUCCESS_MESSAGE, example_result%failureDescription())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkFailureNoSuccessMessage

    function checkNumAsserts(example_result) result(result_)
        use example_asserts_m, only: NUM_ASSERTS_IN_FAILING
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(NUM_ASSERTS_IN_FAILING, example_result%numAsserts())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumAsserts

    function checkNumFailingAsserts(example_result) result(result_)
        use example_asserts_m, only: NUM_FAILING_ASSERTS_IN_FAILING
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals( &
                    NUM_FAILING_ASSERTS_IN_FAILING, example_result%numFailingAsserts())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumFailingAsserts

    function checkNumPassingAsserts(example_result) result(result_)
        use example_asserts_m, only: NUM_PASSING_ASSERTS_IN_FAILING
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals( &
                    NUM_PASSING_ASSERTS_IN_FAILING, example_result%numPassingAsserts())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumPassingAsserts
end module failing_case_test
