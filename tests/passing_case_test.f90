module passing_case_test
    implicit none
    private

    public :: test_passing_case_behaviors
contains
    function test_passing_case_behaviors() result(test)
        use example_cases_m, only: examplePassingTestCase, runCase
        use Vegetables_m, only: SimpleTestCase_t, TestItem_t, given, then_, when

        type(TestItem_t) :: test

        type(TestItem_t) :: collection(1)
        type(SimpleTestCase_t) :: example_case
        type(TestItem_t) :: individual_tests(8)

        example_case = examplePassingTestCase()
        individual_tests(1) = then_("it knows it passed", checkCasePasses)
        individual_tests(2) = then_("it has 1 test case", checkNumCases)
        individual_tests(3) = then_("it has no failing case", checkNumFailingCases)
        individual_tests(4) = then_("it's verbose description still includes the given description", checkVerboseDescription)
        individual_tests(5) = then_("it's verbose description includes the assertion message", checkVerboseDescriptionAssertion)
        individual_tests(6) = then_("it's failure description is empty", checkFailureDescriptionEmpty)
        individual_tests(7) = then_("it knows how many asserts there were", checkNumAsserts)
        individual_tests(8) = then_("it has no failing asserts", checkNumFailingAsserts)
        collection(1) = when("it is run", runCase, individual_tests)
        test = given("a passing test case", example_case, collection)
    end function test_passing_case_behaviors

    function checkCasePasses(example_result) result(result_)
        use Vegetables_m, only: &
                Result_t, TestCaseResult_t, assertThat, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertThat(example_result%passed(), "It passed", "It didn't pass")
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkCasePasses

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

    function checkNumFailingCases(example_result) result(result_)
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(0, example_result%numFailingCases())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumFailingCases

    function checkVerboseDescription(example_result) result(result_)
        use example_cases_m, only: EXAMPLE_DESCRIPTION
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(EXAMPLE_DESCRIPTION, example_result%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkVerboseDescription

    function checkVerboseDescriptionAssertion(example_result) result(result_)
        use example_asserts_m, only: SUCCESS_MESSAGE
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertIncludes, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertIncludes(SUCCESS_MESSAGE, example_result%verboseDescription(.false.))
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkVerboseDescriptionAssertion

    function checkFailureDescriptionEmpty(example_result) result(result_)
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEmpty, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEmpty(example_result%failureDescription(.false.))
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkFailureDescriptionEmpty

    function checkNumAsserts(example_result) result(result_)
        use example_asserts_m, only: NUM_ASSERTS_IN_PASSING
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(NUM_ASSERTS_IN_PASSING, example_result%numAsserts())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumAsserts

    function checkNumFailingAsserts(example_result) result(result_)
        use Vegetables_m, only: Result_t, TestCaseResult_t, assertEquals, fail

        class(*), intent(in) :: example_result
        type(Result_t) :: result_

        select type (example_result)
        type is (TestCaseResult_t)
            result_ = assertEquals(0, example_result%numFailingAsserts())
        class default
            result_ = fail("Expected to get a TestCaseResult_t")
        end select
    end function checkNumFailingAsserts
end module passing_case_test
