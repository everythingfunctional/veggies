module assert_doesnt_include_test
    implicit none
    private

    public :: test_assert_includes
contains
    function test_assert_includes() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it("passes with different strings", checkPassForDifferentStrings)
        individual_tests(2) = it("fails with the same string", checkFailForSameString)
        tests = describe("assertDoesntInclude", individual_tests)
    end function test_assert_includes

    function checkPassForDifferentStrings() result(result_)
        use Vegetables_m, only: Result_t, assertDoesntInclude, assertThat

        type(Result_t) :: result_

        character(len=*), parameter :: ONE_STRNIG = "One String"
        character(len=*), parameter :: OTHER_STRING = "Other String"
        type(Result_t) :: example_result

        example_result = assertDoesntInclude(ONE_STRNIG, OTHER_STRING)

        result_ = assertThat( &
                example_result%passed(), example_result%verboseDescription())
    end function checkPassForDifferentStrings

    function checkFailForSameString() result(result_)
        use Vegetables_m, only: Result_t, assertDoesntInclude, assertNot

        type(Result_t) :: result_

        character(len=*), parameter :: EXAMPLE_STRING = "Example String"
        type(Result_t) :: example_result

        example_result = assertDoesntInclude(EXAMPLE_STRING, EXAMPLE_STRING)


        result_ = assertNot( &
                example_result%passed(), example_result%verboseDescription())
    end function checkFailForSameString
end module assert_doesnt_include_test
