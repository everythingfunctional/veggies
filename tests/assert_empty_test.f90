module assert_empty_test
    implicit none
    private

    public :: test_assert_empty
contains
    function test_assert_empty() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        tests = describe("assertEmpty", &
                [it("passes with an empty character", checkPassForEmptyChars), &
                it("passes with an empty string", checkPassForEmptyString), &
                it("fails with a non empty character", checkFailsForNonemptyChars), &
                it("fails with a non empty string", checkFailsForNonemptyString)])
    end function test_assert_empty

    function checkPassForEmptyChars() result(result_)
        use Vegetables_m, only: Result_t, assertEmpty, assertThat

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEmpty("")

        result_ = assertThat(example_result%passed())
    end function checkPassForEmptyChars

    function checkPassForEmptyString() result(result_)
        use Vegetables_m, only: Result_t, assertEmpty, assertThat, toString

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEmpty(toString(""))

        result_ = assertThat(example_result%passed())
    end function checkPassForEmptyString

    function checkFailsForNonemptyChars() result(result_)
        use Vegetables_m, only: Result_t, assertEmpty, assertNot

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEmpty("Not Empty")

        result_ = assertNot(example_result%passed())
    end function checkFailsForNonemptyChars

    function checkFailsForNonemptyString() result(result_)
        use Vegetables_m, only: Result_t, assertEmpty, assertNot, toString

        type(Result_t) :: result_

        type(Result_t) :: example_result

        example_result = assertEmpty(toString("Not Empty"))

        result_ = assertNot(example_result%passed())
    end function checkFailsForNonemptyString
end module assert_empty_test
