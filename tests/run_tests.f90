program run_tests
    use Vegetables_m, only: TestItem_t, TestResultItem_t, describe, it, testThat

    implicit none

    type(TestResultItem_t) :: first_results
    type(TestItem_t) :: first_test

    first_test= testThat(&
            [describe("collection", &
                    [it("succeeds", test_function), &
                    it("succeeds again", test_function)]), &
            describe("another collection", &
                    [it("succeeds", test_function), &
                    it("succeeds again", test_function)])])
    first_results = first_test%run()
contains
    function test_function() result(result_)
        use Vegetables_m, only: Result_t, succeed

        type(Result_t) :: result_

        result_ = succeed()
    end function test_function
end program run_tests
