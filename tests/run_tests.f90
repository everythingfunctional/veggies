program run_tests
    use single_case_properties_test, only: test_case_properties
    use Vegetables_m, only: TestItem_t, testThat, runTests

    implicit none

    type(TestItem_t) :: tests

    tests = testThat([test_case_properties()])
    call runTests(tests)
end program run_tests
