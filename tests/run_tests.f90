program run_tests
    use Vegetables_m, only: Test_t, runTests

    implicit none

    class(Test_t), allocatable :: tests

    call runTests(tests)
end program run_tests
