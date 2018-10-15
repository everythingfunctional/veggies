program test_runner
    use test_case_test, only: test_case_testGenerator => testGenerator
    use test_collection_test, only: test_collection_testGenerator => testGenerator
    use Vegetables_m,  only: TestCollection_t, executeEverything

    implicit none

    integer, parameter :: NUM_TEST_COLLECTIONS = 2
    type(TestCollection_t) :: test_collections(NUM_TEST_COLLECTIONS)

    test_collections(1) = test_collection_testGenerator()
    test_collections(2) = test_case_testGenerator()
    call executeEverything(test_collections)

end program test_runner
