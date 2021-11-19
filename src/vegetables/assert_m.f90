module vegetables_assert_m
    use vegetables_assert_doesnt_include_m, only: assert_doesnt_include
    use vegetables_assert_empty_m, only: assert_empty
    use vegetables_assert_equals_double_precision_m, only: assert_equals
    use vegetables_assert_equals_integer_m, only: assert_equals
    use vegetables_assert_equals_strings_m, only: assert_equals
    use vegetables_assert_equals_within_absolute_m, only: &
            assert_equals_within_absolute
    use vegetables_assert_equals_within_absolute_array_m, only: &
            assert_equals_within_absolute
    use vegetables_assert_equals_within_relative_m, only: &
            assert_equals_within_relative
    use vegetables_assert_faster_than_m, only: assert_faster_than
    use vegetables_assert_includes_m, only: assert_includes
    use vegetables_assert_not_m, only: assert_not
    use vegetables_assert_that_m, only: assert_that
end module
