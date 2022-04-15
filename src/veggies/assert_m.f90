module veggies_assert_m
    use veggies_assert_doesnt_include_m, only: assert_doesnt_include
    use veggies_assert_empty_m, only: assert_empty
    use veggies_assert_equals_double_precision_m, only: assert_equals
    use veggies_assert_equals_double_precision_array_m, only: assert_equals
    use veggies_assert_equals_double_precision_matrix_m, only: assert_equals
    use veggies_assert_equals_double_precision_tensor_m, only: assert_equals
    use veggies_assert_equals_integer_m, only: assert_equals
    use veggies_assert_equals_integer_array_m, only: assert_equals
    use veggies_assert_equals_integer_matrix_m, only: assert_equals
    use veggies_assert_equals_integer_tensor_m, only: assert_equals
    use veggies_assert_equals_strings_m, only: assert_equals
    use veggies_assert_equals_within_absolute_m, only: &
            assert_equals_within_absolute
    use veggies_assert_equals_within_absolute_array_m, only: &
            assert_equals_within_absolute
    use veggies_assert_equals_within_absolute_matrix_m, only: &
            assert_equals_within_absolute
    use veggies_assert_equals_within_absolute_tensor_m, only: &
            assert_equals_within_absolute
    use veggies_assert_equals_within_relative_m, only: &
            assert_equals_within_relative
    use veggies_assert_equals_within_relative_array_m, only: &
            assert_equals_within_relative
    use veggies_assert_equals_within_relative_matrix_m, only: &
            assert_equals_within_relative
    use veggies_assert_equals_within_relative_tensor_m, only: &
            assert_equals_within_relative
    use veggies_assert_faster_than_m, only: assert_faster_than
    use veggies_assert_includes_m, only: assert_includes
    use veggies_assert_not_m, only: assert_not
    use veggies_assert_that_m, only: assert_that
end module
