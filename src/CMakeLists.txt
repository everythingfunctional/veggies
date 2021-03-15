set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

set(
  srcs
  "${dir}/vegetables.f90"
  "${dir}/vegetables/ascii_string_generator_m.f90"
  "${dir}/vegetables/assert_m.f90"
  "${dir}/vegetables/command_line_m.f90"
  "${dir}/vegetables/constants_m.f90"
  "${dir}/vegetables/double_precision_input_m.f90"
  "${dir}/vegetables/example_m.f90"
  "${dir}/vegetables/generated_m.f90"
  "${dir}/vegetables/generator_m.f90"
  "${dir}/vegetables/individual_result_m.f90"
  "${dir}/vegetables/input_m.f90"
  "${dir}/vegetables/input_test_case_m.f90"
  "${dir}/vegetables/integer_generator_m.f90"
  "${dir}/vegetables/integer_input_m.f90"
  "${dir}/vegetables/messages_m.f90"
  "${dir}/vegetables/random_m.f90"
  "${dir}/vegetables/result_m.f90"
  "${dir}/vegetables/run_tests_m.f90"
  "${dir}/vegetables/shrink_result_m.f90"
  "${dir}/vegetables/simple_test_case_m.f90"
  "${dir}/vegetables/simple_test_collection_m.f90"
  "${dir}/vegetables/string_input_m.f90"
  "${dir}/vegetables/test_case_result_m.f90"
  "${dir}/vegetables/test_case_with_examples_m.f90"
  "${dir}/vegetables/test_case_with_generator_m.f90"
  "${dir}/vegetables/test_collection_result_m.f90"
  "${dir}/vegetables/test_collection_with_input_m.f90"
  "${dir}/vegetables/test_constructors_m.f90"
  "${dir}/vegetables/test_interfaces_m.f90"
  "${dir}/vegetables/test_item_m.f90"
  "${dir}/vegetables/test_m.f90"
  "${dir}/vegetables/test_result_item_m.f90"
  "${dir}/vegetables/test_result_m.f90"
  "${dir}/vegetables/transformation_failure_m.f90"
  "${dir}/vegetables/transformed_m.f90"
  "${dir}/vegetables/transforming_test_collection_m.f90"
  "${dir}/vegetables/assert/doesnt_include_m.f90"
  "${dir}/vegetables/assert/empty_m.f90"
  "${dir}/vegetables/assert/equals_double_precision_m.f90"
  "${dir}/vegetables/assert/equals_integer_m.f90"
  "${dir}/vegetables/assert/equals_strings_m.f90"
  "${dir}/vegetables/assert/equals_within_absolute_m.f90"
  "${dir}/vegetables/assert/equals_within_relative_m.f90"
  "${dir}/vegetables/assert/faster_than_m.f90"
  "${dir}/vegetables/assert/includes_m.f90"
  "${dir}/vegetables/assert/not_m.f90"
  "${dir}/vegetables/assert/that_m.f90"
)

set(
  MODULE_OUTPUT_DIR
  "${CMAKE_CURRENT_BINARY_DIR}/include"
)

# Create example library target
add_library(
  "${PROJECT_NAME}-lib"
  "${srcs}"
)
set_target_properties(
  "${PROJECT_NAME}-lib"
  PROPERTIES
  POSITION_INDEPENDENT_CODE TRUE
  OUTPUT_NAME "${PROJECT_NAME}"
  VERSION "${PROJECT_VERSION}"
  SOVERSION "${PROJECT_VERSION_MAJOR}"
  Fortran_MODULE_DIRECTORY "${MODULE_OUTPUT_DIR}"
)
target_link_libraries(
  "${PROJECT_NAME}-lib"
  PRIVATE
  "${lib-deps}"
)
target_include_directories(
  "${PROJECT_NAME}-lib"
  INTERFACE
  $<BUILD_INTERFACE:${MODULE_OUTPUT_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

if(INSTALL_PROJECT)
  # Export targets for other projects
  install(
    TARGETS
    "${PROJECT_NAME}-lib"
    EXPORT
    "${PROJECT_NAME}-targets"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  )
  install(
    DIRECTORY
    "${MODULE_OUTPUT_DIR}/"
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
  )
endif()
