set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

set(
  srcs
  "${dir}/veggies.f90"
  "${dir}/veggies/ascii_string_generator_m.f90"
  "${dir}/veggies/assert_m.f90"
  "${dir}/veggies/command_line_m.f90"
  "${dir}/veggies/constants_m.f90"
  "${dir}/veggies/double_precision_input_m.f90"
  "${dir}/veggies/example_m.f90"
  "${dir}/veggies/generated_m.f90"
  "${dir}/veggies/generator_m.f90"
  "${dir}/veggies/individual_result_m.f90"
  "${dir}/veggies/input_m.f90"
  "${dir}/veggies/input_test_case_m.f90"
  "${dir}/veggies/integer_generator_m.f90"
  "${dir}/veggies/integer_input_m.f90"
  "${dir}/veggies/messages_m.f90"
  "${dir}/veggies/random_m.f90"
  "${dir}/veggies/result_m.f90"
  "${dir}/veggies/run_tests_m.f90"
  "${dir}/veggies/shrink_result_m.f90"
  "${dir}/veggies/simple_test_case_m.f90"
  "${dir}/veggies/simple_test_collection_m.f90"
  "${dir}/veggies/string_input_m.f90"
  "${dir}/veggies/test_case_result_m.f90"
  "${dir}/veggies/test_case_with_examples_m.f90"
  "${dir}/veggies/test_case_with_generator_m.f90"
  "${dir}/veggies/test_collection_result_m.f90"
  "${dir}/veggies/test_collection_with_input_m.f90"
  "${dir}/veggies/test_constructors_m.f90"
  "${dir}/veggies/test_interfaces_m.f90"
  "${dir}/veggies/test_item_m.f90"
  "${dir}/veggies/test_m.f90"
  "${dir}/veggies/test_result_item_m.f90"
  "${dir}/veggies/test_result_m.f90"
  "${dir}/veggies/transformation_failure_m.f90"
  "${dir}/veggies/transformed_m.f90"
  "${dir}/veggies/transforming_test_collection_m.f90"
  "${dir}/veggies/assert/doesnt_include_m.f90"
  "${dir}/veggies/assert/empty_m.f90"
  "${dir}/veggies/assert/equals_double_precision_m.f90"
  "${dir}/veggies/assert/equals_integer_m.f90"
  "${dir}/veggies/assert/equals_strings_m.f90"
  "${dir}/veggies/assert/equals_within_absolute_m.f90"
  "${dir}/veggies/assert/equals_within_relative_m.f90"
  "${dir}/veggies/assert/faster_than_m.f90"
  "${dir}/veggies/assert/includes_m.f90"
  "${dir}/veggies/assert/not_m.f90"
  "${dir}/veggies/assert/that_m.f90"
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
