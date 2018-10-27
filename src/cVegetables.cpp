/*
MODULE cVegetables
*/

#include <cstring>
#include <iostream>
#include <vector>

class Result {
private:
  bool passed;

public:
  Result(bool passed);
};

class TestResult {
protected:
  char *_description;

public:
  TestResult(char *description);
};

class TestCaseResult : public TestResult {
private:
public:
  TestCaseResult(char *_description);
};

class TestCollectionResult : public TestResult {
private:
public:
  TestCollectionResult(char *_description);
};

class Test {
protected:
  char *_description;

public:
  Test(char *description);
  virtual TestResult *run() = 0;
};

class TestCase : public Test {
private:
  void *_test;

public:
  TestCase(char *description, void *test);
  char *description();
  TestCaseResult *run();
};

class TestCollection : public Test {
private:
  std::vector<Test *> _tests;

public:
  TestCollection(char *description, std::vector<Test *> tests);
  TestCollectionResult *run();
};

Result::Result(bool passed) : passed(passed) {}

extern "C" Result *cResult(bool passed) {
  Result *result = new Result(passed);
  return result;
}

Test::Test(char *description) : _description(description) {}

TestCase::TestCase(char *description, void *test)
    : Test(description), _test(test) {}

char *TestCase::description() { return this->_description; }

TestCaseResult *TestCase::run() {
  TestCaseResult *result = new TestCaseResult(this->_description);
  return result;
}

extern "C" TestCase *cTestCase(char *description, void *test) {
  TestCase *test_case = new TestCase(description, test);
  std::cout << "Test case at " << test_case << "\n";
  return test_case;
}

extern "C" void cTestCaseDescription(TestCase *test_case, char *description,
                                     int maxlen) {
  strncpy(description, test_case->description(), maxlen);
}

TestCollection::TestCollection(char *description, std::vector<Test *> tests)
    : Test(description), _tests(tests) {}

TestCollectionResult *TestCollection::run() {
  TestCollectionResult *results = new TestCollectionResult(this->_description);
  return results;
}

extern "C" TestCollection *cTestCollection(char *description, Test **tests,
                                           int num_tests) {
  std::cout << "Given tests at " << tests << "\n";
  std::vector<Test *> tests_;
  for (int i = 0; i < num_tests; i++) {
    std::cout << "Adding test at " << tests[i] << "\n";
    tests_.push_back(tests[i]);
  }
  TestCollection *test_collection = new TestCollection(description, tests_);
  std::cout << "Test collection at " << test_collection << "\n";
  return test_collection;
}

extern "C" TestCollectionResult *cRunTestCollection(TestCollection *tests) {
  return tests->run();
}

TestResult::TestResult(char *description) : _description(description) {}

TestCaseResult::TestCaseResult(char *description) : TestResult(description) {}

TestCollectionResult::TestCollectionResult(char *description)
    : TestResult(description) {}
