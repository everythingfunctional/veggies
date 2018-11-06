/*
MODULE cVegetables
*/

#include <algorithm>
#include <cstring>
#include <iostream>
#include <numeric>
#include <string>
#include <vector>

class Result {
private:
  std::string _message;
  int _num_failing_asserts;
  int _num_passing_asserts;
  bool _passed;

public:
  Result(std::string message, bool passed);
  Result(std::string message, bool passed, int num_failing_asserts,
         int num_passing_asserts);
  Result *combineWith(Result *addition);
  std::string message();
  int numAsserts();
  int numFailingAsserts();
  int numPassingAsserts();
  bool passed();
};

class TestResult {
protected:
  std::string _description;

public:
  TestResult(std::string description);
  virtual std::string failureDescription() = 0;
  virtual int numAsserts() = 0;
  virtual int numCases() = 0;
  virtual int numFailingCases() = 0;
  virtual int numPassingCases() = 0;
  virtual bool passed() = 0;
  virtual std::string verboseDescription() = 0;
};

class TestCaseResult : public TestResult {
private:
  Result *_result;

public:
  TestCaseResult(std::string _description, Result *result);
  std::string failureDescription();
  int numAsserts();
  int numCases();
  int numFailingAsserts();
  int numFailingCases();
  int numPassingAsserts();
  int numPassingCases();
  bool passed();
  std::string verboseDescription();
};

class TestCollectionResult : public TestResult {
private:
  std::vector<TestResult *> _results;

public:
  TestCollectionResult(std::string _description,
                       std::vector<TestResult *> results);
  std::string failureDescription();
  int numAsserts();
  int numCases();
  int numFailingCases();
  int numPassingCases();
  bool passed();
  std::string verboseDescription();
};

class Test {
protected:
  std::string _description;

public:
  Test(std::string description);
  virtual std::string description() = 0;
  virtual int numCases() = 0;
  virtual TestResult *run() = 0;
};

class TestCase : public Test {
private:
  void *_test;

public:
  TestCase(std::string description, void *test);
  std::string description();
  int numCases();
  TestCaseResult *run();
};

class TestCollection : public Test {
private:
  std::vector<Test *> _tests;

public:
  TestCollection(std::string description);
  void addTest(Test *test);
  std::string description();
  int numCases();
  TestCollectionResult *run();
};

extern "C" Result *runATest(void *test);

std::string join(std::vector<std::string> strings, std::string separator) {
  std::string joined = strings[0];
  for (size_t i = 1; i < strings.size(); i++) {
    joined += separator + strings[i];
  }
  return joined;
}

std::vector<std::string> doSplit(std::string, std::string separators);

std::vector<std::string> splitAt(std::string string, std::string separators) {
  if (separators.size() > 0) {
    if (string.size() > 0) {
      if (separators.find(string[0]) != std::string::npos) {
        return splitAt(string.substr(1), separators);
      } else if (separators.find(string[string.size() - 1]) !=
                 std::string::npos) {
        return splitAt(string.substr(0, string.size() - 1), separators);
      } else {
        return doSplit(string, separators);
      }
    } else {
      std::vector<std::string> result;
      result.push_back(std::string(""));
      return result;
    }
  } else {
    std::vector<std::string> result;
    result.push_back(string);
    return result;
  }
}

std::vector<std::string> doSplit(std::string string, std::string separators) {
  size_t i;
  for (i = 1; i < string.size(); i++) {
    if (separators.find(string[i]) != std::string::npos)
      break;
  }
  if (i < string.size()) {
    std::string this_string = string.substr(0, i);
    std::vector<std::string> rest = splitAt(string.substr(i), separators);
    std::vector<std::string> result;
    result.push_back(this_string);
    result.insert(result.end(), rest.begin(), rest.end());
    return result;
  } else {
    std::vector<std::string> result;
    result.push_back(string);
    return result;
  }
}

std::string hangingIndent(std::string string) {
  std::vector<std::string> lines = splitAt(string, "\n");
  return join(lines, "\n    ");
}

Result::Result(std::string message, bool passed)
    : _message(message), _passed(passed) {
  if (passed) {
    this->_num_failing_asserts = 0;
    this->_num_passing_asserts = 1;
  } else {
    this->_num_failing_asserts = 1;
    this->_num_passing_asserts = 0;
  }
}

Result::Result(std::string message, bool passed, int num_failing_asserts,
               int num_passing_asserts)
    : _message(message), _num_failing_asserts(num_failing_asserts),
      _num_passing_asserts(num_passing_asserts), _passed(passed) {}

std::string Result::message() { return this->_message; }

int Result::numAsserts() {
  return this->_num_failing_asserts + this->_num_passing_asserts;
}

int Result::numFailingAsserts() { return this->_num_failing_asserts; }

int Result::numPassingAsserts() { return this->_num_passing_asserts; }

bool Result::passed() { return this->_passed; }

Result *Result::combineWith(Result *addition) {
  if (this->_passed && addition->_passed) {
    return new Result(
        "", true, this->_num_failing_asserts + addition->_num_failing_asserts,
        this->_num_passing_asserts + addition->_num_passing_asserts);
  } else if (this->_passed) {
    return new Result(
        addition->_message, false,
        this->_num_failing_asserts + addition->_num_failing_asserts,
        this->_num_passing_asserts + addition->_num_passing_asserts);
  } else if (addition->_passed) {
    return new Result(
        this->_message, false,
        this->_num_failing_asserts + addition->_num_failing_asserts,
        this->_num_passing_asserts + addition->_num_passing_asserts);
  } else {
    return new Result(
        this->_message + "\n" + addition->_message, false,
        this->_num_failing_asserts + addition->_num_failing_asserts,
        this->_num_passing_asserts + addition->_num_passing_asserts);
  }
}

extern "C" Result *cCombineResults(Result *lhs, Result *rhs) {
  return lhs->combineWith(rhs);
}

extern "C" Result *cResult(char *message, bool passed) {
  Result *result = new Result(std::string(message), passed);
  return result;
}

Test::Test(std::string description) : _description(description) {}

TestCase::TestCase(std::string description, void *test)
    : Test(description), _test(test) {}

std::string TestCase::description() { return this->_description; }

int TestCase::numCases() { return 1; }

TestCaseResult *TestCase::run() {
  Result *test_result = runATest(this->_test);
  TestCaseResult *result = new TestCaseResult(this->_description, test_result);
  return result;
}

extern "C" TestCaseResult *cRunTestCase(TestCase *test_case) {
  return test_case->run();
}

extern "C" TestCase *cTestCase(char *description, void *test) {
  TestCase *test_case = new TestCase(std::string(description), test);
  return test_case;
}

extern "C" void cTestCaseDescription(TestCase *test_case, char *description,
                                     int maxlen) {
  std::string the_description = test_case->description();
  strncpy(description, the_description.c_str(), maxlen);
}

extern "C" int cTestCaseNumCases(TestCase *test_case) {
  return test_case->numCases();
}

TestCollection::TestCollection(std::string description) : Test(description) {}

void TestCollection::addTest(Test *test) { this->_tests.push_back(test); }

std::string TestCollection::description() {
  std::vector<std::string> individual_descriptions;
  individual_descriptions.resize(this->_tests.size());
  std::transform(this->_tests.begin(), this->_tests.end(),
                 individual_descriptions.begin(),
                 [](Test *test) { return test->description(); });
  return hangingIndent(this->_description + "\n" +
                       join(individual_descriptions, "\n"));
}

int TestCollection::numCases() {
  std::vector<int> individual_nums;
  individual_nums.resize(this->_tests.size());
  std::transform(this->_tests.begin(), this->_tests.end(),
                 individual_nums.begin(),
                 [](Test *test) { return test->numCases(); });
  return std::accumulate(individual_nums.begin(), individual_nums.end(), 0);
}

TestCollectionResult *TestCollection::run() {
  std::vector<TestResult *> results;
  for (auto const &test : this->_tests) {
    results.push_back(test->run());
  }
  TestCollectionResult *result =
      new TestCollectionResult(this->_description, results);
  return result;
}

extern "C" TestCollection *cTestCollection(char *description) {
  TestCollection *test_collection =
      new TestCollection(std::string(description));
  return test_collection;
}

extern "C" void cAddTest(TestCollection *collection, Test *test) {
  collection->addTest(test);
}

extern "C" TestCollectionResult *cRunTestCollection(TestCollection *tests) {
  return tests->run();
}

extern "C" void cTestCollectionDescription(TestCollection *test_collection,
                                           char *description, int maxlen) {
  std::string the_description = test_collection->description();
  strncpy(description, the_description.c_str(), maxlen);
}

extern "C" int cTestCollectionNumCases(TestCollection *tests) {
  return tests->numCases();
}

TestResult::TestResult(std::string description) : _description(description) {}

TestCaseResult::TestCaseResult(std::string description, Result *result)
    : TestResult(description), _result(result) {}

std::string TestCaseResult::failureDescription() {
  if (this->passed()) {
    return "";
  } else {
    return hangingIndent(this->_description + "\n" + this->_result->message());
  }
}

int TestCaseResult::numAsserts() { return this->_result->numAsserts(); }

int TestCaseResult::numCases() { return 1; }

int TestCaseResult::numFailingAsserts() {
  return this->_result->numFailingAsserts();
}

int TestCaseResult::numFailingCases() {
  if (this->passed()) {
    return 0;
  } else {
    return 1;
  }
}

int TestCaseResult::numPassingAsserts() {
  return this->_result->numPassingAsserts();
}

int TestCaseResult::numPassingCases() {
  if (this->passed()) {
    return 1;
  } else {
    return 0;
  }
}

bool TestCaseResult::passed() { return this->_result->passed(); }

std::string TestCaseResult::verboseDescription() {
  if (this->passed()) {
    return this->_description;
  } else {
    return this->failureDescription();
  }
}

extern "C" void cTestCaseResultFailureDescription(TestCaseResult *test_case,
                                                  char *description,
                                                  int maxlen) {
  std::string the_description = test_case->failureDescription();
  strncpy(description, the_description.c_str(), maxlen);
}

extern "C" int cTestCaseNumAsserts(TestCaseResult *test) {
  return test->numAsserts();
}

extern "C" int cTestCaseNumFailingAsserts(TestCaseResult *test) {
  return test->numFailingAsserts();
}

extern "C" int cTestCaseNumPassingAsserts(TestCaseResult *test) {
  return test->numPassingAsserts();
}

extern "C" int cTestCaseResultNumCases(TestCaseResult *test) {
  return test->numCases();
}

extern "C" int cTestCaseNumFailingCases(TestCaseResult *test) {
  return test->numFailingCases();
}

extern "C" int cTestCaseNumPassingCases(TestCaseResult *test) {
  return test->numPassingCases();
}

extern "C" bool cTestCasePassed(TestCaseResult *test) { return test->passed(); }

extern "C" void cTestCaseResultVerboseDescription(TestCaseResult *test_case,
                                                  char *description,
                                                  int maxlen) {
  std::string the_description = test_case->verboseDescription();
  strncpy(description, the_description.c_str(), maxlen);
}

TestCollectionResult::TestCollectionResult(std::string description,
                                           std::vector<TestResult *> results)
    : TestResult(description), _results(results) {}

std::string TestCollectionResult::failureDescription() {
  if (this->passed()) {
    return "";
  } else {
    return "";
  }
}

int TestCollectionResult::numAsserts() {
  std::vector<int> individual_nums;
  individual_nums.resize(this->_results.size());
  std::transform(this->_results.begin(), this->_results.end(),
                 individual_nums.begin(),
                 [](TestResult *result) { return result->numAsserts(); });
  return std::accumulate(individual_nums.begin(), individual_nums.end(), 0);
}

int TestCollectionResult::numCases() {
  std::vector<int> individual_nums;
  individual_nums.resize(this->_results.size());
  std::transform(this->_results.begin(), this->_results.end(),
                 individual_nums.begin(),
                 [](TestResult *result) { return result->numCases(); });
  return std::accumulate(individual_nums.begin(), individual_nums.end(), 0);
}

int TestCollectionResult::numFailingCases() {
  std::vector<int> individual_nums;
  individual_nums.resize(this->_results.size());
  std::transform(this->_results.begin(), this->_results.end(),
                 individual_nums.begin(),
                 [](TestResult *result) { return result->numFailingCases(); });
  return std::accumulate(individual_nums.begin(), individual_nums.end(), 0);
}

int TestCollectionResult::numPassingCases() {
  std::vector<int> individual_nums;
  individual_nums.resize(this->_results.size());
  std::transform(this->_results.begin(), this->_results.end(),
                 individual_nums.begin(),
                 [](TestResult *result) { return result->numPassingCases(); });
  return std::accumulate(individual_nums.begin(), individual_nums.end(), 0);
}

bool TestCollectionResult::passed() {
  return std::all_of(this->_results.begin(), this->_results.end(),
                     [](TestResult *result) { return result->passed(); });
}

std::string TestCollectionResult::verboseDescription() {
  std::vector<std::string> individual_descriptions;
  individual_descriptions.resize(this->_results.size());
  std::transform(this->_results.begin(), this->_results.end(),
                 individual_descriptions.begin(), [](TestResult *result) {
                   return result->verboseDescription();
                 });
  return hangingIndent(this->_description + "\n" +
                       join(individual_descriptions, "\n"));
}

extern "C" bool cTestCollectionPassed(TestCollectionResult *collection) {
  return collection->passed();
}

extern "C" int cTestCollectionResultNumCases(TestCollectionResult *collection) {
  return collection->numCases();
}

extern "C" int cTestCollectionNumAsserts(TestCollectionResult *collection) {
  return collection->numAsserts();
}

extern "C" int
cTestCollectionNumFailingCases(TestCollectionResult *collection) {
  return collection->numFailingCases();
}

extern "C" int
cTestCollectionNumPassingCases(TestCollectionResult *collection) {
  return collection->numPassingCases();
}

extern "C" void
cTestCollectionResultFailureDescription(TestCollectionResult *collection,
                                        char *description, int maxlen) {
  std::string the_description = collection->failureDescription();
  strncpy(description, the_description.c_str(), maxlen);
}

extern "C" void
cTestCollectionResultVerboseDescription(TestCollectionResult *collection,
                                        char *description, int maxlen) {
  std::string the_description = collection->verboseDescription();
  strncpy(description, the_description.c_str(), maxlen);
}
