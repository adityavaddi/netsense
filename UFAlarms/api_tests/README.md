## Guidelines to be followed for mocha tests:

1. No explicit timeout on test case (don't use this.timeout(..)).
2. No setTimeout() in test case. If used, then it should have a most reliable minimum value for timeout.
3. Use unique name for node ids (e.g. alertscorenodeid001) in test suite (test file).
4. Clean up data (use DELETE api's) on the end of test suite (test file).
5. Make sure each test suite (test file) can be run multiple times consecutively without the need of clean db.
