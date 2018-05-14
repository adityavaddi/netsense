#!/bin/bash
echo "staring script..."

pwd
node -v
which node
npm -v
which npm
npm install -g protractor mocha chai


# webdriver-manager update
webdriver-manager start &>/dev/null & PID=$!
echo $"selenium-server server startup with process ID : $PID."

# Running the test cases
npm install
npm test

# Killing Selenium web driver
kill -9 $(ps -x | grep 'selenium-server-standalone' | awk '{print $1}')
# ps -ef | grep 'selenium-server-standalone' | awk '{print $2}' | xargs kill -9

echo "Done"
