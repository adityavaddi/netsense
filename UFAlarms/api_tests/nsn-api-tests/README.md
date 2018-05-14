# api_collections
postman collections to run test against netsense api

running the collections:

- Install postman
https://www.getpostman.com/

- Install newman

Newman is built on Node.js. To run Newman, make sure you have Node.js installed. Node.js can be downloaded and installed on Linux, Windows, and Mac OSX.
- npm install -g newman

$ newman run -h <br />
Options: <br />

Utility: <br />
-h, --help                      output usage information <br />
-v, --version                   output the version number <br />

Basic setup: <br />
--folder [folderName]           Specify a single folder to run from a collection. <br />
-e, --environment [file|URL]    Specify a Postman environment as a JSON [file]  <br />
-d, --data [file]               Specify a data file to use either json or csv <br />
-g, --global [file]             Specify a Postman globals file as JSON [file] <br />
-n, --iteration-count [number]  Define the number of iterations to run <br />

Request options: <br />
--delay-request [number]        Specify a delay (in ms) between requests [number] <br />
--timeout-request [number]      Specify a request timeout (in ms) for a request <br />

Misc.: <br />
--bail                          Stops the runner when a test case fails <br />
--silent                        Disable terminal output <br />
--no-color                      Disable colored output <br />
-k, --insecure                  Disable strict ssl <br />
-x, --suppress-exit-code        Continue running tests even after a failure, but exit with code=0 <br />
--ignore-redirects              Disable automatic following of 3XX responses <br />

Example

- newman run collections/SensityAPITests.postman_collection.json -e CI_Environment.postman_environment.json --reporters cli,html --reporter-html-export <<<html_path>>>


Pointing the enviroment for the test

- Open the CI_Environment.postman_environment.json
- look for the hostname object in the json

``` javascript
 {
      "enabled": true,
      "hovered": false,
      "type": "text",
      "value": "cdlxd.sensity.com",
      "key": "hostname"
    }
```
 and change it to the server you want it to run against.
