# NetSense UI:

Netsence User Interface Application

The goal is to setup same development envernment for all the developers developing the application.

## Setup instructions:
```
Usage: yarn (start | [-D | -P | -O | -E | -T] add | remove | clean )
Example: 
       yarn start
       yarn run test
       
Options:
    -D  : Using --dev or -D will install one or more packages in your devDependencies       
    -P  : Using --peer or -P will install one or more packages in your peerDependencies.
    -O  : Using --optional or -O will install one or more packages in your optionalDependencies.
    -E  : Using --exact or -E installs the packages as exact versions.
          The default is to use the most recent release with the same major version.
          For example, yarn add foo@1.2.3 would accept version 1.9.1, 
          but yarn add foo@1.2.3 --exact would only accept version 1.2.3.
    -T  : Using --tilde or -T installs the most recent release of the packages that have the same minor 
          version. The default is to use the most recent release with the same major version.
          For example, yarn add foo@1.2.3 --tilde would accept 1.2.9 but not 1.3.0.

Commands:
    start   	: Running the project (coustom script)
    add     	: Adding a dependency
    upgrade 	: Upgrading a dependency
    remove  	: Removing a dependency
    install		: Installing all the dependencies of project
    clean   	: Cleans and removes unnecessary files from package dependencies
    run     	: Runs a defined package script

Install: 
    * yarn : 
            brew update
            brew install yarn
    * avn : 
            npm install -g avn avn-nvm avn-n
            avn setup
            
```
Learn more about [Yarn CLI][1].

## Running Application:

```
$ yarn start
```

## Environment Variables Used : 

```
APP           : app* || demo

NODE_ENV      : development* || production || docker

UI_PROTOCOL   : http* || https

UI_HOST       : localhost* || nsn-local.sensity.com

UI_PORT       : 8090

INTF_PROTOCOL : http* || https

INTF_HOST     : localhost* || nsn-local.sensity.com

INTF_PORT     : 10010* || 443
```

NOTE : * indecates default type in development environment



[1]: https://yarnpkg.com/en/docs/cli/