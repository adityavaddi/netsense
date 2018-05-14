# datagen

This tool is independent tool, need not have the whole projetc set up to use it.

## Installation

Download from only the data gen part from github. Exactly from where you see this readme file.

##Prerequisites
Install latest leiningen from http://leiningen.org/#install,    
Once suceesfully installed and in your path check whether you can run the command "lein"    
Most like leiningen will bring required clojure by itself - I have to double check on that - if it does not then      
Install clojure 1.7 or higher from http://clojure.org/downloads    
You are ready to use the tool     

## Usage
Go to the datagen directory of this project      
issue a command as below exclude the "$" :) you know it      
$lein deps (only one time required)       
Make sure you have a file called input.csv in the same directory          
$lein run           
You get a output.csv file - thats your new realistic site info.               
Rename the output.csv to reflect the name of the place and number of nodes in it, e.g. santaclara-2000.csv         

## Options
Now how to generate new input.csv?             

Go to this site http://www.geomidpoint.com/random/     
Do this as shown in the pic ![alt tag](https://raw.githubusercontent.com/Xeralux/Farallones/master/tools/datagen/resources/geomidpoint.random.png?token=AA5rLtlDk6jA3EO80wFTJ1kqG76y0UaTks5WB4QLwA%3D%3D)

If pciture does not appeat here for whatever reason, find it in the "resources" directory under "datagen".

Once you get the 2000 points, or whatever number you choose (cant be more than 2000), copy the resulting csv, make sure you select result format as "Comma Separated". Paste the copied comma separated result into input.csv - and you got your new input      

Follow the #Usage section now again 

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
