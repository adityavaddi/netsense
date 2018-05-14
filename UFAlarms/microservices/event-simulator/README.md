Event simulator is developed to simulate Northbound and Southbound events

By default it will publish and subscribe from Northbound Simulator and Southbound simulator

If you want to run only  Northbound publisher and Northbound subscriber then you have to make southbound false. 

Example:

  southbound-publisher-enable = "false"
  
  southbound-subscriber-enable = "false"
  
 If you want only run southbound simulator publisher then make other values false
 
 Example:
 
   northbound-publisher-enable = "false"
   
   northbound-subscriber-enable = "false"
   
   southbound-publisher-enable = "true"
   
   southbound-subscriber-enable = "false"