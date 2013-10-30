// Publisher
context = require('rabbit.js').createContext();
 
context.on('ready', function() {
  var pub = context.socket('PUB');
    pub.connect('my-exchange', function() {
    	var myInt = 0;
        setInterval(function() {
                pub.write({'type':'datapoint', 'data':'hello, world '+(myInt++)});
        }, 500);
    });
});