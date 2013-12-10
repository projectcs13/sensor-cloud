
// Create context using rabbit.js (cfr ZMQ),
// io and the subscriber socket.

var port = 8080,
    context = require('rabbit.js').createContext(),
    io = require('socket.io').listen(port),
    url = require('url');
    var streams = new Object();
    process.setMaxListeners(0);
    // Limits debug messages that are printed by socket.io
    io.set('log level', 1); 

// A websocket is connected (eg: browser).
io.sockets.on('connection', function(socket) {
    var sub = context.socket('SUB');
    sub.setEncoding('utf8');
    var con_url = (url.parse(socket.handshake.url, true).query.ns).split("/");
    var namespace = con_url[con_url.length - 1];
    if(streams[namespace] == undefined) {
        streams[namespace] = 1;
        sub.connect(namespace);
        console.log("Created "+namespace+" with "+streams[namespace]+" users connected.");
    }else {
        streams[namespace]++;
        console.log("Connected to "+namespace+" with "+streams[namespace]+" users connected.");
    }

    io.of('/'+namespace).on('connection', function(sock) {
        sub.on('data', function(data) {
		sock.send(data);        
	    //console.log(io.sockets.clients().length);
        });
    });
    socket.on('disconnect', function() {
    streams[namespace]--;
    
    console.log("Disconnected from "+namespace+" with "+streams[namespace]+" users connected.");
    if(streams[namespace] == 0) {
        console.log("No user connected to "+namespace);
        //streams[namespace] = undefined;
        //sub.destroy();
        console.log("NameSpace : "+namespace+" is "+streams[namespace]);
    }
    });
});
