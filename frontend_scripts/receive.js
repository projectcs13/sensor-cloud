
// Create context using rabbit.js (cfr ZMQ),
// io and the subscriber socket.

var exchange = process.argv[2],
    port = 8080,
    context = require('rabbit.js').createContext(),
    io = require('socket.io').listen(port),
    sub = context.socket('SUB');

sub.setEncoding('utf8');

// A websocket is connected (eg: browser).
io.sockets.on('connection', function(socket) {
    console.log("Websocket is connected.");
    // Connect socket to updates exchange.
    sub.connect(exchange);
    console.log("EXCHANGE : "+exchange);   
    // Register handler that hanles incoming data when the socket
    // detects new data on our queues.
    // When receiving data, it gets pushed to the connectec websocket.
    sub.on('data', function(data) {
        var type = data.substring(6, 15),
            time = data.substring(18, 37),
            rest = data.substring(40);
            //Add support for stream_id
        console.log("TYPE : "+type+" TIME : "+time+" VALUE : "+value);

        //socket.emit(message.type, message.data);
    });
});
