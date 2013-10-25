//Needs machine ip + port and type.
window.onload = function()
{
	//alert((window.location.href).split("/"));
    var socket = io.connect('http://130.238.15.206:8080');
    socket.on('datapoint', function(data) {
    	console.log(data);
    });
}
