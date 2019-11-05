document.getElementById("renderButton").addEventListener("click", function() {
    tellServerToRender ();
} )
var ws = new WebSocket('ws://10.10.2.136:9000/');

ws.onmessage = function (event) {
    var img = document.createElement('img');
    var urlObject = URL.createObjectURL(event.data);
    img.src = urlObject;
    var ctx = document.getElementById("display").getContext('2d');
    img.onload = function(){
        ctx.drawImage(img, 0, 0)
      }
    // ws.send("From web app")
}

tellServerToRender = function () {
    ws.send(document.getElementById("sceneText").value);
}

