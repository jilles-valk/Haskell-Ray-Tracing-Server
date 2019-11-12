

Vue.component("main-display", {
  props: ['image'],
  template: `
        <div>
            <canvas ref="display" width="300px" height="200px" ></canvas>
            <v-image :config="{
                image: image
              }"/>
        </div>
        `,
  methods: {
    // changeObject: function(event) {
    //   if (event) {
    //     this.$emit("changedThisObject", event.target.id);
    //   }
    // }
  }
});

Vue.component("controls", {
  props: ['scene'],
  template: `
  <div>
  <div id="object.index" v-for="(object, index) in scene.objects">{{object.x}}
  <div id="objectTitle">Object one</div>
  <div class="objectButtons">
      x:<input :id="index" class="x" type="number" v-on:change="changeObject" :value="object.x"/>
      y:<input :id="index" class="y" type="number" v-on:change="changeObject" :value="object.y"/>
      z:<input :id="index" class="z" type="number" v-on:change="changeObject" :value="object.z"/>
      r:<input :id="index" class="r" type="number" v-on:change="changeObject" :value="object.r"/>
  </div>
</div>
</div>  
    `,
  methods: {
    changeObject: function(event) {
        if (event) {
            console.log(event.target.id + event.target.className + event.target.value)
            this.$emit("change-this-object", event.target.id, event.target.className, event.target.value);
        }
    }
  }
});

const app = new Vue({
  el: "#app",

  data: {
    image: "no image",
    scene: {objects: [{x:0, y:0, z:0, r:1},
                      {x:0, y:0, z:0, r:1},
                      {x:0, y:0, z:0, r:2}]
            },
    // scene:"[{'radius':1,'center':{'z':0,'x':-1,'y':0.5}}, {'radius':1,'center':{'z':0,'x':1,'y':0.5}}, {'radius':1.5,'center':{'z':0,'x':0,'y':-0.5}}]",
    status: "not connected",
    WS: undefined
  },

  computed: {},

  created: function () {
      console.log("Connect")
    this.connect();
  },

  methods: {
        connect() {
            this.WS = new WebSocket("ws://10.10.2.136:9000/");
            this.WS.onopen = () => {
                console.log("opened")
                this.status = "connected";
                this.WS.onmessage = function (event) {
                    console.log("message")
                    console.log(event)
                    image = new Image();
                    var urlObject = URL.createObjectURL(event.data);
                    image.src = urlObject;
                    // var ctx = this.$refs.image.getContext("2d");
                    // onload = function() {
                    //   ctx.drawImage(img, 0, 0);
                    // };
                    this.image = image;
                    // var arrayBufferView = new Uint8Array( event.data );
                    // var blob = new Blob( [ arrayBufferView ], { type: "image/jpeg" } );
                    // var urlCreator = window.URL || window.webkitURL;
                    // var imageUrl = urlCreator.createObjectURL( blob );
                    // this.image = new Image();
                    // this.image.src = imageUrl;
                    console.log(this.image)
                };
          };
        },
        showImage(event) {
            var arrayBufferView = new Uint8Array( event.data );
            var blob = new Blob( [ arrayBufferView ], { type: "image/jpeg" } );
            var urlCreator = window.URL || window.webkitURL;
            var imageUrl = urlCreator.createObjectURL( blob );
            this.image = new Image();
            this.img.src = imageUrl;
            console.log(image)
        },
        changeScene(objectID, objectParameter, parameterValue){
            console.log(objectID)
            console.log(this.scene.objects[objectID].x)
            this.scene.objects[objectID][objectParameter] = parameterValue;
            console.log(JSON.stringify(this.scene.objects))
            this.WS.send(JSON.stringify(this.scene.objects));
        }
    // async startGame(playerOne, playerTwo) {
    //     const response = await fetch('api/players', {
    //         method: 'POST',
    //         headers: {
    //             'Accept': 'application/json',
    //             'Content-Type': 'application/json'
    //         },
    //         body: JSON.stringify({ nameplayer1: playerOne, nameplayer2: playerTwo })
    //     });
    //     const gameState = await response.json();
    //     this.gameState = gameState;
    // },
    // async tellServerToPlayBowl(bowlNum) {
    //     const response = await fetch('api/play/' + bowlNum, {
    //         method: 'PUT'});
    //     const gameState = await response.json();
    //     this.gameState = gameState;
    // }
  }
});

// document.getElementById("renderButton").addEventListener("click", function() {
//     tellServerToRender();
//   });
//   var ws = new WebSocket("ws://10.10.2.136:9000/");
  
//   ws.onmessage = function(event) {
//     var img = document.createElement("img");
//     var urlObject = URL.createObjectURL(event.data);
//     img.src = urlObject;
//     var ctx = document.getElementById("display").getContext("2d");
//     img.onload = function() {
//       ctx.drawImage(img, 0, 0);
//     };
//   };
  
//   tellServerToRender = function() {
//     ws.send(document.getElementById("sceneText").value);
//   };
