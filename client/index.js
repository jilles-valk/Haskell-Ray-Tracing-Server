var display = Vue.extend({
  props: ["image", "test"],
  template: `
        <div>
            <canvas ref="canvas" width="300px" height="200px" ></canvas>
            <div>Scene: {{test}}</div>
        </div>
        `,
  watch: {
    image: function(newVal, oldVal) {
      var ctx = this.$refs.canvas.getContext("2d");
      ctx.drawImage(image, 0, 0);
    }
  },
  methods: {}
});

var controls = Vue.extend({
  props: ["scene"],
  template: `
  <div>
  <div id="objectContainer" v-for="(object, index) in scene.objects">{{object.x}}
  <div id="objectTitle">Object {{index}}</div>
  <div class="objectButtons">
      x:<input :id="index" class="x" type="number" step=".1" v-on:change="changeObject" :value="object.center.x"/>
      y:<input :id="index" class="y" type="number" step=".1" v-on:change="changeObject" :value="object.center.y"/>
      z:<input :id="index" class="z" type="number" step=".1" v-on:change="changeObject" :value="object.center.z"/>
      r:<input :id="index" class="r" type="number" step=".1" v-on:change="changeObject" :value="object.radius"/>
  </div>
</div>
<div>Add object
    <button v-on:click="addObject">Add sphere</button>
</div>
<div>Camera controlls
<div>Position:
x:<input id="positionInput" class="x" type="number" step=".1" v-on:change="emitChangeView" :value="scene.view.position.x"/>
y:<input id="positionInput" class="y" type="number" step=".1" v-on:change="emitChangeView" :value="scene.view.position.y"/>
z:<input id="positionInput" class="z" type="number" step=".1" v-on:change="emitChangeView" :value="scene.view.position.z"/>
</div>
<div>Look:
    <button>Left</button>
    <button>Up</button>
    <button>Down</button>
    <button>Right</button>
    <button>Roll Left</button>
    <button>Roll Right</button>
</div>
</div>
</div>  
    `,
  methods: {
    changeObject: function(event) {
      if (event) {
        console.log(
          event.target.id + event.target.className + event.target.value
        );
        this.$emit(
          "change-this-object",
          event.target.id,
          event.target.className,
          event.target.value
        );
      }
    },
    addObject: function() {
      this.$emit("add-object");
    },
    emitChangeView: function(event) {
      if (event){
        this.$emit("change-view", event.target.className, event.target.value);
      }
    }
  }
});

const app = new Vue({
  el: "#app",
  components: { display, controls },
  data: function() {
    return {
      image: undefined,
      scene: { objects: [], 
        view: { position: {x: 0, y:0, z:5}, forwardVector: {xv:0, yv:0, zv:-1}, 
          upVector: {xv:0, yv:1, zv:0}, horPixels:300, verPixels:200, fieldOfView: 1.57, 
          aspectRatio:1.5} },
      status: "not connected",
      WS: undefined,
      test: "a"
    };
  },
  created: function() {
    this.connect();
  },

  methods: {
    connect: function() {
      WS = new WebSocket("ws://10.10.2.136:9000/");
      this.WS = WS;
      WS.onopen = () => {
        this.WS = WS;
        this.status = "connected";
        this.WS.onmessage = event => {
          // not: function(event){
          image = new Image();
          var urlObject = URL.createObjectURL(event.data);
          image.src = urlObject;
          image.onload = image => {
            this.image = image;
          };
        };
      };
    },
    changeObject(objectID, objectParameter, parameterValue) {
      if (objectParameter == "r") {
        this.scene.objects[objectID].radius = parseFloat(parameterValue);
      } else {
        this.scene.objects[objectID].center[objectParameter] = parseFloat(
          parameterValue
        );
      }
      this.sendScene();
    },
    addObject() {
      this.scene.objects.push({ radius: 1, center: { x: 0, y: 0, z: 0 } });
      this.sendScene();
    },
    sendScene() {
      WS.send(JSON.stringify(this.scene));
      this.test = JSON.stringify(this.scene);
    },
    changeView(id, value){
      this.scene.view.position[id] = value;
      this.sendScene();
    }
  }
});
