var display = Vue.extend({
  props: ["image", "test", "view"],
  template: `
        <div>{{view.horPixels}}
            <canvas id="canvas" ref="canvas" width="view.horPixels" height="view.verPixels"></canvas>
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
      <button :id="index" class="remove" v-on:click="emitRemoveObject">Delete</button>
  </div>
</div>
<div>Add object
    <button v-on:click="addObject">Add sphere</button>
</div>
<div id="lightsourcesContainer" v-for="(object, index) in scene.lightsources">{{object.x}}
  <div id="lightsourceTitle">Lightsource {{index}}</div>
  <div class="objectButtons">
      x:<input :id="index" class="x" type="number" step=".1" v-on:change="emitChangeLightsource" :value="object.location.x"/>
      y:<input :id="index" class="y" type="number" step=".1" v-on:change="emitChangeLightsource" :value="object.location.y"/>
      z:<input :id="index" class="z" type="number" step=".1" v-on:change="emitChangeLightsource" :value="object.location.z"/>
      intensity:<input :id="index" class="intensity" type="number" step=".1" v-on:change="emitChangeLightsource" :value="object.intensity"/>
      <button :id="index" class="remove" v-on:click="emitChangeLightsource">Delete</button>
  </div>
</div>
<div>Add object
    <button id="addLightsource" class="add"v-on:click="emitChangeLightsource">Add lightsource</button>
</div>
<div id="camera">Camera controlls
<div id="camera-pos">Position:
x:<input id="positionInput" class="x" type="number" step=".1" v-on:change="emitChangeViewPosition" :value="scene.view.position.x.toFixed(2)"/>
y:<input id="positionInput" class="y" type="number" step=".1" v-on:change="emitChangeViewPosition" :value="scene.view.position.y.toFixed(2)"/>
z:<input id="positionInput" class="z" type="number" step=".1" v-on:change="emitChangeViewPosition" :value="scene.view.position.z.toFixed(2)"/>
</div>
<div id="camera-orientation">Orientation: 
  <div id="top-orientation-buttons">
    <button id="rollLeft" v-on:click="emitChangeViewOrientation"
      v-on:keyup.enter="emitChangeViewOrientation">Roll Left</button>
    <button id="turnUp" v-on:click="emitChangeViewOrientation">Up</button>
    <button id="rollRight" v-on:click="emitChangeViewOrientation">Roll Right</button>
  </div>
  <div id="bottom-orientation-buttons">
    <button id="turnLeft" v-on:click="emitChangeViewOrientation">Left</button>
    <button id="turnDown" v-on:click="emitChangeViewOrientation">Down</button>
    <button id="turnRight" v-on:click="emitChangeViewOrientation">Right</button>
  </div>
  <div id="fieldOfView">
    Field of view: <input id="fieldOfViewInput" class="fov" type="number" step="1" min="0" max="180"v-on:change="emitChangeViewFOV" :value="(scene.view.fieldOfView*180/Math.PI).toFixed(1)"/>
  </div>
  <div id="resolution">
    Resolution: 
    horizontal: <input id="horPixels" class="res" type="number" step="1" min="0" max="3840"v-on:change="emitChanceRes" :value="scene.view.horPixels"/>
    vertical: <input id="verPixels" class="res" type="number" step="1" min="0" max="2160"v-on:change="emitChanceRes" :value="scene.view.verPixels"/>
  </div>
</div>
</div>
</div>  
    `,
  created: function () {
    window.addEventListener('keydown', this.onkey)
  },
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
    emitChangeLightsource: function(event) {
      if (event){
        this.$emit("change-lightsource", event.target.id, event.target.className, event.target.value);
      }
    },
    addObject: function() {
      this.$emit("add-object");
    },
    emitChangeViewPosition: function(event) {
      if (event){
        this.$emit("change-view", event.target.className, event.target.value);
      }
    },
    emitRemoveObject: function(event) {
      if (event){
        this.$emit("remove-object", event.target.id);
      }
    },
    emitChangeViewOrientation: function(event) {
      if (event){
        this.$emit("change-view-orientation", event.target.id);
      }
    },
    emitChangeViewFOV: function(event) {
      if (event){
        this.$emit("change-view-fov", event.target.value);
      }
    },
    emitChanceRes: function(event) {
      if (event){
        this.$emit("change-view-res", event.target.id, event.target.value);
      }
    },
    onkey: function(event) {
      console.log(event.code);
      switch (event.code) {
        case "KeyU":
          this.$emit("change-view-orientation", "rollLeft");
          break;
        case "KeyI":
          this.$emit("change-view-orientation", "turnUp");
          break;
        case "KeyO":
          this.$emit("change-view-orientation", "rollRight");
          break;
        case "KeyJ":
          this.$emit("change-view-orientation", "turnLeft");
          break;
        case "KeyK":
          this.$emit("change-view-orientation", "turnDown");
          break;
        case "KeyL":
          this.$emit("change-view-orientation", "turnRight");
          break;
        case "KeyW":
          this.$emit("change-view-orientation", "moveForward");
          break;
        case "KeyA":
          this.$emit("change-view-orientation", "moveLeft");
          break;
        case "KeyS":
          this.$emit("change-view-orientation", "moveBackward");
          break;
        case "KeyD":
          this.$emit("change-view-orientation", "moveRight");
          break;
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
               lightsources: [{location:{x: -3, y: 0, z: 0}, intensity: 0.5}],
        view: { position: {x: 0, y:0, z:5}, forwardVector: {xv:0, yv:0, zv:-1}, 
          upVector: {xv:0, yv:1, zv:0}, horPixels:300, verPixels:200, fieldOfView: 0.25*Math.PI} },
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
        this.scene.objects[objectID].center[objectParameter] = parseFloat(parameterValue);
      }
      this.sendScene();
    },
    changeLightsource(lightsourceID, lightsourceParameter, parameterValue) {
      switch (lightsourceParameter) {
        case "intensity":
            this.scene.lightsources[lightsourceID].intensity = parseFloat(parameterValue);
            break;
        case "remove":
            this.scene.lightsources.splice(lightsourceID,1);
            break;
        case "add":
            this.scene.lightsources.push({ intensity: 0.5, location: { x: 0, y: 0, z: 5 } });
            break;
        default:
          this.scene.lightsources[lightsourceID].location[lightsourceParameter] = parseFloat(parameterValue);
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
      this.scene.view.position[id] = parseFloat(value);
      this.sendScene();
    },
    removeObject(id) {
      this.scene.objects.splice(id,1);
      this.sendScene();
    },
    changeViewOrientation (command) {
      let increment = 0.025*Math.PI;
      let moveIncrement = 0.1;
      let rightVector = 
            rotate(this.scene.view.forwardVector, this.scene.view.upVector, 0.5*Math.PI);
      switch (command) {
        case "rollLeft":
          this.scene.view.upVector = 
            rotate(this.scene.view.upVector, this.scene.view.forwardVector, -increment);
          break;
        case "turnUp":
            this.scene.view.forwardVector = 
              rotate(this.scene.view.forwardVector, rightVector, -increment);
            this.scene.view.upVector = 
              rotate(this.scene.view.upVector, rightVector, -increment);
        break;
        case "rollRight":
          this.scene.view.upVector = 
            rotate(this.scene.view.upVector, this.scene.view.forwardVector, increment);
          break;
        case "turnLeft":
          this.scene.view.forwardVector = 
            rotate(this.scene.view.forwardVector, this.scene.view.upVector, increment);
          break;
        case "turnDown":
          this.scene.view.forwardVector = 
            rotate(this.scene.view.forwardVector, rightVector, increment);
          this.scene.view.upVector = 
              rotate(this.scene.view.upVector, rightVector, increment);
          break;
        case "turnRight":
          this.scene.view.forwardVector = 
            rotate(this.scene.view.forwardVector, this.scene.view.upVector, -increment);
          break;
        case "moveForward":
          this.scene.view.position = 
            addVectorToPoint(this.scene.view.position, 
              times(this.scene.view.forwardVector, moveIncrement));
          break;
        case "moveLeft":
          this.scene.view.position = 
            addVectorToPoint(this.scene.view.position, 
              times(rightVector, moveIncrement));
          break;
        case "moveBackward":
            this.scene.view.position = 
              addVectorToPoint(this.scene.view.position, 
                times(this.scene.view.forwardVector, -moveIncrement));
          break;
        case "moveRight":
          this.scene.view.position = 
            addVectorToPoint(this.scene.view.position, 
              times(rightVector, -moveIncrement));
          break;
      };
      this.sendScene();
    },
    changeViewFOV (value) {
      this.scene.view.fieldOfView = value*Math.PI/180;
      this.sendScene();
    },
    changeViewRes (id, value) {
      this.scene.view[id] = parseFloat(value)
      this.sendScene();
    }
  }
});

function rotate (v, k, theta) {
  return (add(
            times(v, Math.cos(theta)),
            add(
              times(
                cross(k, v),
                Math.sin(theta)
                ),
              times(
                k,
                dot(k,v) * (1 - Math.cos(theta))
                )
              )
            )
          );    
}

function cross (a, b) {
  return {xv:(a.yv*b.zv - a.zv*b.yv), 
          yv:(-(a.xv*b.zv - a.zv*b.xv)), 
          zv: (a.xv*b.yv - a.yv*b.xv)};
}

function times (v, s) {
  return {xv:(v.xv*s), 
          yv:(v.yv*s), 
          zv:(v.zv*s)};
}

function add (a, b) {
  return {xv:(a.xv+b.xv), 
          yv:(a.yv+b.yv), 
          zv:(a.zv+b.zv)};
}

function addVectorToPoint (a, b) {
  return {x:(a.x+b.xv), 
          y:(a.y+b.yv), 
          z:(a.z+b.zv)};
}

function dot (a, b) {
  return ((a.xv*b.xv) + 
          (a.yv*b.yv) + 
          (a.zv*b.zv));
}
