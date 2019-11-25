var websocketAddress = "ws://10.10.2.136:9000/";

var display = Vue.extend({
  props: ["image", "test", "view", "timeSendToRecieve"],
  template: `
        <div>{{view.horPixels}}
            <canvas id="canvas" ref="canvas" width="1920px" height="1080px"></canvas>
            <div id="displayInfo">Time taken to render image: {{timeSendToRecieve}}ms.</div>
        </div>
        `,
  watch: {
    image: function(newVal, oldVal) {
      var ctx = this.$refs.canvas.getContext("2d");
      ctx.drawImage(image, 0, 0, 1920, 1080);
    }
  },
  methods: {}
});

var controls = Vue.extend({
  props: ["scene", "sceneNumText", "timeSendToRecieve"],
  template: `
  <div>
  <div id="objectsAndLightsources">
      <div id="objectsAndAdd">
          <div id="objectContainer" v-for="(object, index) in scene.objects">{{object.x}}
              <div id="objectTitle">Object {{index}}</div>
              <div class="objectButtons">
                  x:<input :id="index" class="x" type="number" step=".1" v-on:change="changeObject"
                      :value="object.center.x" />
                  y:<input :id="index" class="y" type="number" step=".1" v-on:change="changeObject"
                      :value="object.center.y" />
                  z:<input :id="index" class="z" type="number" step=".1" v-on:change="changeObject"
                      :value="object.center.z" />
                  r:<input :id="index" class="r" type="number" step=".1" v-on:change="changeObject"
                      :value="object.radius" />
                  <button :id="index" class="remove" v-on:click="emitRemoveObject">Delete</button>
              </div>
          </div>
          <div id="addObjectContainer">
              <button v-on:click="addObject">Add sphere</button>
          </div>
      </div>
      <div id="lightsourcesAndAdd">
          <div id="lightsourcesContainer" v-for="(object, index) in scene.lightsources">{{object.x}}
              <div id="lightsourceTitle">Lightsource {{index}}</div>
              <div class="objectButtons">
                  x:<input :id="index" class="x" type="number" step=".1" v-on:change="emitChangeLightsource"
                      :value="object.location.x" />
                  y:<input :id="index" class="y" type="number" step=".1" v-on:change="emitChangeLightsource"
                      :value="object.location.y" />
                  z:<input :id="index" class="z" type="number" step=".1" v-on:change="emitChangeLightsource"
                      :value="object.location.z" />
                  I:<input :id="index" class="intensity" type="number" step=".1"
                      v-on:change="emitChangeLightsource" :value="object.intensity" />
                  <button :id="index" class="remove" v-on:click="emitChangeLightsource">Delete</button>
              </div>
          </div>
          <div id="addLightsourceContainer">
              <button id="addLightsource" class="add" v-on:click="emitChangeLightsource">Add lightsource</button>
          </div>
      </div>
  </div>
  <div id="bottomControlls">
      <div id="camera">Camera controlls
          <div id="camera-pos">Position:
              x:<input id="positionInput" class="x" type="number" step=".1" v-on:change="emitChangeViewPosition"
                  :value="scene.view.position.x.toFixed(1)" />
              y:<input id="positionInput" class="y" type="number" step=".1" v-on:change="emitChangeViewPosition"
                  :value="scene.view.position.y.toFixed(1)" />
              z:<input id="positionInput" class="z" type="number" step=".1" v-on:change="emitChangeViewPosition"
                  :value="scene.view.position.z.toFixed(1)" />
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
                  Field of view: <input id="fieldOfViewInput" class="fov" type="number" step="1" min="0" max="180"
                      v-on:change="emitChangeViewFOV" :value="(scene.view.fieldOfView*180/Math.PI).toFixed(1)" />
              </div>
              <div id="resolution">
                  Resolution:
                  horizontal: <input id="horPixels" class="res" type="number" step="1" min="0" max="1920"
                      v-on:change="emitChanceRes" :value="scene.view.horPixels" />
              </div>
          </div>
      </div>
      <div id="instructions">
          You can move through the scene using the "wasd" keys, you can look around using "ijkl" and "uo" are for
          rolling left and right.
      </div>
      <div id="loadSave">
          Select scene:
          <input id="sceneNum" type="number" v-on:change="emitChangeSaveScene" step="1" min="0" value="0" />
          <button id="loadScene" v-on:click="emitChangeSaveScene">Load</button>
          <button id="saveScene" v-on:click="emitChangeSaveScene">Save</button>
          <div id="storageText">
              {{sceneNumText}}
          </div>
      </div>
  </div>
</div>
    `,
    data: function() {
      return {
          rollLeft: false,
          turnUp: false,
          rollRight: false,
          turnLeft: false,
          turnDown: false,
          turnRight: false,
          moveForward: false,
          moveLeft: false,
          moveBackward: false,
          moveRight: false,
          noCommands: true
    }},
  created: function () {
    window.addEventListener('keydown', this.onKeyDown);
    window.addEventListener('keyup', this.onKeyUp);
    setInterval(this.collectKeys, 17);
    this.emitSendScene();
    this.$emit("change-save-scene","sceneNum", 0);
  },
  methods: {
    changeObject: function(event) {
      if (event) {
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
        this.noCommands = false;
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
    emitChangeSaveScene: function(event) {
      if (event){
        this.$emit("change-save-scene", event.target.id, document.getElementById("sceneNum").value);
      }
    },
    onKeyDown: function(event) {
      switch (event.code) {
        case "KeyU":
          this.rollLeft = true;
          break;
        case "KeyI":
          this.turnUp = true;
          break;
        case "KeyO":
          this.rollRight = true;
          break;
        case "KeyJ":
          this.turnLeft = true;
          break;
        case "KeyK":
          this.turnDown = true;
          break;
        case "KeyL":
          this.turnRight = true;
          break;
        case "KeyW":
          this.moveForward = true;
          break;
        case "KeyA":
          this.moveLeft = true;
          break;
        case "KeyS":
          this.moveBackward = true;
          break;
        case "KeyD":
          this.moveRight = true;
          break;
      }
    },
    onKeyUp: function(event) {
      setTimeout(this.removeCommands(), 10);
    },
    removeCommands: function(){
      switch (event.code) {
        case "KeyU":
          this.rollLeft = false;
          break;
        case "KeyI":
          this.turnUp = false;
          break;
        case "KeyO":
          this.rollRight = false;
          break;
        case "KeyJ":
          this.turnLeft = false;
          break;
        case "KeyK":
          this.turnDown = false;
          break;
        case "KeyL":
          this.turnRight = false;
          break;
        case "KeyW":
          this.moveForward = false;
          break;
        case "KeyA":
          this.moveLeft = false;
          break;
        case "KeyS":
          this.moveBackward = false;
          break;
        case "KeyD":
          this.moveRight = false;
          break;
      }
    },
    collectKeys: function() {
        if (this.rollLeft){
          this.$emit("change-view-orientation", "rollLeft");
          this.noCommands = false;
        } 
        if (this.turnUp){
          this.$emit("change-view-orientation", "turnUp");
          this.noCommands = false;
        } 
        if (this.rollRight){
          this.$emit("change-view-orientation", "rollRight");
          this.noCommands = false;
        } 
        if (this.turnLeft){
          this.$emit("change-view-orientation", "turnLeft");
          this.noCommands = false;
        } 
        if (this.turnDown){
          this.$emit("change-view-orientation", "turnDown");
          this.noCommands = false;
        } 
        if (this.turnRight){
          this.$emit("change-view-orientation", "turnRight");
          this.noCommands = false;
        } 
        if (this.moveForward){
          this.$emit("change-view-orientation", "moveForward");
          this.noCommands = false;
        } 
        if (this.moveLeft){
          this.$emit("change-view-orientation", "moveLeft");
          this.noCommands = false;
        } 
        if (this.moveBackward){
          this.$emit("change-view-orientation", "moveBackward");
          this.noCommands = false;
        } 
        if (this.moveRight){
          this.$emit("change-view-orientation", "moveRight");
          this.noCommands = false;
        }
    },
    emitSendScene: function() {
      var interval = 17;
        if (this.$props.timeSendToRecieve > interval){
          interval = this.$props.timeSendToRecieve;
        }
        if (!this.noCommands){
          this.$emit("send-scene");
          this.noCommands = true;
          setTimeout(this.emitSendScene, interval*1.5);
        }
        else{
          setTimeout(this.emitSendScene, 17);
        }
    }
  },
});


const app = new Vue({
  el: "#app",
  components: { display, controls },
  data: function() {
    return {
      image: undefined,
      scene: { objects: [],
               lightsources: [{location:{x: -3, y: 0, z: 0}, intensity: 1}],
        view: { position: {x: 0, y:0, z:5}, forwardVector: {xv:0, yv:0, zv:-1}, 
          upVector: {xv:0, yv:1, zv:0}, horPixels:320, verPixels:180, fieldOfView: 0.25*Math.PI} },
      status: "not connected",
      WS: undefined,
      test: "a",
      sceneNumText: "",
      startSendTime: 0,
      timeSendToRecieve: 100
    };
  },
  created: function() {
    this.connect();
  },

  methods: {
    connect: function() {
      WS = new WebSocket(websocketAddress);
      this.WS = WS;
      WS.onopen = () => {
        this.WS = WS;
        this.status = "connected";
        this.WS.onmessage = event => {
          var d = new Date();
          this.timeSendToRecieve = d.getTime() - this.startSendTime;
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
      var d = new Date();
      this.startSendTime = d.getTime();
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
      let increment = 0.0025*Math.PI;
      let moveIncrement = 0.01;
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
    },
    changeViewFOV (value) {
      this.scene.view.fieldOfView = value*Math.PI/180;
      this.sendScene();
    },
    changeViewRes (id, value) {
      this.scene.view.horPixels = parseFloat(value)
      this.scene.view.verPixels = parseInt(parseFloat(value)*(9/16))
      this.sendScene();
    },
    changeSaveScene (id, value) {
      var tempScenes = [];
      switch (id) {
        case ("saveScene"):
          if (localStorage.savedScenes){
            tempScenes = JSON.parse(localStorage.savedScenes);
            tempScenes[value] = this.scene;
            localStorage.savedScenes = JSON.stringify(tempScenes);
          }
          else {
            localStorage.savedScenes = JSON.stringify([this.scene]);
          }
          break;
        case ("loadScene"):
          if (localStorage.savedScenes){
            tempScenes = JSON.parse(localStorage.savedScenes);
            if (tempScenes.length > value && tempScenes[value] != null){
              this.scene = tempScenes[value];
              this.sendScene();
            }
          }
          break;
        case ("sceneNum"):
            if (localStorage.savedScenes){
            tempScenes = JSON.parse(localStorage.savedScenes);
            if (tempScenes.length > value && tempScenes[value] != null){
              this.sceneNumText = "Scene " + value + " has been set, be carefull when overwriting.";
            }
            else {
              this.sceneNumText = "Scene " + value + " has not been set yet.";
            }
            break;
          }
        }
}}});

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

function removeFromArray (array, toRemove) {
  for (var i=0; i < array.length; i++) {
    if (array[i] == toRemove){
      return array.splice(i,1);
    }
  }
  return array;
}

function addToArray (array, toAdd) {
  var canAdd = true;
  for (var i=0; i < array.length; i++) {
    if (array[i] == toAdd){
      canAdd = false;
      break;
    }
  }
  if (canAdd){
    array.push(toAdd);
  }
  return array;
}