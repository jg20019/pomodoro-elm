importScripts("timer.js");

const timer = Elm.Timer.init();
timer.ports.sendTick.subscribe(result => {
    postMessage('tick');
});
  
/*
const timer = Elm.Timer.init();
timer.ports.sendTick.subscribe(result => {
    app.ports.receiveTick.send("")
});
*/
