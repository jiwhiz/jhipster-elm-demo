import './toasty-default.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import { bind as bindStorage } from "./localStorage.js";

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    currentTime: Date.now(),
    winSize: {
      width: window.innerWidth,
      height: window.innerHeight
    },
    jwtToken: localStorage.getItem("jhi-authenticationToken")
  }
});

registerServiceWorker();

// LocalStorage

bindStorage(app);