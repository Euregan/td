import { Elm } from "./Main.elm";
import model from "url:../static/snow_woodStructure_high.glb";

Elm.Main.init({ node: document.getElementById("root"), flags: model });
