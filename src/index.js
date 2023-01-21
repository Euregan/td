import { Elm } from "./Main.elm";
import tile from "url:../static/tile.glb";
import spawn from "url:../static/tile_endRoundSpawn.glb";
import straight from "url:../static/tile_straight.glb";
import corner from "url:../static/tile_cornerRound.glb";
import end from "url:../static/tile_endRound.glb";
import orcTower from "url:../static/towerRound_base.glb";

Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    viewport: {
      width: window.innerWidth,
      height: window.innerHeight,
    },
    models: {
      tile,
      spawn,
      straight,
      corner,
      end,
      orcTower,
    },
    seed: Math.round(Math.random() * 1000000),
  },
});
