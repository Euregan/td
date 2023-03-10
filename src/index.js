import { Elm } from "./Main.elm";
import tile from "url:../static/tile.glb";
import spawn from "url:../static/tile_endRoundSpawn.glb";
import straight from "url:../static/tile_straight.glb";
import corner from "url:../static/tile_cornerRound.glb";
import end from "url:../static/tile_endRound.glb";
import orcTowerBase from "url:../static/towerRound_base.glb";
import orcTowerBottom from "url:../static/towerRound_bottomA.glb";
import orcTowerMiddle from "url:../static/towerRound_middleA.glb";
import orcTowerTop from "url:../static/towerRound_topB.glb";
import characterSkeleton from "url:../static/characterSkeleton.glb";

const app = Elm.Main.init({
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
      orcTowerBase,
      orcTowerBottom,
      orcTowerMiddle,
      orcTowerTop,
      characterSkeleton,
    },
    seed: Math.round(Math.random() * 1000000),
  },
});

addEventListener("wheel", (event) => {
  app.ports.onWheel.send(event.wheelDeltaY);
});
