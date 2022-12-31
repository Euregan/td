import { Elm } from "./Main.elm";
import tile from "url:../static/tile.glb";
import spawn from "url:../static/tile_endRoundSpawn.glb";
import straight from "url:../static/tile_straight.glb";
import corner from "url:../static/tile_cornerRound.glb";
import end from "url:../static/tile_endRound.glb";

Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    tile,
    spawn,
    straight,
    corner,
    end,
  },
});
