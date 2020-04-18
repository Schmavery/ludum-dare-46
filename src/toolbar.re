open Reprocessing;

let toolbarHeight = 100.0;

let drawBackground = (env) => {
  Draw.fill(Utils.color(~r=180, ~g=180, ~b=180, ~a=255), env);
  let width = float_of_int(Env.width(env));
  let height = float_of_int(Env.height(env));
  let x = 0.0
  let y = height -. toolbarHeight;
  Draw.rectf(~pos=(x, y), ~width, ~height, env);
}

let drawInventory = (inventory, env) => {
  drawBackground(env)
  // Draw the inventory.
}

let drawControls = (env) => {
  drawBackground(env)
  // Draw the inventory.
}
