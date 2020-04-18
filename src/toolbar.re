open Reprocessing;

let toolbarHeight = 200.0;
let margin = 20.0;

let drawBackground = env => {
  Draw.fill(Utils.color(~r=210, ~g=210, ~b=210, ~a=255), env);
  let width = float_of_int(Env.width(env));
  let height = float_of_int(Env.height(env));
  let x = 0.0;
  let y = height -. toolbarHeight;
  Draw.rectf(~pos=(x, y), ~width, ~height, env);
  y;
};

let drawInventory = (inventory, env) => {
  let backgroundY = drawBackground(env);
  let size = toolbarHeight /. 2.0 -. 2.0 *. margin;

  Draw.fill(Utils.color(~r=20, ~g=160, ~b=20, ~a=255), env);
  Draw.rectf(
    ~pos=(margin, margin +. backgroundY),
    ~width=size,
    ~height=size,
    env,
  );

  Draw.fill(Utils.color(~r=20, ~g=20, ~b=160, ~a=255), env);
  let x = margin *. 2.0 +. size;
  Draw.rectf(
    ~pos=(x, margin +. backgroundY),
    ~width=size,
    ~height=size,
    env,
  );

  List.iteri((i, item) => {() // TODO: draw inventory
}, inventory);
};

let drawControls = env => {
  let y = drawBackground(env);
  ();
  // Draw the inventory.
};
