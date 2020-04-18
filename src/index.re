open Reprocessing;

type moves = TurnRight | Forward | TurnLeft;
type obj = Player(list(moves)) | Box | Empty;
type floorKind = Regular | FilledPit;
type tile = Wall | Floor(floorKind, obj) | Pit ; 

let tileSizef = 50.;

/* type level = { */
/*   map: array(array(tile)), */
  /* startPos: (int, int), */
  /* movement: list(moves) */
/* }; */

let level = [
    [ Floor(Regular, Empty),  Floor(Regular, Empty), Floor(Regular, Empty), Floor(Regular, Empty), Floor(Regular, Empty) ],
    [ Floor(Regular, Player([Forward])), Floor(Regular, Empty), Floor(Regular, Empty), Pit, Floor(Regular, Empty) ],
    [ Floor(Regular, Empty),  Floor(Regular, Empty), Floor(Regular, Empty), Floor(Regular, Empty), Floor(Regular, Empty) ],
  ];

let setup = (env) => {
  Env.size(~width=600, ~height=600, env);
  Hooks.empty;
}

let drawTile = (kind, x, y, env) => {
  switch (kind) {
    | Floor(_, Player(_)) =>
      Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
      Draw.rectf(~pos=(x, y), ~width=tileSizef, ~height=tileSizef, env)
      Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
      let halfTileSize = tileSizef /. 2.;
      Draw.ellipsef(~center=(x +. halfTileSize, y +. halfTileSize), ~radx=halfTileSize, ~rady=halfTileSize, env)
    | Floor(_, _) =>
      Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
      Draw.rectf(~pos=(x, y), ~width=tileSizef, ~height=tileSizef, env)
    | Pit =>
      Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
      Draw.rectf(~pos=(x, y), ~width=tileSizef, ~height=tileSizef, env)
  };
};

let draw = (state, env) => {
  Hooks.initialize(state);
  Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);

  List.iteri((y, row) => {
    List.iteri((x, cell) => {
      drawTile(cell, float_of_int(x) *. tileSizef, float_of_int(y) *. tileSizef, env);
    }, row);
  }, level);

  Hooks.finalize();
};

run(~setup, ~draw, ());
