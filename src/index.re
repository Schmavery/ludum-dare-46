open Reprocessing;

type id = int;
type move = TurnRight | Forward | TurnLeft;
type facing = Up | Down | Left | Right;
type obj = Player(id, facing, list(move)) | Boulder(id) | Empty;
type floorKind = Regular | FilledPit(id);
type tile = Wall | Floor(floorKind, obj) | Pit ; 
type map = list(list(tile));
type tick = Win | Lose | Move(map);

let tileSizef = 50.;

let id = {
  let counter = ref(0);
  () => {
    incr(counter);
    counter^
  }
};

let level = [
    [ Floor(Regular, Empty),  Floor(Regular, Empty), Floor(Regular, Empty), Floor(Regular, Empty), Floor(Regular, Empty) ],
    [ Floor(Regular, Player(id(), Right, [Forward, Forward, Forward, Forward])), Floor(Regular, Empty), Floor(Regular, Boulder(id())), Pit, Floor(Regular, Empty) ],
    [ Floor(Regular, Empty),  Floor(Regular, Empty), Floor(Regular, Empty), Floor(Regular, Empty), Floor(Regular, Empty) ],
  ];

let setup = (env) => {
  Env.size(~width=600, ~height=600, env);
  Hooks.empty;
}

let drawTile = (kind, x, y, env) => { switch (kind) {
    | Floor(kind, obj) =>
      switch (kind) {
        | Regular =>
          Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
        | FilledPit(_) => 
          Draw.fill(Utils.color(~r=35, ~g=112, ~b=166, ~a=255), env);
      };
      Draw.rectf(~pos=(x, y), ~width=tileSizef, ~height=tileSizef, env)
      switch (obj) { 
        | Player(_, _, _) => 
          Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
          let halfTileSize = tileSizef /. 2.;
          Draw.ellipsef(~center=(x +. halfTileSize, y +. halfTileSize), ~radx=halfTileSize, ~rady=halfTileSize, env)
        | Boulder(_) => 
          Draw.fill(Utils.color(~r=100, ~g=100, ~b=100, ~a=255), env);
          let halfTileSize = tileSizef /. 2.;
          Draw.ellipsef(~center=(x +. halfTileSize, y +. halfTileSize), ~radx=halfTileSize, ~rady=halfTileSize, env)
        | Empty => ()
      };
    | Pit =>
      Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
      Draw.rectf(~pos=(x, y), ~width=tileSizef, ~height=tileSizef, env)
  };
};

let getLevelTile = (level, {x, y}: Point.Int.t) => {
  switch (List.nth_opt(level, y)) {
    | None => Wall
    | Some(row) => switch (List.nth_opt(row, x)){
      | None => Wall
      | Some(tile) => tile
    }
  }
};

let setLevelTile = (level, {x, y}: Point.Int.t, newTile) => {
  List.mapi((y2, row) => {
    List.mapi((x2, tile) => x == x2 && y == y2 ? newTile : tile, row)
  }, level)
};

let updateLevelTile = (level, {x, y}: Point.Int.t, update) => {
  List.mapi((y2, row) => {
    List.mapi((x2, tile) => x == x2 && y == y2 ? update(tile) : tile, row)
  }, level)
};

let facingToDelta = (facing) => switch (facing) {
  | Right => Point.create(1, 0);
  // TODO(david): Finish the cases
  | _ => Point.Int.zero
};

// TODO(david): Write turnFacing(facing, move)
// ex: (Right, TurnRight) -> Down
let turnFacing = (facing, move) => Down;

let rec resolveMove = (level, pos, moveDelta) =>  {
  let secondPos = Point.Int.add(pos, moveDelta);
  let replaceWith = (level, t1, t2) => 
    Move(setLevelTile(setLevelTile(level, pos, t1), secondPos, t2));

  switch (getLevelTile(level, pos), getLevelTile(level, secondPos)) {
    | (Wall | Pit | Floor(_, Empty), _) => Move(level)
    | (Floor(k1, Boulder(id)), Wall) => Move(level) // TODO: crushed boulders
    | (_, Floor(_, Player(_))) => Move(level) // TODO: What happens if something gets pushed into the player?
    | (Floor(k1, Boulder(id)), Floor(k2, Empty)) => replaceWith(level, Floor(k1, Empty), Floor(k2, Boulder(id)))
    | (Floor(k1, Boulder(id)), Pit) => replaceWith(level, Floor(k1, Empty), Floor(FilledPit(id), Empty))
    | (Floor(k1, Player(_) as p), Floor(k2, Empty)) => 
      replaceWith(level, Floor(k1, Empty), Floor(k2, p))
    | (Floor(_, Player(_)), Wall)
    | (Floor(_, Player(_)), Pit) => Lose
    | (Floor(k1, Player(_) as p), Floor(k2, Boulder(_))) =>
      // TODO: Handle win/lose in the recursive case
      switch (resolveMove(level, secondPos, moveDelta)) {
        | Move(level) =>
          switch (getLevelTile(level, secondPos)){
            | Floor(k2, Empty) => replaceWith(level, Floor(k1, Empty), Floor(k2, p))
            | Pit | Wall => Lose
            | _ => Move(level) // TODO: crushed boulders? Other cases? Many of these cases can't happen...
          }
        | other => other
      }
    | (Floor(_, Player(_)), _) => 
      print_endline("Failed to move player, go fill in some more cases");
      Move(level)
    | (Floor(k1, Boulder(id)), Floor(k2, Boulder(_))) => 
      switch (resolveMove(level, secondPos, moveDelta)) {
        | Move(level) =>
          // TODO I kinda want to just call resolveMove again cause this is
          // repetitive.........
          switch (getLevelTile(level, secondPos)){
            | Floor(k2, Empty) => replaceWith(level, Floor(k1, Empty), Floor(k2, Boulder(id)))
            | Pit => replaceWith(level, Floor(k1, Empty), Floor(FilledPit(id), Empty))
            | _ => Move(level) // TODO: crushed boulders? Other cases?
          }
        | other => other
      }
}};


type agentInfo = 
| AgentWin
| Push(facing, Point.Int.t);

// TODO: tick needs to return a win or lose state that's compatible with animation
let tick = (level) => {
  // Find all 'players'
  // Move them all one move
  // TODO actually get this from the level:
  /* let playerPos = Point.Int.create(0, 1); */
  /* let playerFacing = Right; */
  /* let playerMoves = [Forward, Forward]; */

  /* let rec findi = (p, l) => switch (l) { */
  /*   | [] => None */
  /*   | [hd, ...tl] when p(hd) => Some(p) */
  /*   | [_, ...tl] => findi(p, tl) */
  /* }; */

  let fold_righti = (f, a, l) => snd(List.fold_right((el, (i, a)) => (i - 1, f(i, a, el)), l, (List.length(l) - 1, a)));

  let (movingAgents, level) = 
    fold_righti((y, (agents, rows), row) => {
      let (agents, newRow) = fold_righti((x, (agents, row), tile) => switch (tile) {
        | Floor(k, Player(id, facing, [])) => ([AgentWin, ...agents], [Floor(k, Player(id, facing, [])), ...row])
        | Floor(k, Player(id, facing, [Forward, ...moves])) => ([Push(facing, Point.create(x, y)), ...agents], [Floor(k, Player(id, facing, moves)), ...row])
        | Floor(k, Player(id, facing, [turn, ...moves])) => (agents, [Floor(k, Player(id, turnFacing(facing, turn), moves)), ...row])
        | _ => (agents, [tile, ...row])
      }, (agents, []), row);
      (agents, [newRow, ...rows])
    }, ([], []), level);

  // TODO: Optionally sort the agents
  List.fold_left(
    (level, agent) => switch (level, agent) {
      | (Move(level), Push(facing, pos)) => 
      Point.Int.print(pos);
      resolveMove(level, pos, facingToDelta(facing));
      | (_, AgentWin) | (Win, _) => Win
      | (Lose, _) => Lose // TODO: Might still want to resolve the rest of the moves..
    },
    Move(level),
    movingAgents  
  );
  
  /* switch (playerMoves) { */
  /*   | [] => Win */
  /*   | [Forward, ...rest] => */ 
  /*     // TODO: This is kinda wrong cause the player might have been moved? */
  /*     // Ideally when we find the player we remove their first move and store it */
  /*     // here I guess. Something like that. */
  /*     let level = updateLevelTile(level, playerPos, (Floor(k, Player(id, f, _))) => Floor(k, Player(id, f, rest))); */
  /*     resolveMove(level, playerPos, facingToDelta(playerFacing)); */
  /*   | [move, ...rest] => */
  /*     Move(updateLevelTile(level, playerPos, (Floor(k, Player(id, f, _))) => Floor(k, Player(id, turnFacing(f, move), rest)))); */
  /* }; */

};

let draw = (state, env) => {
  Hooks.initialize(state);
  let (levelStack, setLevelStack) = Hooks.useState(__LOC__, [level]);
  let currentLevel = List.hd(levelStack^);

  if (Env.keyPressed(R, env)){
    print_endline("Reset");
    setLevelStack([level])
  };

  if (Env.keyPressed(Space, env)){
    print_endline("Moving");
    switch (tick(currentLevel)){
      | Move(level) => setLevelStack([level, ...levelStack^])
      | Win => print_endline("You won")
      | Lose => print_endline("You lost")
    }
  };
  Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);

  List.iteri((y, row) => {
    List.iteri((x, tile) => {
      drawTile(tile, float_of_int(x) *. tileSizef, float_of_int(y) *. tileSizef, env);
    }, row);
  }, currentLevel);

  Hooks.finalize();
};

run(~setup, ~draw, ());
