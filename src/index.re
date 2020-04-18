open Common;
open Reprocessing;

let setup = (env): Common.state => {
  Env.size(~width=600, ~height=600, env);
  {
    hooks: Hooks.empty,
    mouse: {
      down: false,
      up: false,
      pressed: false,
      pos: Point.fromPair(Env.mouse(env)),
    },
  };
};

let drawTile = (kind, x, y, env) => {
  switch (kind) {
  | Floor(kind, obj) =>
    switch (kind) {
    | Regular => Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env)
    | FilledPit(_) =>
      Draw.fill(Utils.color(~r=35, ~g=112, ~b=166, ~a=255), env)
    };
    Draw.rectf(
      ~pos=(x, y),
      ~width=Common.tileSizef,
      ~height=Common.tileSizef,
      env,
    );
    switch (obj) {
    | Player(_, _, _) =>
      Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
      let halfTileSize = Common.tileSizef /. 2.;
      Draw.ellipsef(
        ~center=(x +. halfTileSize, y +. halfTileSize),
        ~radx=halfTileSize,
        ~rady=halfTileSize,
        env,
      );
    | Boulder(_, health) =>
      switch (health) {
      | Hard => Draw.fill(Utils.color(~r=100, ~g=100, ~b=100, ~a=255), env)
      | Cracked => Draw.fill(Utils.color(~r=100, ~g=50, ~b=50, ~a=255), env)
      };
      let halfTileSize = Common.tileSizef /. 2.;
      Draw.ellipsef(
        ~center=(x +. halfTileSize, y +. halfTileSize),
        ~radx=halfTileSize,
        ~rady=halfTileSize,
        env,
      );
    | Empty => ()
    };
  | Pit =>
    Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
    Draw.rectf(
      ~pos=(x, y),
      ~width=Common.tileSizef,
      ~height=Common.tileSizef,
      env,
    );
  | Wall =>
    Draw.fill(Utils.color(~r=100, ~g=100, ~b=100, ~a=255), env);
    Draw.rectf(
      ~pos=(x, y),
      ~width=Common.tileSizef,
      ~height=Common.tileSizef,
      env,
    );
  };
};

let drawInventoryBackground = env => {
  Draw.fill(Utils.color(~r=210, ~g=210, ~b=230, ~a=255), env);
  let width = float_of_int(Env.width(env));
  let height = float_of_int(Env.height(env));
  let x = 0.0;
  let y = height -. Common.toolbarHeight;
  Draw.rectf(~pos=(x, y), ~width, ~height, env);
  y;
};

let drawInventory = (inventory, env) => {
  let backgroundY = drawInventoryBackground(env);
  let btnSize = Common.toolbarHeight /. 2.0 -. 2.0 *. Common.btnMargin;

  Draw.fill(Utils.color(~r=20, ~g=160, ~b=20, ~a=255), env);
  Draw.rectf(
    ~pos=(Common.btnMargin, Common.btnMargin +. backgroundY),
    ~width=btnSize,
    ~height=btnSize,
    env,
  );

  Draw.fill(Utils.color(~r=20, ~g=20, ~b=160, ~a=255), env);
  let x = Common.btnMargin *. 2.0 +. btnSize;
  Draw.rectf(
    ~pos=(x, Common.btnMargin +. backgroundY),
    ~width=btnSize,
    ~height=btnSize,
    env,
  );

  // let width = float_of_int();
  let xOffset = Common.btnMargin *. 3.0 +. btnSize *. 2.0

  List.iteri((i, item) => {
    let x = (xOffset +. float_of_int(i) *. (Common.tileSizef +. Common.btnMargin)) mod_float Env.width(env);
    let y = backgroundY +. (Common.tileSizef +. Common.btnMargin) *. float_of_int(i mod Common.toolbarItemRowLen);
    drawTile(item, x, y, env)
  }, inventory);
};

let drawControls = env => {
  let y = drawInventoryBackground(env);
  ();
  // Draw the inventory.
};

let getLevelTile = (level, {x, y}: Point.Int.t) => {
  switch (List.nth_opt(level, y)) {
  | None => Wall
  | Some(row) =>
    switch (List.nth_opt(row, x)) {
    | None => Wall
    | Some(tile) => tile
    }
  };
};

let setLevelTile = (level, {x, y}: Point.Int.t, newTile) => {
  List.mapi(
    (y2, row) => {
      List.mapi((x2, tile) => x == x2 && y == y2 ? newTile : tile, row)
    },
    level,
  );
};

let updateLevelTile = (level, {x, y}: Point.Int.t, update) => {
  List.mapi(
    (y2, row) => {
      List.mapi((x2, tile) => x == x2 && y == y2 ? update(tile) : tile, row)
    },
    level,
  );
};

let facingToDelta = facing =>
  switch (facing) {
  | Up => Point.create(0, -1)
  | Right => Point.create(1, 0)
  | Down => Point.create(0, 1)
  | Left => Point.create(-1, 0)
  };

let turnFacing = (facing, move) => {
  switch (move) {
  | Forward => facing
  | TurnLeft =>
    switch (facing) {
    | Up => Left
    | Right => Up
    | Down => Right
    | Left => Down
    }
  | TurnRight =>
    switch (facing) {
    | Up => Right
    | Right => Down
    | Down => Left
    | Left => Up
    }
  };
};

let rec resolveMove = (level, pos, moveDelta, preResolved) => {
  let secondPos = Point.Int.add(pos, moveDelta);
  let replaceWith = (level, t1, t2) =>
    Move(setLevelTile(setLevelTile(level, pos, t1), secondPos, t2));

  let retryResolveMove = level => resolveMove(level, pos, moveDelta, true);

  let resolveMove = (level, pos, moveDelta) =>
    !preResolved ? resolveMove(level, pos, moveDelta, false) : Move(level);

  switch (getLevelTile(level, pos), getLevelTile(level, secondPos)) {
  | (Wall | Pit | Floor(_, Empty), _) => Move(level)
  | (Floor(k1, Boulder(id, health)), Wall) => Move(level)
  | (_, Floor(_, Player(_))) => Lose
  | (Floor(k1, Boulder(id, health)), Floor(k2, Empty)) =>
    replaceWith(level, Floor(k1, Empty), Floor(k2, Boulder(id, health)))
  | (Floor(k1, Boulder(id, health)), Pit) =>
    replaceWith(level, Floor(k1, Empty), Floor(FilledPit(id), Empty))
  | (Floor(k1, Player(_) as p), Floor(k2, Empty)) =>
    replaceWith(level, Floor(k1, Empty), Floor(k2, p))
  | (Floor(_, Player(_)), Wall)
  | (Floor(_, Player(_)), Pit) => Lose
  | (Floor(k1, Player(_) as p), Floor(k2, Boulder(_, Cracked)))
      when preResolved =>
    replaceWith(level, Floor(k1, p), Floor(k2, Empty))
  | (Floor(k1, Player(_) as p), Floor(k2, Boulder(id, Hard)))
      when preResolved =>
    replaceWith(level, Floor(k1, p), Floor(k2, Boulder(id, Cracked)))
  | (Floor(k1, Player(_) | Boulder(_, _)), Floor(k2, Boulder(_))) =>
    switch (resolveMove(level, secondPos, moveDelta)) {
    | Move(level) => retryResolveMove(level)
    | other => other
    }
  };
};

type agentInfo =
  | AgentWin
  | Push(facing, Point.Int.t);

// TODO: tick needs to return a win or lose state that's compatible with animation
let tick = level => {
  let fold_righti = (f, a, l) =>
    snd(
      List.fold_right(
        (el, (i, a)) => (i - 1, f(i, a, el)),
        l,
        (List.length(l) - 1, a),
      ),
    );

  let (movingAgents, level) =
    fold_righti(
      (y, (agents, rows), row) => {
        let (agents, newRow) =
          fold_righti(
            (x, (agents, row), tile) =>
              switch (tile) {
              | Floor(k, Player(id, facing, [])) => (
                  [AgentWin, ...agents],
                  [Floor(k, Player(id, facing, [])), ...row],
                )
              | Floor(k, Player(id, facing, [Forward, ...moves])) => (
                  [Push(facing, Point.create(x, y)), ...agents],
                  [Floor(k, Player(id, facing, moves)), ...row],
                )
              | Floor(k, Player(id, facing, [turn, ...moves])) => (
                  agents,
                  [
                    Floor(k, Player(id, turnFacing(facing, turn), moves)),
                    ...row,
                  ],
                )
              | _ => (agents, [tile, ...row])
              },
            (agents, []),
            row,
          );
        (agents, [newRow, ...rows]);
      },
      ([], []),
      level,
    );

  // TODO: Optionally sort the agents
  List.fold_left(
    (level, agent) =>
      switch (level, agent) {
      | (Move(level), Push(facing, pos)) =>
        Point.Int.print(pos);
        resolveMove(level, pos, facingToDelta(facing), false);
      | (_, AgentWin)
      | (Win, _) => Win
      | (Lose, _) => Lose // TODO: Might still want to resolve the rest of the moves..
      },
    Move(level),
    movingAgents,
  );
};

let drawMap = (map, env) => {
  List.iteri(
    (y, row) => {
      List.iteri(
        (x, tile) => {
          drawTile(
            tile,
            float_of_int(x) *. Common.tileSizef,
            float_of_int(y) *. Common.tileSizef,
            env,
          )
        },
        row,
      )
    },
    map,
  );
};

let draw = (state, env) => {
  Hooks.initialize(state.hooks);
  let (levels, setLevels) = Hooks.useState(__LOC__, Levels.all);
  let (gameState, setGameState) = Hooks.useState(__LOC__, Intro);

  if (Env.keyPressed(T, env)) {
    setLevels(Levels.all);
    setGameState(Intro);
  };

  Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);

  switch (levels^, gameState^) {
  | ([], _) =>
    Draw.text(~body="You WON the whole game", ~pos=(100, 100), env)
  | ([first, ...rest], Intro) =>
    Draw.text(~body="Welcome", ~pos=(100, 100), env);
    if (Env.keyPressed(Space, env)) {
      setGameState(PreparingLevel(first));
    };
  | (
      [levelInitialState, ...restOfLevels],
      PreparingLevel(levelCurrentState),
    ) =>
    if (Env.keyPressed(R, env)) {
      setGameState(PreparingLevel(levelInitialState));
    };
    if (Env.keyPressed(Space, env)) {
      setGameState(RunningLevel([levelCurrentState]));
    };
    drawMap(levelCurrentState.map, env);
    drawInventory(levelCurrentState.items, env);
  | (
      [levelInitialState, ...restOfLevels],
      RunningLevel([levelCurrentState, ...pastLevelStates]),
    ) =>
    if (Env.keyPressed(R, env)) {
      setGameState(PreparingLevel(levelInitialState));
    };
    if (Env.keyPressed(Space, env)) {
      switch (tick(levelCurrentState.map)) {
      | Move(level) =>
        setGameState(
          RunningLevel([
            {...levelCurrentState, map: level},
            levelCurrentState,
            ...pastLevelStates,
          ]),
        )
      | Win =>
        setLevels(restOfLevels);
        setGameState(WinLevel(levelCurrentState));
      | Lose => setGameState(LoseLevel)
      };
    };
    drawMap(levelCurrentState.map, env);
    drawControls(env);
  | ([nextLevel, ..._], WinLevel(level)) =>
    if (Env.keyPressed(Space, env)) {
      setGameState(PreparingLevel(nextLevel));
    };
    drawMap(level.map, env);
    Draw.text(~body="You WIN", ~pos=(100, 100), env);
  | ([initialLevel, ..._], LoseLevel) =>
    if (Env.keyPressed(Space, env)) {
      setGameState(PreparingLevel(initialLevel));
    };
    Draw.text(~body="You LOSE", ~pos=(100, 100), env);
  };

  {
    hooks: Hooks.finalize(),
    mouse: {
      ...state.mouse,
      down: false,
      up: false,
    },
  };
};

let mouseDown = (state, _) => {
  ...state,
  mouse: {
    ...state.mouse,
    down: true,
    up: false,
    pressed: true,
  },
};
let mouseUp = (state, _) => {
  ...state,
  mouse: {
    ...state.mouse,
    down: false,
    up: true,
    pressed: false,
  },
};

run(~setup, ~draw, ~mouseDown, ~mouseUp, ());
