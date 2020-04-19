open Common;
open Reprocessing;

let setup = (spriteData, env): Common.state => {
  let fontPath = "assets/font/PTSans-Regular.ttf.fnt";
  let spritesheetLocation = "assets/sprites/spritesheet.png";

  Env.size(~width=600, ~height=600, env);
  {
    hooks: Hooks.empty,
    mouse: {
      down: false,
      up: false,
      pressed: false,
      pos: Point.fromPair(Env.mouse(env)),
    },
    spriteData: Sprite.create(
        Draw.loadImage(~filename=spritesheetLocation, env),
        spriteData,
      ),
  };
};

let drawTile = (kind, {x, y}: Point.Float.t, spriteData, env) => {
  switch (kind) {
  | Floor(kind, obj) =>
    switch (kind) {
    | Regular => Draw.fill(Utils.color(~r=35, ~g=112, ~b=166, ~a=255), env)
    | FilledPit(_) =>
      Draw.fill(Utils.color(~r=35, ~g=112, ~b=166, ~a=255), env)
    };
    Draw.rectf(
      ~pos=(x, y),
      ~width=tileSizef,
      ~height=tileSizef,
      env,
    );
    switch (obj) {
    | Player(_, _, _) =>
      Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
      let halfTileSize = tileSizef /. 2.;
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
      let halfTileSize = tileSizef /. 2.;
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
      ~width=tileSizef,
      ~height=tileSizef,
      env,
    );
  | Wall =>
    Draw.fill(Utils.color(~r=100, ~g=100, ~b=100, ~a=255), env);
    Draw.rectf(
      ~pos=(x, y),
      ~width=tileSizef,
      ~height=tileSizef,
      env,
    );
  };
};

let getInventoryTopLeft = env => {
  let height = float_of_int(Env.height(env));
  let x = 0.0;
  let backgroundY = height -. toolbarHeight;
  let xOffset = btnMargin *. 3.0 +. btnSize *. 2.0;
  let yOffset = backgroundY +. btnMargin;
  Point.create(xOffset, yOffset);
};

let getHoveredInventoryIndex = (mousePos, env) => {
  let inventoryTopLeft = getInventoryTopLeft(env);
  let relativePos = Point.Float.(ofIntPt(mousePos) - inventoryTopLeft);
  let {x, y}: Point.Int.t =
    Point.Int.ofFloatPt(
      Point.Float.(relativePos /@ (tileSizef +. btnMargin)),
    );
  x + y * toolbarItemRowLen;
};

let drawInventory = (inventory, spriteData, env) => {
  let topleft = getInventoryTopLeft(env);
  List.iteri(
    (i, item) => {
      let x =
        float_of_int(i mod toolbarItemRowLen)
        *. (tileSizef +. btnMargin);
      let y =
        (tileSizef +. btnMargin)
        *. float_of_int(i / toolbarItemRowLen);
      let relativePos = Point.create(x, y);
      if (i == getHoveredInventoryIndex(Point.fromPair(Env.mouse(env)), env)) {
        drawTile(Pit, Point.Float.add(topleft, relativePos), spriteData, env);
      } else {
        drawTile(item, Point.Float.add(topleft, relativePos), spriteData, env);
      };
    },
    inventory,
  );
};

let drawToolbar = (inventory, spriteData, env) => {
  Draw.fill(Utils.color(~r=210, ~g=210, ~b=230, ~a=255), env);
  let width = float_of_int(Env.width(env));
  let height = float_of_int(Env.height(env));
  let x = 0.0;
  let backgroundY = height -. toolbarHeight;
  Draw.rectf(~pos=(x, backgroundY), ~width, ~height, env);

  Draw.fill(Utils.color(~r=20, ~g=160, ~b=20, ~a=255), env);
  Draw.rectf(
    ~pos=(btnMargin, btnMargin +. backgroundY),
    ~width=btnSize,
    ~height=btnSize,
    env,
  );

  Draw.fill(Utils.color(~r=20, ~g=20, ~b=160, ~a=255), env);
  let x = btnMargin *. 2.0 +. btnSize;
  Draw.rectf(
    ~pos=(x, btnMargin +. backgroundY),
    ~width=btnSize,
    ~height=btnSize,
    env,
  );

  drawInventory(inventory, spriteData, env);
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

let drawMessage = (message, env) => {
  let y = (Env.height(env) - int_of_float(toolbarHeight)) / 2;
  let textWidth = Draw.textWidth(~body=message, env);
  let x = (Env.width(env) - textWidth) / 2;
  Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=100), env);
  Draw.rectf(
    ~pos=(0.0, 0.0),
    ~width=float_of_int(Env.width(env)),
    ~height=float_of_int(Env.height(env)),
    env,
  );
  Draw.text(
    ~body=message,
    ~pos=(x, y),
    env,
  );
}

let drawMap = (map, spriteData, env) => {
  List.iteri(
    (y, row) => {
      List.iteri(
        (x, tile) => {
          let p = Point.Int.create(x, y);
          drawTile(tile, Point.Float.(ofIntPt(p) *@ tileSizef), spriteData, env);
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
    drawMessage("You WON the whole game", env);
  | ([first, ...rest], Intro) =>
    drawMessage("Welcome", env);
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
    drawMap(levelCurrentState.map, state.spriteData, env);
    drawInventory(levelCurrentState.items, state.spriteData, env);
  | (
      [levelInitialState, ...restOfLevels],
      RunningLevel([levelCurrentState, ...pastLevelStates]),
    ) =>
    let (lastTickTime, setLastTickTime) = Hooks.useState(__LOC__, 0.0);
    let deltaTime = Env.deltaTime(env) *. 1000.0;
    if (Env.keyPressed(R, env)) {
      setGameState(PreparingLevel(levelInitialState));
      setLastTickTime(0.0);
    };
    if (lastTickTime^ > tickTimeMS) {
      switch (tick(levelCurrentState.map)) {
      | Move(level) =>
        setLastTickTime(0.0);
        setGameState(
          RunningLevel([
            {...levelCurrentState, map: level},
            levelCurrentState,
            ...pastLevelStates,
          ]),
        );
      | Win =>
        setLevels(restOfLevels);
        setGameState(WinLevel(levelCurrentState));
        setLastTickTime(0.0);
      | Lose =>
        setGameState(
          LoseLevel(
            List.nth(pastLevelStates, List.length(pastLevelStates) - 1),
          ),
        );
        setLastTickTime(0.0);
      };
    } else {
      setLastTickTime(lastTickTime^ +. deltaTime);
    };
    drawMap(levelCurrentState.map, state.spriteData, env);
    drawToolbar([], state.spriteData, env); // TODO: Any items?
  | ([nextLevel, ..._], WinLevel(level)) =>
    if (Env.keyPressed(Space, env)) {
      setGameState(PreparingLevel(nextLevel));
    };
    drawMap(level.map, spriteData, env);
    drawMessage("He's safe and sound.", env)
  | ([initialLevel, ..._], LoseLevel(prepLevelState)) =>
    drawMap(prepLevelState.map, env);
    drawToolbar([], env);
    let (loseTimer, setLoseTimer) = Hooks.useState(__LOC__, loseMsgTimeMS);
    let deltaTime = Env.deltaTime(env) *. 1000.0;
    if (loseTimer^ < 0.0) {
      setLoseTimer(loseMsgTimeMS);
      setGameState(PreparingLevel(prepLevelState));
    };
    setLoseTimer(loseTimer^ -. deltaTime);
    drawMessage("Gosh, keep him ALIVE this time will ya?", env)
  };

  {
    ...state,
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

Assets.loadSpriteSheet("assets/sprites/spritesheet.json", (assets) => 
  run(~setup=setup(assets), ~draw, ~mouseDown, ~mouseUp, ())
);

