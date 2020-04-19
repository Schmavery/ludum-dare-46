open Common;
open Reprocessing;

let editor = ref(false);

let setup = (spriteData, env): Common.state => {
  let fontPath = "assets/font/ptsans_regular_2x.fnt";
  let spritesheetLocation = "assets/sprites/spritesheet.png";
  Env.size(~width=1000, ~height=800, env);
  {
    hooks: Hooks.empty,
    mouse: {
      down: false,
      up: false,
      pressed: false,
    },
    spriteData:
      Sprite.create(
        Draw.loadImage(~filename=spritesheetLocation, env),
        spriteData,
      ),
    font: Draw.loadFont(~filename=fontPath, env),
  };
};

let drawObj = (~obj, ~pos as {Point.x, y}, ~spriteData, env) => {
  let halfTileSize = tileSizef /. 2.;
  let pos = Point.create(x +. halfTileSize, y +. halfTileSize);
  switch (obj) {
  | Player(_, facing, _) =>
    let assetName =
      switch (facing) {
      | Up => "guy_up"
      | Down => "guy_down"
      | Right => "guy_right"
      | Left => "guy_left"
      };
    Assets.drawSprite(spriteData, assetName, ~pos, env);
  | Boulder(_, health) =>
    switch (health) {
    | Hard => Assets.drawSprite(spriteData, "normal_boulder", ~pos, env)
    | Cracked => Assets.drawSprite(spriteData, "cracked_boulder", ~pos, env)
    }
  | Empty => ()
  };
};

let drawTile =
    (
      kind,
      {x, y}: Point.Float.t,
      ~noBackground=false,
      ~withObj=false,
      spriteData: Sprite.t,
      env,
    ) => {
  let halfTileSize = tileSizef /. 2.;
  let pos = Point.create(x +. halfTileSize, y +. halfTileSize);
  switch (kind) {
  | Floor(kind, obj) =>
    if (!noBackground || editor^) {
      switch (kind) {
      | Regular => Assets.drawSprite(spriteData, "floor", ~pos, env)
      // TODO: Differentiate the filled pit vs floor.
      | FilledPit(_) =>
        Assets.drawSprite(spriteData, "pit_with_boulder", ~pos, env)
      };
    };
    if (withObj) {
      drawObj(~obj, ~pos={x, y}, ~spriteData, env);
    };
  | Pit => Assets.drawSprite(spriteData, "pit", ~pos, env)
  | Wall => Assets.drawSprite(spriteData, "wall", ~pos, env)
  };
};

let getMapTile = (map, {x, y}: Point.Int.t) =>
  if (y < 0 || x < 0) {
    Wall;
  } else {
    switch (List.nth_opt(map, y)) {
    | None => Wall
    | Some(row) =>
      switch (List.nth_opt(row, x)) {
      | None => Wall
      | Some(tile) => tile
      }
    };
  };

let setMapTile = (map, {x, y}: Point.Int.t, newTile) => {
  List.mapi(
    (y2, row) => {
      List.mapi((x2, tile) => x == x2 && y == y2 ? newTile : tile, row)
    },
    map,
  );
};

let updateMapTile = (map, {x, y}: Point.Int.t, update) => {
  List.mapi(
    (y2, row) => {
      List.mapi((x2, tile) => x == x2 && y == y2 ? update(tile) : tile, row)
    },
    map,
  );
};

let getInventoryTopLeft = env => {
  let height = float_of_int(Env.height(env));
  let backgroundY = height -. toolbarHeight;
  let xOffset = btnMargin +. (btnMargin +. btnSize) *. 4.0;
  let yOffset = backgroundY +. btnMargin;
  Point.create(xOffset, yOffset);
};

let getMapTopLeft = (map, env) => {
  Point.x:
    Env.width(env)->float_of_int
    /. 2.
    -. List.length(List.hd(map))->float_of_int
    *. tileSizef
    /. 2.,
  y:
    (Env.height(env)->float_of_int -. toolbarHeight)
    /. 2.
    -. List.length(map)->float_of_int
    *. tileSizef
    /. 2.,
};

let getHoveredMapSquare = (map, env) => {
  let mousePt = Point.Float.ofIntPt(Point.fromPair(Env.mouse(env)));
  let topLeft = getMapTopLeft(map, env);
  let mapWidth = tileSizef *. float_of_int(List.length(List.hd(map)));
  let mapHeight = tileSizef *. float_of_int(List.length(map));
  let mapRect = Rect.fromPoints(topLeft, Point.create(mapWidth, mapHeight));

  if (Rect.containsPtf(mapRect, mousePt)) {
    let relativePos = Point.Float.(mousePt - topLeft);
    let {Point.x, y} as tilePos =
      Point.Int.ofFloatPt(Point.Float.(relativePos /@ tileSizef));
    switch (getMapTile(map, tilePos)) {
    | Floor(Regular, Empty) => Some((x, y))
    | _ when editor^ => Some((x, y))
    | _ => None
    };
  } else {
    None;
  };
};

let getHoveredInventoryIndex = env => {
  let mousePt = Point.Float.ofIntPt(Point.fromPair(Env.mouse(env)));
  let inventoryTopLeft = getInventoryTopLeft(env);

  let inventoryWidth =
    (btnMargin +. tileSizef) *. float_of_int(Common.toolbarItemRowLen);
  let inventoryHeight = (btnMargin +. tileSizef) *. 2.;
  let inventoryRect =
    Rect.fromPoints(
      inventoryTopLeft,
      Point.create(inventoryWidth, inventoryHeight),
    );

  if (Rect.containsPtf(inventoryRect, mousePt)) {
    let relativePos = Point.Float.(mousePt - inventoryTopLeft);
    let tileAndMargin = tileSizef +. btnMargin;
    let hoverOffset =
      Point.map(~f=v => mod_float(v, tileAndMargin), relativePos);
    let {x, y}: Point.Int.t =
      Point.Int.ofFloatPt(Point.Float.(relativePos /@ tileAndMargin));
    Some((x + y * toolbarItemRowLen, hoverOffset));
  } else {
    None;
  };
};

let drawInventory = (inventory, spriteData, hovered, env) => {
  let topleft = getInventoryTopLeft(env);
  List.iteri(
    (i, item) => {
      let x =
        float_of_int(i mod toolbarItemRowLen) *. (tileSizef +. btnMargin);
      let y = (tileSizef +. btnMargin) *. float_of_int(i / toolbarItemRowLen);
      let relativePos = Point.create(x, y);
      if (Some(i) == hovered) {
        Draw.tint(Utils.color(~r=255, ~g=255, ~b=255, ~a=100), env);
      };
      drawTile(
        item,
        ~noBackground=true,
        ~withObj=true,
        Point.Float.add(topleft, relativePos),
        spriteData,
        env,
      );
      if (Some(i) == hovered) {
        Draw.noTint(env);
      };
    },
    inventory,
  );
};

let drawToolbar = (inventory, spriteData, hovered, env) => {
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
  let x = btnMargin +. (btnMargin +. btnSize) *. 1.;
  Draw.rectf(
    ~pos=(x, btnMargin +. backgroundY),
    ~width=btnSize,
    ~height=btnSize,
    env,
  );

  Draw.fill(Utils.color(~r=20, ~g=20, ~b=160, ~a=255), env);
  let x = btnMargin +. (btnMargin +. btnSize) *. 2.;
  Draw.rectf(
    ~pos=(x, btnMargin +. backgroundY),
    ~width=btnSize,
    ~height=btnSize,
    env,
  );

  Draw.fill(Utils.color(~r=20, ~g=20, ~b=160, ~a=255), env);
  let x = btnMargin +. (btnMargin +. btnSize) *. 3.;
  Draw.rectf(
    ~pos=(x, btnMargin +. backgroundY),
    ~width=btnSize,
    ~height=btnSize,
    env,
  );

  drawInventory(inventory, spriteData, hovered, env);
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
    Move(setMapTile(setMapTile(level, pos, t1), secondPos, t2));

  let retryResolveMove = level =>
    !preResolved ? resolveMove(level, pos, moveDelta, true) : Move(level);

  let resolveMove = (level, pos, moveDelta) =>
    !preResolved ? resolveMove(level, pos, moveDelta, false) : Move(level);

  switch (getMapTile(level, pos), getMapTile(level, secondPos)) {
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

let drawMessage = (message, offset, font, ~withBackground=true, env) => {
  let y = (Env.height(env) - int_of_float(offset) - fontHeight) / 2;
  let textWidth = Draw.textWidth(~font, ~body=message, env);
  let x = (Env.width(env) - textWidth) / 2;
  if (withBackground) {
    Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=100), env);
    Draw.rectf(
      ~pos=(0.0, 0.0),
      ~width=float_of_int(Env.width(env)),
      ~height=float_of_int(Env.height(env)),
      env,
    );
  };
  Draw.tint(Utils.color(~r=0, ~g=0, ~b=0, ~a=190), env);
  Draw.text(~font, ~body=message, ~pos=(x, y), env);
  Draw.noTint(env);
};

let drawLines = (map, mapTopLeft, env) => {
  let halfTileSize = tileSizef /. 2.;
  let centerOffset = Point.create(halfTileSize, halfTileSize);

  List.iteri(
    (y, row) => {
      List.iteri(
        (x, tile) => {
          switch (tile) {
          | Floor(_, Player(id, facing, moves)) =>
            let (_, mapPositions) =
              List.fold_left(
                ((currFacing, acc), move) =>
                  switch (acc) {
                  | [prevPoint, ...rest] =>
                    switch (move) {
                    | Forward => (
                        currFacing,
                        [
                          Point.Int.add(facingToDelta(currFacing), prevPoint),
                          prevPoint,
                          ...rest,
                        ],
                      )
                    | turn => (
                        turnFacing(currFacing, turn),
                        [prevPoint, ...rest],
                      )
                    }
                  | _ => assert(false)
                  },
                (facing, [Point.(create(x, y))]),
                moves,
              );

            let screenPositions =
              List.map(
                p =>
                  Point.Float.(
                    ofIntPt(p) *@ tileSizef + centerOffset + mapTopLeft
                  ),
                mapPositions,
              );

            Draw.stroke(Utils.color(~r=255, ~g=236, ~b=214, ~a=255), env);
            let _ =
              List.fold_left(
                (acc, pos) =>
                  switch (acc) {
                  | None => Some(pos)
                  | Some(prevPos) =>
                    Draw.linef(
                      ~p1=Point.toPair(prevPos),
                      ~p2=Point.toPair(pos),
                      env,
                    );
                    Some(pos);
                  },
                None,
                screenPositions,
              );
            Draw.noStroke(env);
            ();
          | _ => ()
          }
        },
        row,
      )
    },
    map,
  );
};

let drawMap = (map, spriteData, env) => {
  let topleft = getMapTopLeft(map, env);
  List.iteri(
    (y, row) => {
      List.iteri(
        (x, tile) => {
          let p = Point.Int.create(x, y);
          drawTile(
            tile,
            Point.Float.(topleft + ofIntPt(p) *@ tileSizef),
            spriteData,
            env,
          );
        },
        row,
      )
    },
    map,
  );
  drawLines(map, topleft, env);
};

let findInMap = (level, withId) => {
  let cur = ref(None);
  List.iteri(
    (y, row) => {
      List.iteri(
        (x, tile) => {
          switch (tile) {
          | Floor(FilledPit(id), _) when id == withId =>
            cur := Some((Point.create(x, y), None))
          | Floor(_, Boulder(id, _) as obj)
          | Floor(_, Player(id, _, _) as obj) when id === withId =>
            cur := Some((Point.create(x, y), Some(obj)))
          | _ => ()
          }
        },
        row,
      )
    },
    level,
  );
  cur^;
};

let easeInOutQuad = t =>
  t < 0.5 ? 2. *. t *. t : (-1.) +. (4. -. 2. *. t) *. t;

let easeInOutCubic = t =>
  t < 0.5
    ? 4. *. t *. t *. t : (t -. 1.) *. (2. *. t -. 2.) *. (2. *. t -. 2.) +. 1.;

let drawObjects = (~previousLevel=?, ~time=0., level, spriteData, env) => {
  let topleft = getMapTopLeft(level, env);
  let drawHelper = (x, y, obj) => {
    let p = Point.Int.create(x, y);
    let pos = Point.Float.(topleft + ofIntPt(p) *@ tileSizef);
    drawObj(~obj, ~pos, ~spriteData, env);
  };
  switch (previousLevel) {
  | None =>
    List.iteri(
      (y, row) => {
        List.iteri(
          (x, tile) => {
            switch (tile) {
            | Floor(_, Boulder(id, _) as obj)
            | Floor(_, Player(id, _, _) as obj) => drawHelper(x, y, obj)
            | _ => ()
            }
          },
          row,
        )
      },
      level,
    )
  | Some({map: previousLevel}) =>
    List.iteri(
      (y, row) => {
        List.iteri(
          (x, tile) => {
            switch (tile) {
            | Floor(_, Boulder(id, _) as obj)
            | Floor(_, Player(id, _, _) as obj) =>
              switch (findInMap(level, id)) {
              | None => drawHelper(x, y, obj)
              | Some((p, newObj)) =>
                let obj =
                  switch (newObj) {
                  | Some(o) => o
                  | None => obj
                  };
                let pos = Point.Float.(topleft + ofIntPt(p) *@ tileSizef);
                let prevP = Point.Int.create(x, y);
                let prevPos =
                  Point.Float.(topleft + ofIntPt(prevP) *@ tileSizef);
                let time = easeInOutQuad(time /. tickTimeMS);
                let animatingPosX =
                  Utils.remapf(
                    ~value=time,
                    ~low1=0.,
                    ~high1=1.0,
                    ~low2=prevPos.x,
                    ~high2=pos.x,
                  );
                let animatingPosY =
                  Utils.remapf(
                    ~value=time,
                    ~low1=0.,
                    ~high1=1.0,
                    ~low2=prevPos.y,
                    ~high2=pos.y,
                  );

                drawObj(
                  ~obj,
                  ~pos=Point.create(animatingPosX, animatingPosY),
                  ~spriteData,
                  env,
                );
                ();
              }

            | _ => ()
            }
          },
          row,
        )
      },
      previousLevel,
    )
  };
};

let draw = (state, env) => {
  Hooks.initialize(state.hooks);
  let (levels, setLevels) = Hooks.useState(__LOC__, Levels.all);
  let (gameState, setGameState) = Hooks.useState(__LOC__, Intro);

  // This value always starts at MAX so we tick once immediately
  let (lastTickTime, setLastTickTime) =
    Hooks.useState(__LOC__, tickTimeMS +. 1.);

  if (Env.keyPressed(T, env)) {
    setLevels(Levels.all);
    setGameState(Intro);
    setLastTickTime(tickTimeMS +. 1.);
  };

  if (Env.keyPressed(E, env)) {
    editor := ! editor^;
  };

  Draw.background(Utils.color(~r=13, ~g=43, ~b=69, ~a=255), env);

  switch (levels^, gameState^) {
  | ([], _) => drawMessage("You WON the whole game", 0.0, state.font, env)
  | ([first, ...rest], Intro) =>
    drawMessage("Welcome", 0.0, state.font, env);
    if (Env.keyPressed(Space, env)) {
      setGameState(PreparingLevel(first));
    };
  | (
      [levelInitialState, ...restOfLevels],
      PreparingLevel(levelCurrentState),
    ) =>
    let (dragging, setDragging) = Hooks.useState(__LOC__, None);

    let hoveredItem = getHoveredInventoryIndex(env);
    let hoveredMapSquare = getHoveredMapSquare(levelCurrentState.map, env);

    if (editor^ && Env.keyPressed(P, env)) {
      Serialize.map(levelCurrentState.map);
    };

    let levelCurrentState =
      switch (hoveredItem, hoveredMapSquare, dragging^) {
      | (None, _, None) => levelCurrentState
      | (Some(v), _, None) when state.mouse.down =>
        setDragging(Some(v));
        levelCurrentState;
      | (Some(_), _, None) => levelCurrentState
      | (_, _, Some(i)) when Env.mousePressed(env) => levelCurrentState
      | (_, Some((x, y)), Some((draggedI, _))) =>
        setDragging(None);
        {
          ...levelCurrentState,
          items:
            List.filteri(
              (i, _) => editor^ || i != draggedI,
              levelCurrentState.items,
            ),
          map:
            setMapTile(
              levelCurrentState.map,
              Point.create(x, y),
              List.nth(levelCurrentState.items, draggedI),
            ),
        };
      | (_, _, Some(i)) =>
        setDragging(None);
        levelCurrentState;
      };

    setGameState(PreparingLevel(levelCurrentState));

    if (Env.keyPressed(R, env)) {
      setGameState(PreparingLevel(levelInitialState));
    };
    if (Env.keyPressed(Space, env)) {
      setGameState(RunningLevel([levelCurrentState]));
    };
    drawMap(levelCurrentState.map, state.spriteData, env);
    drawObjects(levelCurrentState.map, state.spriteData, env);
    drawToolbar(
      levelCurrentState.items,
      state.spriteData,
      Option.map(fst, dragging^),
      env,
    );
    Option.iter(
      ((i, dragOffset)) =>
        drawTile(
          List.nth(levelCurrentState.items, i),
          Point.Float.(
            ofIntPt(Point.fromPair(Env.mouse(env))) - dragOffset
          ),
          ~noBackground=true,
          ~withObj=true,
          state.spriteData,
          env,
        ),
      dragging^,
    );
  | ([levelInitialState, ...restOfLevels], RunningLevel([])) =>
    failwith("This should not happen, RunningLevel got an empty list.")
  | (
      [levelInitialState, ...restOfLevels],
      RunningLevel(
        [levelCurrentState, ...pastLevelStates] as allLevelStates,
      ),
    ) =>
    let deltaTime = Env.deltaTime(env) *. 1000.0;

    let (pastLevelStates, levelCurrentState) =
      if (lastTickTime^ > tickTimeMS) {
        switch (tick(levelCurrentState.map)) {
        | Move(level) =>
          setLastTickTime(0.0);
          let newLevelState = {...levelCurrentState, map: level};
          setGameState(
            RunningLevel([
              newLevelState,
              levelCurrentState,
              ...pastLevelStates,
            ]),
          );
          ([levelCurrentState, ...pastLevelStates], newLevelState);
        | Win =>
          if (editor^) {
            setGameState(
              PreparingLevel(
                List.nth(allLevelStates, List.length(allLevelStates) - 1),
              ),
            );
          } else {
            setLevels(restOfLevels);
            setGameState(WinLevel(levelCurrentState));
          };
          setLastTickTime(tickTimeMS +. 1.);
          (pastLevelStates, levelCurrentState);
        | Lose =>
          setGameState(
            LoseLevel(
              List.nth(allLevelStates, List.length(allLevelStates) - 1),
            ),
          );
          setLastTickTime(tickTimeMS +. 1.);
          (pastLevelStates, levelCurrentState);
        };
      } else {
        setLastTickTime(lastTickTime^ +. deltaTime);
        (pastLevelStates, levelCurrentState);
      };
    switch (List.nth_opt(pastLevelStates, 0)) {
    | None =>
      failwith(
        "There was only one level in the stack of levels, should not happen",
      ) // WEIRD
    | Some(pastLevel) =>
      drawMap(pastLevel.map, state.spriteData, env);
      drawObjects(
        ~previousLevel=pastLevel,
        ~time=lastTickTime^,
        levelCurrentState.map,
        state.spriteData,
        env,
      );
    };
    drawToolbar([], state.spriteData, None, env); // TODO: Any items?
    if (Env.keyPressed(R, env)) {
      setGameState(PreparingLevel(levelInitialState));
      setLastTickTime(tickTimeMS +. 1.);
    };
  | ([nextLevel, ..._], WinLevel(level)) =>
    drawMap(level.map, state.spriteData, env);
    drawObjects(level.map, state.spriteData, env);
    drawToolbar([], state.spriteData, None, env);
    let (winTimer, setWinMsgTimer) = Hooks.useState(__LOC__, winMsgTimeMS);
    let deltaTime = Env.deltaTime(env) *. 1000.0;
    if (winTimer^ < 0.0 || Env.keyPressed(Space, env)) {
      setWinMsgTimer(loseMsgTimeMS);
      setGameState(PreparingLevel(nextLevel));
    };
    setWinMsgTimer(winTimer^ -. deltaTime);
    drawMessage("You did it!", toolbarHeight, state.font, env);
    drawMessage(
      "He's safe and sound.",
      toolbarHeight -. 90.,
      state.font,
      ~withBackground=false,
      env,
    );
  | ([initialLevel, ..._], LoseLevel(prepLevelState)) =>
    drawMap(prepLevelState.map, state.spriteData, env);
    drawObjects(prepLevelState.map, state.spriteData, env);
    drawToolbar([], state.spriteData, None, env);
    let (loseTimer, setLoseTimer) = Hooks.useState(__LOC__, loseMsgTimeMS);
    let deltaTime = Env.deltaTime(env) *. 1000.0;
    if (loseTimer^ < 0.0 || Env.keyPressed(Space, env)) {
      setLoseTimer(loseMsgTimeMS);
      setGameState(PreparingLevel(prepLevelState));
    };
    setLoseTimer(loseTimer^ -. deltaTime);
    drawMessage("Gosh, keep him ALIVE", toolbarHeight, state.font, env);
    drawMessage(
      "next time, will ya?",
      toolbarHeight -. 90.,
      state.font,
      ~withBackground=false,
      env,
    );
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
    down: true,
    up: false,
    pressed: true,
  },
};
let mouseUp = (state, _) => {
  ...state,
  mouse: {
    down: false,
    up: true,
    pressed: false,
  },
};

Assets.loadSpriteSheet("assets/sprites/spritesheet.json", assets =>
  run(~setup=setup(assets), ~draw, ~mouseDown, ~mouseUp, ())
);
