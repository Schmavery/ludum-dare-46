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
    soundData: Sound.load(env),
    font: Draw.loadFont(~filename=fontPath, env),
  };
};

let drawObj =
    (
      ~obj,
      ~pos as {Point.x, y},
      ~spriteData,
      ~height=tileSizef,
      ~width=tileSizef,
      env,
    ) => {
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
    Assets.drawSprite(spriteData, assetName, ~pos, ~width, ~height, env);
  | Boulder(_, health) =>
    switch (health) {
    | Hard =>
      Assets.drawSprite(
        spriteData,
        "normal_boulder",
        ~pos,
        ~width=tileSizef,
        ~height=tileSizef,
        env,
      )
    | Cracked =>
      Assets.drawSprite(
        spriteData,
        "cracked_boulder",
        ~pos,
        ~width=tileSizef,
        ~height=tileSizef,
        env,
      )
    }
  | Empty => ()
  };
};

let drawTile =
    (
      kind,
      {x, y}: Point.Float.t,
      ~time,
      ~noBackground=false,
      ~withObj=false,
      spriteData: Sprite.t,
      env,
    ) => {
  let halfTileSize = tileSizef /. 2.;
  let pos = Point.create(x +. halfTileSize, y +. halfTileSize);
  switch (kind) {
  | Floor(kind, obj) =>
    let background = !noBackground || editor^;
    switch (kind) {
    | Regular =>
      if (background) {
        Assets.drawSprite(
          spriteData,
          "floor",
          ~pos,
          ~width=tileSizef,
          ~height=tileSizef,
          env,
        );
      }
    | FilledPit(_) =>
      if (background) {
        Assets.drawSprite(
          spriteData,
          "pit_with_boulder",
          ~pos,
          ~width=tileSizef,
          ~height=tileSizef,
          env,
        );
      }
    | Spinner(dir) =>
      if (background) {
        Assets.drawSprite(
          spriteData,
          "floor",
          ~pos,
          ~width=tileSizef,
          ~height=tileSizef,
          env,
        );
      };
      Assets.drawSpriteWithCenterRotation(
        spriteData,
        "wall",
        ~pos,
        ~width=tileSizef,
        ~height=tileSizef,
        ~rot=dir == CW ? time : -. time,
        env,
      );
    };
    if (withObj) {
      drawObj(~obj, ~pos={x, y}, ~spriteData, env);
    };
  | Pit =>
    Assets.drawSprite(
      spriteData,
      "pit",
      ~pos,
      ~width=tileSizef,
      ~height=tileSizef,
      env,
    )
  | Wall =>
    Assets.drawSprite(
      spriteData,
      "wall",
      ~pos,
      ~width=tileSizef,
      ~height=tileSizef,
      env,
    )
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
    | Floor(Regular, Empty) => Some(Point.create(x, y))
    | _ when editor^ => Some(Point.create(x, y))
    | _ => None
    };
  } else {
    None;
  };
};

let getHoveredInventoryIndex = (items, env) => {
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
    let index = x + y * toolbarItemRowLen;
    if (index < List.length(items)) {
      Some((index, hoverOffset));
    } else {
      None;
    };
  } else {
    None;
  };
};

let drawInventory = (inventory, spriteData, hovered, ~time, env) => {
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
        ~time,
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

let getUndoRect = env => {
  let height = float_of_int(Env.height(env));
  let backgroundY = height -. toolbarHeight;
  let size = tileSizef *. 1.4;
  Rect.fromPoints(
    Point.create(btnMargin, btnMargin +. backgroundY),
    Point.create(size, size),
  );
};

let getBackRect = env => {
  let height = float_of_int(Env.height(env));
  let backgroundY = height -. toolbarHeight;
  let size = tileSizef *. 1.4;
  Rect.fromPoints(
    Point.create(btnMargin, btnMargin +. backgroundY),
    Point.create(size, size),
  );
};

let getPlayRect = env => {
  let height = float_of_int(Env.height(env));
  let backgroundY = height -. toolbarHeight;
  let size = tileSizef *. 1.4;
  Rect.fromPoints(
    Point.create(btnMargin +. size +. btnMargin, btnMargin +. backgroundY),
    Point.create(size, size),
  );
};

let drawToolbar =
    (inventory, ~accelerateTime=false, spriteData, hovered, ~time, env) => {
  Draw.fill(Utils.color(~r=210, ~g=210, ~b=230, ~a=255), env);
  let width = float_of_int(Env.width(env));
  let height = float_of_int(Env.height(env));
  let x = 0.0;
  let backgroundY = height -. toolbarHeight;
  Draw.rectf(~pos=(x, backgroundY), ~width, ~height, env);

  // Reset
  // let rect = getUndoRect(env);
  // Assets.drawSprite(
  //   spriteData,
  //   "undo",
  //   ~pos=
  //     Point.create(
  //       rect.left +. rect.width /. 2.,
  //       rect.top +. rect.height /. 2.,
  //     ),
  //   ~width=rect.width,
  //   ~height=rect.height,
  //   env,
  // );

  let rect = getBackRect(env);
  Assets.drawSprite(
    spriteData,
    "back",
    ~pos=
      Point.create(
        rect.left +. rect.width /. 2.,
        rect.top +. rect.height /. 2.,
      ),
    ~width=rect.width,
    ~height=rect.height,
    env,
  );

  let rect = getPlayRect(env);
  let playOrAcceleate = if (accelerateTime) {"accelerate"} else {"play"};
  Assets.drawSprite(
    spriteData,
    playOrAcceleate,
    ~pos=
      Point.create(
        rect.left +. rect.width /. 2.,
        rect.top +. rect.height /. 2.,
      ),
    ~width=rect.width,
    ~height=rect.height,
    env,
  );

  drawInventory(inventory, spriteData, hovered, ~time, env);
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

let rec resolveMove = (level, pos, moveDelta, retrying, state, env) => {
  let secondPos = Point.Int.add(pos, moveDelta);
  let replaceWith = (level, t1, t2) =>
    Move(setMapTile(setMapTile(level, pos, t1), secondPos, t2));

  let retryResolveMove = level =>
    !retrying ? resolveMove(level, pos, moveDelta, true, state, env) : Move(level);

  let resolveMove = (level, pos, moveDelta) =>
    !retrying ? resolveMove(level, pos, moveDelta, false, state, env) : Move(level);

  switch (getMapTile(level, pos), getMapTile(level, secondPos)) {
  | (Wall | Pit | Floor(_, Empty), _) => Move(level)
  | (Floor(k1, Boulder(id, health)), Wall) => Move(level)
  | (_, Floor(_, Player(_))) => Lose
  | (Floor(k1, Boulder(id, health)), Floor(k2, Empty)) =>
    Sound.play("moving_boulder", state, env);
    replaceWith(level, Floor(k1, Empty), Floor(k2, Boulder(id, health)))
  | (Floor(k1, Boulder(id, health)), Pit) =>
    Sound.play("moving_boulder", state, env);
    replaceWith(level, Floor(k1, Empty), Floor(FilledPit(id), Empty))
  | (Floor(k, Player(id, facing, moves)), Floor(Spinner(dir), Empty)) =>
    let move = dir == CW ? TurnRight : TurnLeft;
    replaceWith(
      level,
      Floor(k, Empty),
      Floor(Spinner(dir), Player(id, facing, [move, ...moves])),
    );
  | (
      Floor(Spinner(dir), Player(id, facing, moves)),
      Floor(k, Boulder(id2, boulderState)),
    )
      when retrying =>
    let move = dir == CW ? TurnRight : TurnLeft;
    let obj = boulderState == Hard ? Boulder(id2, Cracked) : Empty;
    Sound.play("rock_crack", state, env);
    replaceWith(
      level,
      Floor(Spinner(dir), Player(id, facing, [move, ...moves])),
      Floor(k, obj),
    );
  | (Floor(k1, Player(_) as p), Floor(k2, Empty)) =>
    replaceWith(level, Floor(k1, Empty), Floor(k2, p))
  | (Floor(_, Player(_)), Wall)
  | (Floor(_, Player(_)), Pit) => Lose
  | (Floor(k1, Player(_) as p), Floor(k2, Boulder(id, boulderState)))
      when retrying =>
    let obj = boulderState == Hard ? Boulder(id, Cracked) : Empty;
    Sound.play("rock_crack", state, env);
    replaceWith(level, Floor(k1, p), Floor(k2, obj))
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
let tick = (level, state, env) => {
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
        resolveMove(level, pos, facingToDelta(facing), false, state, env)
      | (_, AgentWin)
      | (Win, _) => Win
      | (Lose, _) => Lose // TODO: Might still want to resolve the rest of the moves..
      },
    Move(level),
    movingAgents,
  );
};

let drawMessage =
    (message, font, ~offset=0, ~withControlHelp="", ~time=0., env) => {
  let y = (180 + offset - fontHeight) / 2;
  {
    let textWidth = Draw.textWidth(~font, ~body=message, env);
    let x = (Env.width(env) - textWidth) / 2;
    Draw.tint(Utils.color(~r=255, ~g=236, ~b=214, ~a=255), env);
    Draw.text(~font, ~body=message, ~pos=(x, y), env);
  };

  {
    let textWidth = Draw.textWidth(~font, ~body=withControlHelp, env);
    let x = (Env.width(env) - textWidth) / 2;
    Draw.tint(Utils.color(~r=255, ~g=236, ~b=214, ~a=255), env);
    Draw.text(~font, ~body=withControlHelp, ~pos=(x, y + fontHeight), env);
  };
  Draw.noTint(env);
};

let drawLines = (map, env) => {
  let mapTopLeft = getMapTopLeft(map, env);
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

let drawMap = (map, spriteData, ~time, env) => {
  let topleft = getMapTopLeft(map, env);
  List.iteri(
    (y, row) => {
      List.iteri(
        (x, tile) => {
          let p = Point.Int.create(x, y);
          drawTile(
            tile,
            ~time,
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

let drawObjects = (~previousLevel=?, ~time=0., level, state, env) => {
  let topleft = getMapTopLeft(level, env);
  let drawHelper = (x, y, obj) => {
    let p = Point.Int.create(x, y);
    let pos = Point.Float.(topleft + ofIntPt(p) *@ tileSizef);
    drawObj(~obj, ~pos, ~spriteData=state.spriteData, env);
  };
  let calculateBounce =
      (elapsedTime, pos: Point.Float.t, prevPos: Point.Float.t) => {
    let numBounces = 2.;
    // TODO: There's something we could do here to ease more proportionally.
    let time = easeInOutQuad(elapsedTime /. tickTimeMS);

    let bounce =
      Utils.remapf(
        ~value=time,
        ~low1=0.,
        ~high1=1.0,
        ~low2=0.,
        ~high2=numBounces *. Constants.pi,
      );

    // If we're moving in the x position, we need to calculate gravity from a Y perspective
    let bounceY =
      if (prevPos.x != pos.x) {
        sin(bounce) *. 6.;
      } else if (prevPos.y != pos.y) {
        sin(bounce) *. 6.;
      } else {
        0.;
      };

    abs_float(bounceY) *. (-1.0);
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
                let elapsedTime = time;
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

                let animatedPosition =
                  Point.Float.create(animatingPosX, animatingPosY);
                let bounceY =
                  switch (tile) {
                  | Floor(_, Player(_, _, _)) =>
                    calculateBounce(elapsedTime, pos, prevPos)
                  | _ => 0.
                  };

                let squishY =
                  switch (tile) {
                  | Floor(_, Player(_, _, _)) =>
                    tileSizef -. bounceY *. (-0.75)
                  | _ => tileSizef
                  };
                let squishX =
                  switch (tile) {
                  | Floor(_, Player(_, _, _)) =>
                    tileSizef +. bounceY *. (-0.75)
                  | _ => tileSizef
                  };

                let bouncedPosition = Point.Float.create(0., bounceY);

                drawObj(
                  ~obj,
                  ~pos=Point.Float.(animatedPosition + bouncedPosition),
                  ~spriteData=state.spriteData,
                  ~height=squishY,
                  ~width=squishX,
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

let getClickOn = (rect, mousePtf, env) => {
  let (downOnRestarted, setDownOnRestarted) = Hooks.useState(__LOC__, false);

  let clicked =
    if (Env.mousePressed(env)) {
      setDownOnRestarted(rect->Rect.containsPtf(mousePtf));
      false;
    } else if (downOnRestarted^) {
      setDownOnRestarted(false);
      rect->Rect.containsPtf(mousePtf);
    } else {
      false;
    };
  (clicked, downOnRestarted^);
};

let draw = (state, env) => {
  Hooks.initialize(state.hooks);
  let (levels, setLevels) = Hooks.useState(__LOC__, Levels.all);
  let (gameState, setGameState) = Hooks.useState(__LOC__, Intro);

  // This value always starts at MAX so we tick once immediately
  let (lastTickTime, setLastTickTime) =
    Hooks.useState(__LOC__, tickTimeMS +. 1.);

  let accelerateTime = false;

  let mousePtf = Point.(Float.ofIntPt(fromPair(Env.mouse(env))));

  let undo =
    if (Env.mousePressed(env)) {
      let rect = getUndoRect(env);
      rect->Rect.containsPtf(
        Point.(Float.ofIntPt(fromPair(Env.mouse(env)))),
      );
    } else {
      false;
    };

  let backButtonRect = getBackRect(env);
  let (restartClicked, restartButtonDown) =
    getClickOn(backButtonRect, mousePtf, env);
  let restartClicked = restartClicked || Env.keyPressed(R, env);
  let playButtonRect = getPlayRect(env);
  let (playClicked, playButtonDown) =
    getClickOn(playButtonRect, mousePtf, env);
  let playClicked = playClicked || Env.keyPressed(Space, env);

  let (totalTime, setTotalTime) = Hooks.useState(__LOC__, 0.);
  setTotalTime(mod_float(totalTime^ +. Env.deltaTime(env), 10000000.));

  if (Env.keyPressed(T, env)) {
    setLevels(Levels.all);
    setGameState(Intro);
    setLastTickTime(tickTimeMS +. 1.);
  };

  if (Env.keyPressed(E, env)) {
    editor := ! editor^;
  };

  if (editor^) {
    Draw.background(Constants.red, env);
  } else {
    Draw.background(Utils.color(~r=13, ~g=43, ~b=69, ~a=255), env);
  };

  switch (levels^, gameState^) {
  | ([], _) => drawMessage("You WON the whole game", state.font, env)
  | ([first, ...rest], Intro) =>
    drawMessage("A Treacherous Crossing", state.font, ~withControlHelp="press SPACE", env);
    if (Env.keyPressed(Space, env)) {
      setGameState(PreparingLevel([first]));
    };
  | (
      [levelInitialState, ...restOfLevels],
      PreparingLevel([levelCurrentState, ...undoStates]),
    ) =>
    let (dragging, setDragging) = Hooks.useState(__LOC__, None);

    let hoveredItem = getHoveredInventoryIndex(levelCurrentState.items, env);
    let hoveredMapSquare = getHoveredMapSquare(levelCurrentState.map, env);

    if (editor^ && Env.keyPressed(P, env)) {
      Serialize.map(levelCurrentState.map);
    };

    let levelCurrentState =
      if (editor^) {
        let removeLast = l =>
          switch (List.rev(l)) {
          | [_, ...tl] => List.rev(tl)
          | [] => []
          };

        let append = (e, l) => List.rev([e, ...List.rev(l)]);

        let hoveredTile =
          Option.map(
            p => (getMapTile(levelCurrentState.map, p), p),
            hoveredMapSquare,
          );
        switch (hoveredTile) {
        | Some((Floor(k, Player(id, facing, moves)), pt)) =>
          if (Env.keyPressed(Backspace, env) || Env.keyPressed(Down, env)) {
            {
              ...levelCurrentState,
              map:
                setMapTile(
                  levelCurrentState.map,
                  pt,
                  Floor(k, Player(id, facing, removeLast(moves))),
                ),
            };
          } else if (Env.keyPressed(Right, env)) {
            {
              ...levelCurrentState,
              map:
                setMapTile(
                  levelCurrentState.map,
                  pt,
                  Floor(k, Player(id, facing, append(TurnRight, moves))),
                ),
            };
          } else if (Env.keyPressed(Left, env)) {
            {
              ...levelCurrentState,
              map:
                setMapTile(
                  levelCurrentState.map,
                  pt,
                  Floor(k, Player(id, facing, append(TurnLeft, moves))),
                ),
            };
          } else if (Env.keyPressed(Up, env)) {
            {
              ...levelCurrentState,
              map:
                setMapTile(
                  levelCurrentState.map,
                  pt,
                  Floor(k, Player(id, facing, append(Forward, moves))),
                ),
            };
          } else {
            levelCurrentState;
          }

        | Some(_)
        | None => levelCurrentState
        };
      } else {
        levelCurrentState;
      };

    let maybeUpdatedLevel =
      switch (hoveredItem, hoveredMapSquare, dragging^) {
      | (None, _, None) => levelCurrentState
      | (Some(v), _, None) when state.mouse.down =>
        setDragging(Some(v));
        Sound.play("pickup", state, env);
        levelCurrentState;
      | (Some(_), _, None) => levelCurrentState
      | (_, _, Some(i)) when Env.mousePressed(env) => levelCurrentState
      | (_, Some(mapSquare), Some((draggedI, _))) =>
        setDragging(None);
        let newTile =
          switch (List.nth(levelCurrentState.items, draggedI)) {
          | Floor(k, Boulder(_, bk)) => Floor(k, Boulder(Levels.id(), bk))
          | Floor(k, Player(_, f, m)) =>
            Floor(k, Player(Levels.id(), f, m))
          | t => t
          };

        Sound.play("drop", state, env);
        {
          ...levelCurrentState,
          items:
            List.filteri(
              (i, _) => editor^ || i != draggedI,
              levelCurrentState.items,
            ),
          map: setMapTile(levelCurrentState.map, mapSquare, newTile),
        };
      | (_, _, Some(i)) =>
        setDragging(None);
        levelCurrentState;
      };

    let (levelCurrentState, undoStates) =
      if (maybeUpdatedLevel != levelCurrentState) {
        (maybeUpdatedLevel, [levelCurrentState, ...undoStates]);
      } else if (Env.keyPressed(U, env)) {
        switch (undoStates) {
        | [] => (levelCurrentState, [])
        | [undoState, ...undoStates] => (undoState, undoStates)
        };
      } else {
        (levelCurrentState, undoStates);
      };

    setGameState(PreparingLevel([levelCurrentState, ...undoStates]));

    if (restartClicked) {
      setGameState(PreparingLevel([levelInitialState]));
    };
    if (playClicked) {
      setGameState(
        RunningLevel({
          states: [levelCurrentState],
          preparingUndoStack: [levelCurrentState, ...undoStates],
        }),
      );
    };
    drawMap(levelCurrentState.map, state.spriteData, ~time=totalTime^, env);
    drawLines(levelCurrentState.map, env);
    drawObjects(levelCurrentState.map,state, env);
    drawToolbar(
      levelCurrentState.items,
      ~accelerateTime,
      state.spriteData,
      Option.map(fst, dragging^),
      ~time=totalTime^,
      env,
    );
    Option.iter(
      ((i, dragOffset)) =>
        drawTile(
          List.nth(levelCurrentState.items, i),
          Point.Float.(mousePtf - dragOffset),
          ~time=totalTime^,
          ~noBackground=true,
          ~withObj=true,
          state.spriteData,
          env,
        ),
      dragging^,
    );
  | ([levelInitialState, ...restOfLevels], RunningLevel({states: []})) =>
    failwith("This should not happen, RunningLevel got an empty list.")
  | (
      [levelInitialState, ...restOfLevels],
      RunningLevel({
        states: [levelCurrentState, ...pastLevelStates],
        preparingUndoStack,
      }),
    ) =>
    let deltaTime = Env.deltaTime(env) *. 1000.0;

    let (pastLevelStates, levelCurrentState) =
      if (lastTickTime^ > tickTimeMS) {
        switch (tick(levelCurrentState.map, state, env)) {
        | Move(level) =>
          setLastTickTime(0.0);
          let newLevelState = {...levelCurrentState, map: level};
          setGameState(
            RunningLevel({
              states: [newLevelState, levelCurrentState, ...pastLevelStates],
              preparingUndoStack,
            }),
          );
          ([levelCurrentState, ...pastLevelStates], newLevelState);
        | Win =>
          if (editor^) {
            setGameState(PreparingLevel(preparingUndoStack));
          } else {
            Sound.play("win", state, env);
            setLevels(restOfLevels);
            setGameState(WinLevel(levelCurrentState));
          };
          setLastTickTime(tickTimeMS +. 1.);
          (pastLevelStates, levelCurrentState);
        | Lose =>
          Sound.play("lose", state, env);
          setGameState(
            LoseLevel({loseState: levelCurrentState, preparingUndoStack}),
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
      drawMap(pastLevel.map, state.spriteData, ~time=totalTime^, env);
      if (lastTickTime^ /. tickTimeMS < 0.5) {
        drawLines(pastLevel.map, env);
      } else {
        drawLines(levelCurrentState.map, env);
      };
      drawObjects(
        ~previousLevel=pastLevel,
        ~time=lastTickTime^,
        levelCurrentState.map,
        state,
        env,
      );
    };
    drawToolbar([], state.spriteData, None, ~time=totalTime^, env); // TODO: Any items?
    if (restartClicked) {
      setGameState(PreparingLevel(preparingUndoStack));
      setLastTickTime(tickTimeMS +. 1.);
    };
  | ([nextLevel, ..._], WinLevel(level)) =>
    drawMap(level.map, state.spriteData, ~time=totalTime^, env);
    drawLines(level.map, env);
    drawObjects(level.map, state, env);
    drawToolbar([], state.spriteData, None, ~time=totalTime^, env);
    let deltaTime = Env.deltaTime(env) *. 1000.0;
    if (Env.keyPressed(Space, env)) {
      setGameState(PreparingLevel([nextLevel]));
    };
    drawMessage(
      "That's how it's done!",
      ~withControlHelp="press SPACE",
      state.font,
      env,
    );
  | (
      [initialLevel, ..._],
      LoseLevel({loseState, preparingUndoStack: prepLevelState}),
    ) =>
    drawMap(loseState.map, state.spriteData, ~time=totalTime^, env);
    drawLines(loseState.map, env);
    drawObjects(loseState.map, state, env);
    drawToolbar([], state.spriteData, None, ~time=totalTime^, env);
    let deltaTime = Env.deltaTime(env) *. 1000.0;
    if (Env.keyPressed(Space, env)) {
      setGameState(PreparingLevel(prepLevelState));
    };
    drawMessage(
      "Oh no! Keep him alive next time, ok?",
      state.font,
      ~withControlHelp="press SPACE",
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
