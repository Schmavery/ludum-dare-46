open Common;

// Do this to get the level LevelSerializer.serializeLevel(level);

let serializeFloorKind = floorKind => {
  switch (floorKind) {
  | Regular => "Regular"
  | FilledPit(_) => "FilledPit(id())"
  };
};

let serializeMove = move => {
  switch (move) {
  | TurnRight => "TurnRight"
  | Forward => "Forward"
  | TurnLeft => "TurnLeft"
  };
};

let serializeMoves = moves => {
  "["
  ++ String.concat(",", List.map(move => {serializeMove(move)}, moves))
  ++ "]";
};

let serializeBoulderHealth = health => {
  switch (health) {
  | Cracked => "Cracked"
  | Hard => "Hard"
  };
};

let serializeFacing = facing => {
  switch (facing) {
  | Up => "Up"
  | Down => "Down"
  | Left => "Left"
  | Right => "Right"
  };
};

let serializeObj = obj => {
  switch (obj) {
  | Player(id, facing, moves) =>
    "Player(id(),"
    ++ serializeFacing(facing)
    ++ ","
    ++ serializeMoves(moves)
    ++ ")"
  | Boulder(id, health) =>
    "Boulder(id()," ++ serializeBoulderHealth(health) ++ ")"
  | Empty => "Empty"
  };
};

let serializeTile = tile => {
  switch (tile) {
  | Wall => "Wall"
  | Floor(floorKind, obj) =>
    "Floor("
    ++ serializeFloorKind(floorKind)
    ++ ", "
    ++ serializeObj(obj)
    ++ ")"
  | Pit => "Pit"
  };
};

let serializeLevel = level => {
  let r =
    "["
    ++ String.concat(
         ",",
         List.map(
           row => {
             "["
             ++ String.concat(
                  ",",
                  List.map(tile => {serializeTile(tile)}, row),
                )
             ++ "]"
           },
           level,
         ),
       )
    ++ "]";
  print_endline(r);
};
