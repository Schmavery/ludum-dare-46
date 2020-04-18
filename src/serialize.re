open Common;

// Do this to get the map Serialize.map(level);

let floorKind = fKind => {
  switch (fKind) {
  | Regular => "Regular"
  | FilledPit(_) => "FilledPit(id())"
  };
};

let move = m => {
  switch (m) {
  | TurnRight => "TurnRight"
  | Forward => "Forward"
  | TurnLeft => "TurnLeft"
  };
};

let moves = mList => {
  "[" ++ String.concat(",", List.map(m => {move(m)}, mList)) ++ "]";
};

let boulderHealth = health => {
  switch (health) {
  | Cracked => "Cracked"
  | Hard => "Hard"
  };
};

let facing = f => {
  switch (f) {
  | Up => "Up"
  | Down => "Down"
  | Left => "Left"
  | Right => "Right"
  };
};

let obj = o => {
  switch (o) {
  | Player(id, f, mList) =>
    "Player(id()," ++ facing(f) ++ "," ++ moves(mList) ++ ")"
  | Boulder(id, health) => "Boulder(id()," ++ boulderHealth(health) ++ ")"
  | Empty => "Empty"
  };
};

let tile = t => {
  switch (t) {
  | Wall => "Wall"
  | Floor(fKind, o) => "Floor(" ++ floorKind(fKind) ++ ", " ++ obj(o) ++ ")"
  | Pit => "Pit"
  };
};

let map = m => {
  let r =
    "["
    ++ String.concat(
         ",",
         List.map(
           row => {
             "[" ++ String.concat(",", List.map(t => {tile(t)}, row)) ++ "]"
           },
           m,
         ),
       )
    ++ "]";
  print_endline(r);
};
