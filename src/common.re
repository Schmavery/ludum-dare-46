type mouse = {
  down: bool,
  up: bool,
  pressed: bool,
  pos: Point.Int.t,
}

type state = {
  hooks: Hooks.t,
  mouse: mouse,
};

type id = int;
type move =
  | TurnRight
  | Forward
  | TurnLeft;
type facing =
  | Up
  | Down
  | Left
  | Right;
type boulderHealth =
  | Hard
  | Cracked; // TODO: We could have "strong" boulders be ones starting with a SuperHard state
type obj =
  | Player(id, facing, list(move))
  | Boulder(id, boulderHealth)
  | Empty;
type floorKind =
  | Regular
  | FilledPit(id);
type tile =
  | Wall
  | Floor(floorKind, obj)
  | Pit;
type map = list(list(tile));
type tick =
  | Win
  | Lose
  | Move(map);