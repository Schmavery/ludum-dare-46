let tileSizef = 50.;
let toolbarHeight = 180.0;
let toolbarItemRowLen = 6;
let btnMargin = 20.0;
let tickTimeMS = 500.0;
let loseMsgTimeMS = 1500.0;
let winMsgTimeMS = 1500.0;
let btnSize = toolbarHeight /. 2.0 -. 2.0 *. btnMargin;

type mouse = {
  down: bool,
  up: bool,
  pressed: bool,
  pos: Point.Int.t,
};

type state = {
  hooks: Hooks.t,
  mouse,
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
type level = {
  map,
  items: list(tile),
  title: string,
};
type tick =
  | Win
  | Lose
  | Move(map);

type rect('a) = {
  top: 'a,
  left: 'a,
  width: 'a,
  height: 'a,
};

type gameState =
  | Intro
  | WinLevel(level)
  | LoseLevel(level)
  | RunningLevel(list(level))
  | PreparingLevel(level);
