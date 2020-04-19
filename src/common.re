open Reprocessing;

let tileSizef = 50.;
let toolbarHeight = 90.0;
let toolbarItemRowLen = 10;
let btnMargin = 20.0;
let fontHeight = 50;
let tickTimeMS = 600.0;
let loseMsgTimeMS = 1500.0;
let winMsgTimeMS = 1500.0;
let btnSize = toolbarHeight -. 2.0 *. btnMargin;

module StringMap = Map.Make(String);

module Sprite = {
  type spriteEntry = {
    x: int,
    y: int,
    w: int,
    h: int,
  };

  type t = {
    sheet: Reprocessing.imageT,
    map: StringMap.t(spriteEntry),
  };

  let create = (sheet, map) => {sheet, map};
};

type mouse = {
  down: bool,
  up: bool,
  pressed: bool,
};

type state = {
  hooks: Hooks.t,
  mouse,
  spriteData: Sprite.t,
  font: fontT,
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

module Rect = {
  type t('a) = {
    top: 'a,
    left: 'a,
    width: 'a,
    height: 'a,
  };

  let fromPoints =
      ({x: left, y: top}: Point.t('a), {x: width, y: height}: Point.t('a)) => {
    top,
    left,
    width,
    height,
  };

  let containsPt = ({top, left, width, height}, {x, y}: Point.Int.t) => {
    x >= left && y >= top && x < left + width && y < top + height;
  };

  let containsPtf = ({top, left, width, height}, {x, y}: Point.Float.t) => {
    x >= left && y >= top && x < left +. width && y < top +. height;
  };

  let printf = ({top, left, width, height}) =>
    Printf.printf(
      "{top:%f, left:%f, w:%f, h%f}\n%!",
      top,
      left,
      width,
      height,
    );
};

type gameState =
  | Intro
  | WinLevel(level)
  | LoseLevel(level)
  | RunningLevel(list(level))
  | PreparingLevel(level);

module Option = {
  type t('a) = option('a);
  let iter = (f, o) =>
    switch (o) {
    | None => ()
    | Some(v) => f(v)
    };

  let map = (f, o) =>
    switch (o) {
    | Some(v) => Some(f(v))
    | None => None
    };
};

module List = {
  include List;

  let filteri = (p: (int, 'a) => bool, l: list('a)) =>
    fst(
      List.fold_right(
        (el, (acc, i)) =>
          p(i, el) ? ([el, ...acc], i - 1) : (acc, i - 1),
        l,
        ([], List.length(l) - 1),
      ),
    );
};
