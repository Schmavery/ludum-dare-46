open Common;

let id = {
  let counter = ref(0);
  () => {
    incr(counter);
    counter^;
  };
};

let level1 = {
  title: "Tutorial",
  items: [Floor(Regular, Boulder(id(), Hard))],
  map: [
    [
      Floor(Regular, Empty),
      Floor(Regular, Empty),
      Floor(Regular, Empty),
      Floor(Regular, Empty),
      Floor(Regular, Empty),
    ],
    [
      Floor(
        Regular,
        Player(
          id(),
          Right,
          [Forward, Forward, Forward, Forward, TurnLeft, Forward],
        ),
      ),
      Floor(Regular, Boulder(id(), Hard)),
      Floor(Regular, Boulder(id(), Hard)),
      Pit,
      Floor(Regular, Empty),
    ],
    [
      Floor(Regular, Empty),
      Floor(Regular, Empty),
      Floor(Regular, Empty),
      Floor(Regular, Empty),
      Floor(Regular, Empty),
    ],
  ],
};

let level2 = {
  title: "Tutorial",
  items: [Floor(Regular, Boulder(id(), Hard))],
  map: [
    [
      Floor(Regular, Empty),
      Floor(Regular, Empty),
      Floor(Regular, Empty),
      Floor(Regular, Empty),
      Floor(Regular, Empty),
    ],
    [
      Floor(
        Regular,
        Player(
          id(),
          Right,
          [Forward, Forward, Forward, Forward, TurnLeft, Forward],
        ),
      ),
      Floor(Regular, Empty),
      Floor(Regular, Empty),
      Pit,
      Floor(Regular, Empty),
    ],
    [
      Floor(Regular, Empty),
      Floor(Regular, Empty),
      Floor(Regular, Empty),
      Floor(Regular, Empty),
      Floor(Regular, Empty),
    ],
  ],
};

let all = [level1, level2];
