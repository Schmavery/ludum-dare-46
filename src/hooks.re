module Internal: {
  type t;
  let empty: t;
  let initialize: t => unit;
  let finalize: unit => t;
  let useState: (string, ~id: string=?, 'a) => ('a, ('a => 'a) => unit);
} = {
  module StringMap = Map.Make(String);

  type t = StringMap.t(string);

  let globalRef: ref(t) = ref(StringMap.empty);

  let empty = StringMap.empty;

  let initialize = hooks => globalRef := hooks;

  let finalize = () => globalRef^;

  let useState =
      (loc: string, ~id="", initialState: 'a): ('a, ('a => 'a) => unit) => {
    let key = loc ++ id;
    let initialState = Obj.magic(initialState);
    let v =
      switch (StringMap.find(key, globalRef^)) {
      | v => v
      | exception Not_found =>
        globalRef := StringMap.add(key, initialState, globalRef^);
        initialState;
      };
    let setState = cb => {
      let currentVal = Obj.magic(StringMap.find(key, globalRef^));
      let newVal = Obj.magic(cb(currentVal));
      globalRef := StringMap.add(key, newVal, globalRef^);
    };
    (Obj.magic(v), setState);
  };
};

include Internal
