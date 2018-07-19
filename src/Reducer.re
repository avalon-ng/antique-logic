type gameStep =
  | Init
  | TurnAction
  | VoteAnimal
  | VotePlayer
  | End
  ;

type nested = {
  a: int,
  b: int,
};

type gameState = {
  state: gameStep,
  nested: nested,
  name: string,
  playerCount: int,
};

type testAction =
  | Init(int)
  | UpdateName(string)
  | Dramatic(int, int);

let initState = {
  state: Init,
  playerCount: 6,
  nested: {
    a: 0,
    b: 0,
  },
  name: "just started"
};


module Decode = {
  open Json.Decode;

  let testAction = json =>
    switch (json |> field("type", string)) {
    | "init" => Some(Init(json |> field("playerCount", int)))
    | "update_name" => Some(UpdateName(json |> field("name", string)))
    | "dramatic_action" => Some(Dramatic(
        json |> field("a", int),
        json |> field("b", int),
      ))
    | _ => None
    };
};

module Encode = {
  open Json.Encode;

  let gameState = (state: gameState) => object_([
    ("name", string(state.name)),
    ("payload", object_([
      ("a", int(state.nested.a)),
      ("b", int(state.nested.b)),
    ])),
  ])
};

let toJs = action => action |> Encode.gameState;
let fromJs = js => js |> Decode.testAction;

let reduce'' = (state: gameState, action: testAction) => switch(action) {
  | Init(playerCount) => {
    ...initState,
    playerCount: playerCount
  }
  | UpdateName(name) => {
    ...state,
    name: name,
  }
  | Dramatic(a, b) => {
    ...state,
    nested: {
      a: a + 1,
      b: b * 2,
    }
  }
};

let reduce' = (state: option(gameState), jsAction) => switch(state) {
  | Some(state') => switch(Decode.testAction(jsAction)) {
    | Some(action) => reduce''(state', action)
    | None => state'
    }
  | None => initState
};

let reduce = (state, jsAction) => toJs(reduce'(state, jsAction));
