type gameStep =
  | Init
  | TurnAction
  | VoteAnimal
  | VotePlayer
  | End
  | Error
  ;

type nested = {
  a: int,
  b: int,
};

type gameState = {
  state: gameStep,
  nested: nested,
  name: string,
  activePlayer: int,
  playerCount: int,
};

type testAction =
  | Init(int)
  | UpdateName(string)
  | Dramatic(int, int);

let initState = {
  state: Init,
  playerCount: 6,
  activePlayer: 0,
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

  let gameState = json => {
    state: switch(json |> field("state", string)) {
      | "init" => Init
      | "turn_action" => TurnAction
      | "vote_animal" => VoteAnimal
      | "vote_player" => VotePlayer
      | "end" => End
      | _ => Error
    },
    playerCount: json |> field("playerCount", int),
    activePlayer: json |> field("activePlayer", int),
    nested: {
      a: 0,
      b: 0,
    },
    name: "just started"
  };

};

module Encode = {
  open Json.Encode;

  let gameState = (state: gameState) => object_([
    ("state", string(switch(state.state) {
      | Init => "init"
      | TurnAction => "turn_action"
      | VoteAnimal => "vote_animal"
      | VotePlayer => "vote_player"
      | End => "end"
      | _ => "error"
    })),
    ("name", string(state.name)),
    ("activePlayer", int(state.activePlayer)),
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
    state: TurnAction,
    name: name,
  }
  | Dramatic(a, b) => {
    ...state,
    state: VotePlayer,
    nested: {
      a: (state.nested.a + 1) * a,
      b: (state.nested.b * b + 2) * -1,
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

/* action in, state out */
let reduce = (state, jsAction) => {
  let result = reduce'(state, jsAction);
  Js.log(toJs(result));
  result
};
