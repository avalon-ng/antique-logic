
let s : string = {js|安安|js};

[@bs.deriving jsConverter]
type gameStep = [
  | `Init
  | `TurnAction
  | `VoteAnimal
  | `VotePlayer
  | `End
];

Js.log(gameStepFromJs("Init"));
Js.log(gameStepToJs(`Init));

[@bs.deriving abstract]
type action = {
  [@bs.as "type"] type_: string,
};

[@bs.deriving abstract]
type gameState = {
  state: gameStep,
  playerCount: int,
};

let init = playerCount => gameState(~state=`Init, ~playerCount=playerCount);

let gameReducer = (state, action) => switch(gameStepFromJs(action |. type_Get)) {
  | Some(`Init) => Some(init(8))
  | Some(_) => Some(state)
  | None => None
};

/* Js.log(gameReducer(action(~type_="End"))) */
