
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
  state: string,
  playerCount: int,
};

let init = playerCount => gameState(~state=gameStepToJs(`Init), ~playerCount=playerCount);

let endGame = () => gameState(~state=gameStepToJs(`End), ~playerCount=0);

let turnAction = (state, action) => {
  gameState(
    ~state=gameStepToJs(`TurnAction),
  )
};

let gameReducer = (state, action) => switch(gameStepFromJs(action |. type_Get)) {
  | Some(s) => switch(s) {
    | `Init => init(8)
    | `End => {Js.log("action");Js.log(action);endGame()}
    | `TurnAction => {Js.log("action");Js.log(action);endGame()}
    | _ => state
    }
  | None => state
};

/* Js.log(gameReducer(action(~type_="End"))) */
