type gameStep =
  | Init
  | TurnAction
  | VoteAnimal
  | VotePlayer
  | End;

type role =
  | XuYuan
  | FangZhen
  | MuHuJiaNai
  | HuangYanYan
  | JiYunFu
  | LaoChaoFeng
  | YaoBuRan
  | ZhengGuoQu;

let roles = [|
  XuYuan,
  FangZhen,
  MuHuJiaNai,
  HuangYanYan,
  LaoChaoFeng,
  YaoBuRan,
  ZhengGuoQu,
  JiYunFu,
|];

type nested = {
  a: int,
  b: int,
};

type gameState = {
  state: gameStep,
  nested,
  name: string,
  activePlayer: int,
  playerCount: int,
  roles: array(role),
};

type testAction =
  | Init(int)
  | UpdateName(string)
  | Dramatic(int, int);

let initState = {
  state: Init,
  playerCount: 6,
  activePlayer: 0,
  roles,
  nested: {
    a: 0,
    b: 0,
  },
  name: "just started",
};

module Decode = {
  open Json.Decode;

  let testAction = json =>
    switch (json |> field("type", string)) {
    | "init" => Some(Init(json |> field("playerCount", int)))
    | "update_name" => Some(UpdateName(json |> field("name", string)))
    | "dramatic_action" =>
      Some(Dramatic(json |> field("a", int), json |> field("b", int)))
    | _ => None
    };
};

module Encode = {
  open Json.Encode;

  let role = role =>
    string(
      switch (role) {
      | XuYuan => "xu_yuan"
      | FangZhen => "fang_zhen"
      | MuHuJiaNai => "mu_hu_jia_nai"
      | HuangYanYan => "huang_yan_yan"
      | JiYunFu => "ji_yun_fu"
      | LaoChaoFeng => "lao_chao_feng"
      | YaoBuRan => "yao_bu_ran"
      | ZhengGuoQu => "zheng_guo_qu"
      },
    );

  let gameState = (state: gameState) =>
    object_([
      (
        "state",
        string(
          switch (state.state) {
          | Init => "init"
          | TurnAction => "turn_action"
          | VoteAnimal => "vote_animal"
          | VotePlayer => "vote_player"
          | End => "end"
          },
        ),
      ),
      ("name", string(state.name)),
      ("activePlayer", int(state.activePlayer)),
      ("roles", array(role, state.roles)),
      (
        "payload",
        object_([("a", int(state.nested.a)), ("b", int(state.nested.b))]),
      ),
    ]);
};

let reduce' = (state: gameState, action: testAction) =>
  switch (action) {
  | Init(playerCount) => {
      ...initState,
      playerCount,
      roles: {
        let r = Belt.Array.(roles |> slice(~offset=0, ~len=playerCount) |> shuffle);
        Js.log(r);
        r
      }
    }
  | UpdateName(name) => {...state, state: TurnAction, name}
  | Dramatic(a, b) => {
      ...state,
      state: VotePlayer,
      nested: {
        a: (state.nested.a + 1) * a,
        b: (state.nested.b * b + 2) * (-1),
      },
    }
  };

let toJs = action => action |> Encode.gameState;

let reduce = (state: option(gameState), jsAction) =>
  switch (state) {
  | Some(state') =>
    switch (Decode.testAction(jsAction)) {
    | Some(action) => reduce'(state', action)
    | None => state'
    }
  | None => initState
  };
