type gameStep =
  | Init
  | TurnAction
  | VoteZodiac
  | VotePlayer
  | End

type role =
  | XuYuan
  | FangZhen
  | MuHuJiaNai
  | HuangYanYan
  | JiYunFu
  | LaoChaoFeng
  | YaoBuRan
  | ZhengGuoQu

module RoleCmp = Belt.Id.MakeComparable({
  let roleNum = role => switch(role) {
    | LaoChaoFeng => 0
    | YaoBuRan => 1
    | _ => 2
  }
  type t = role
  let cmp = (r1, r2) => roleNum(r1) - roleNum(r2)
})

let roles = [|
  XuYuan,
  FangZhen,
  MuHuJiaNai,
  HuangYanYan,
  LaoChaoFeng,
  YaoBuRan,
  ZhengGuoQu,
  JiYunFu,
|]

type action =
  | Init(int)
  | Dramatic(int, int)

type zodiacState =
  | Authentic
  | Fake
  | Unknown

type gameAction =
  /* maybe instead of int, use a zodiac variant here */
  | IndentifyZodiac(int)
  /* maybe instead of int, use a zodiac variant here */
  | VoteZodiac(int)
  | VotePlayer(int)

type player = {
  role:role,
  drugged: bool,
  parternerIndex: int,
  actionHistory: array(gameAction),
}

type game = {
  state: gameStep,
  activePlayer: int,
  playerCount: int,
  roles: array(role),
  players: array(player)
}


module InitState {
  let game = {
    state: Init,
    playerCount: 6,
    activePlayer: 0,
    roles: [||],
    players: [||],
  }
}

/* mostly, solely, for actions */
module Decode = {
  open Json.Decode

  let action = json =>
    switch (json |> field("type", string)) {
    | "init" => Some(Init(json |> field("playerCount", int)))
    | "dramatic_action" =>
      Some(Dramatic(json |> field("a", int), json |> field("b", int)))
    | _ => None
    }
}

module Encode = {
  open Json.Encode

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
    )

  let player = player =>
    object_([
      ("role", role(player.role)),
      ("drugged", bool(player.drugged)),
      ("parternerIndex", int(player.parternerIndex)),
      /* ("actionHistory", array) */
    ])

  let game = (state: game) =>
    object_([
      (
        "state",
        string(
          switch (state.state) {
          | Init => "init"
          | TurnAction => "turn_action"
          | VoteZodiac => "vote_zodiac"
          | VotePlayer => "vote_player"
          | End => "end"
          },
        ),
      ),
      ("active_player", int(state.activePlayer)),
      ("roles", array(role, state.roles)),
      ("player_count", int(state.playerCount)),
      ("players", array(player, state.players)),
    ])
}

let reduce' = (state: game, action) =>
  switch (action) {
  | Init(playerCount) => {
      let roles = Belt.Array.(roles |> slice(~offset=0, ~len=playerCount) |> shuffle);
      let roleIndexes = Belt.Array.mapWithIndex(roles, (i, role) => (role, i));
      let roleMap = Belt.Map.fromArray(roleIndexes, ~id=(module RoleCmp));
      let players = Belt.Array.map(roles, role => {
        role,
        drugged: false,
        actionHistory: [||],
        parternerIndex: switch(role) {
          | LaoChaoFeng => Belt.Map.getWithDefault(roleMap, YaoBuRan, -1)
          | YaoBuRan => Belt.Map.getWithDefault(roleMap, LaoChaoFeng, -1)
          | _ => -1
        },
      });
      {
        ...InitState.game,
        activePlayer: Js.Math.random_int(0, playerCount),
        playerCount,
        roles,
        players,
      }
    }
  | Dramatic(a, b) => {
      ...state,
      state: VotePlayer,
      playerCount: a * b,
    }
  }

let reduce = (state: option(game), jsAction) =>
  switch (state) {
  | Some(state') =>
    switch (Decode.action(jsAction)) {
    | Some(action) => reduce'(state', action)
    | None => state'
    }
  | None => InitState.game
  }

let toJs = action => action |> Encode.game

