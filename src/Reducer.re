type phase =
  | Preparation
  | TurnUpkeep
  | Turn
  | Speak
  | VoteTreasure
  | VoteResult
  /* ^ maybe we can skip this? after above, go to TurnUpkeep */
  /* after VoteResult, if red side already got 6 points, red wins
     and can skip VoteRole phase */
  | VoteRole
  | EndGame;

/*
  let turn start as an action and we can do some setup accordingly
  things like,
  1. reset drugged state, expect for JiYunFu
  2. give each player 2 vote token
  3. pick 4 more treasures, and assign their states
  4. assign starting player
  ...and more...?
 */

type role =
  | XuYuan
  | FangZhen
  | MuHuJiaNai
  | HuangYanYan
  | JiYunFu
  | LaoChaoFeng
  | YaoBuRan
  | ZhengGuoQu;

module RoleCmp =
  Belt.Id.MakeComparable({
    let roleNum = role =>
      switch (role) {
      | LaoChaoFeng => 0
      | YaoBuRan => 1
      | _ => 2
      };
    type t = role;
    let cmp = (r1, r2) => roleNum(r1) - roleNum(r2);
  });

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

type action =
  | Init(int)
  | Dramatic(int, int)
  | IdentifyTreasure(int);

type treasureState =
  | Authentic
  | Fake
  | Unknown;

type gameAction =
  /* maybe instead of int, use a beast head variant here */
  | IdentifyTreasure(int)
  /* maybe instead of int, use a beast head variant here */
  | VoteTreasure(int)
  | VotePlayer(int);

type player = {
  role,
  drugged: bool,
  blind: int,
  parternerIndex: int,
  actionHistory: array(gameAction),
};

type game = {
  phase,
  activePlayer: int,
  playerCount: int,
  roles: array(role),
  players: array(player),
};

type meta = {playerIndex: int};

module InitState = {
  let game = {
    phase: Preparation,
    playerCount: 6,
    activePlayer: 0,
    roles: [||],
    players: [||],
  };
};

/* mostly, solely, for actions */
module Decode = {
  open Json.Decode;

  let meta = json => {playerIndex: json |> field("playerIndex", int)};

  let actionMeta = json => json |> field("meta", meta);

  let action = json =>
    switch (json |> field("type", string)) {
    | "init" => Some(Init(json |> field("playerCount", int)))
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

  let player = player =>
    object_([
      ("role", role(player.role)),
      ("drugged", bool(player.drugged)),
      ("parternerIndex", int(player.parternerIndex)),
      /* ("blind", int(player.blind)), */
      /* ("actionHistory", array) */
    ]);

  let game = (state: game) =>
    object_([
      (
        "phase",
        string(
          switch (state.phase) {
          | Preparation => "preparation"
          | TurnUpkeep => "turn_upkeep"
          | Turn => "turn"
          | Speak => "speak"
          | VoteTreasure => "vote_treasure"
          | VoteResult => "vote_result"
          | VoteRole => "vote_role"
          | EndGame => "end_game"
          },
        ),
      ),
      ("active_player", int(state.activePlayer)),
      ("roles", array(role, state.roles)),
      ("player_count", int(state.playerCount)),
      ("players", array(player, state.players)),
    ]);
};

let reduce' = (state: game, action) =>
  switch (action) {
  | Init(playerCount) =>
    let roles =
      Belt.Array.(roles |> slice(~offset=0, ~len=playerCount) |> shuffle);
    let roleIndexes = Belt.Array.mapWithIndex(roles, (i, role) => (role, i));
    let roleMap = Belt.Map.fromArray(roleIndexes, ~id=(module RoleCmp));
    let players =
      Belt.Array.map(roles, role =>
        {
          role,
          drugged: false,
          actionHistory: [||],
          blind:
            switch (role) {
            | MuHuJiaNai => Js.Math.random_int(0, 3)
            | HuangYanYan => Js.Math.random_int(0, 3)
            | _ => (-1)
            },
          parternerIndex:
            switch (role) {
            | LaoChaoFeng => Belt.Map.getWithDefault(roleMap, YaoBuRan, -1)
            | YaoBuRan => Belt.Map.getWithDefault(roleMap, LaoChaoFeng, -1)
            | _ => (-1)
            },
        }
      );
    {
      ...InitState.game,
      activePlayer: Js.Math.random_int(0, playerCount),
      playerCount,
      roles,
      players,
    };
  | Dramatic(a, b) => {...state, phase: VoteRole, playerCount: a * b}
  | IdentifyTreasure(_) => state
  };

let authorized = (index, state, action) =>
  switch (action) {
  | Init(_) => index == 0
  | IdentifyTreasure(_) => state.activePlayer == index
  | _ => true
  };

let reduce = (state: option(game), jsAction) => {
  let playerIndex = Decode.actionMeta(jsAction).playerIndex;

  switch (state) {
  | Some(state') =>
    switch (Decode.action(jsAction)) {
    | Some(action) =>
      authorized(playerIndex, state', action) ?
        reduce'(state', action) : state'
    | None => state'
    }
  | None => InitState.game
  };
};

let toJs = action => action |> Encode.game;
