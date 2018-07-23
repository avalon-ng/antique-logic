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

type treasureType =
  | Rat
  | Bull
  | Tiger
  | Rabbit
  | Dragon
  | Snake
  | Horse
  | Goat
  | Monkey
  | Chicken
  | Dog
  | Pig;

let allTreasureTypes = [|
  Rat,
  Bull,
  Tiger,
  Rabbit,
  Dragon,
  Snake,
  Horse,
  Goat,
  Monkey,
  Chicken,
  Dog,
  Pig,
|];

let treasureNum = treasure =>
  switch (treasure) {
  | Rat => 0
  | Bull => 1
  | Tiger => 2
  | Rabbit => 3
  | Dragon => 4
  | Snake => 5
  | Horse => 6
  | Goat => 7
  | Monkey => 8
  | Chicken => 9
  | Dog => 10
  | Pig => 11
  };
module TreasureCmp =
  Belt.Id.MakeComparable({
    type t = treasureType;
    let cmp = (r1, r2) => treasureNum(r1) - treasureNum(r2);
  });

type action =
  | Init(int)
  | StartTurn
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
  voteTokens: int,
  drugged: bool,
  blind: int,
  parternerIndex: int,
  actionHistory: array(gameAction),
};

type playerIndex = int;
type voteCount = int;
type vote = (playerIndex, voteCount);
type votes = list(vote);

type treasure = {
  type_: treasureType,
  votes,
  voteDone: bool,
  authentic: bool,
};

type game = {
  phase,
  round: int,
  activePlayer: int,
  playerCount: int,
  roles: array(role),
  players: array(player),
  remainingTreasures: array(treasureType),
  treasures: list((treasure, treasure, treasure, treasure)),
};

type meta = {playerIndex: int};

module InitState = {
  let game = {
    phase: Preparation,
    round: 0,
    playerCount: 6,
    activePlayer: 0,
    roles: [||],
    players: [||],
    remainingTreasures: [||],
    treasures: [],
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
    | "start_turn" => Some(StartTurn)
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

  let treasureType = t =>
    string(
      switch (t) {
      | Rat => "rat"
      | Bull => "bull"
      | Tiger => "tiger"
      | Rabbit => "rabbit"
      | Dragon => "dragon"
      | Snake => "snake"
      | Horse => "horse"
      | Goat => "goat"
      | Monkey => "monkey"
      | Chicken => "chicken"
      | Dog => "dog"
      | Pig => "pig"
      },
    );

  let player = player =>
    object_([
      ("role", role(player.role)),
      ("parternerIndex", int(player.parternerIndex)),
      /* ("drugged", bool(player.drugged)), */
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
      ("round", int(state.round)),
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
    let remainingTreasures = Belt.Array.shuffle(allTreasureTypes);
    let roleIndexes = Belt.Array.mapWithIndex(roles, (i, role) => (role, i));
    let roleMap = Belt.Map.fromArray(roleIndexes, ~id=(module RoleCmp));
    let players =
      Belt.Array.map(roles, role =>
        {
          role,
          voteTokens: 0,
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
      remainingTreasures,
      playerCount,
      roles,
      players,
    };

  /* StartTurn does following things
   * give each player 2 vote token
   * pick 4 more treasures, and assign their states
   * assign starting player
   */
  | StartTurn =>
    state.phase != TurnUpkeep ?
      state :
      {
        ...state,
        phase: Turn,
        round: state.round + 1,
        players:
          /* add 2 more vote tokens */
          Belt.Array.map(state.players, p =>
            {...p, voteTokens: p.voteTokens + 2}
          ),
        remainingTreasures:
          Belt.Array.slice(state.remainingTreasures, ~offset=4, ~len=12),
        treasures:
          switch (
            Belt.Array.slice(state.remainingTreasures, ~offset=0, ~len=4)
          ) {
          | [|t1, t2, t3, t4|] =>
            Belt.List.add(
              state.treasures,
              {
                let treasures = [
                  {type_: t1, votes: [], voteDone: false, authentic: true},
                  {type_: t2, votes: [], voteDone: false, authentic: true},
                  {type_: t3, votes: [], voteDone: false, authentic: false},
                  {type_: t4, votes: [], voteDone: false, authentic: false},
                ];
                let treasures =
                  Belt.List.sort(treasures, (a, b) =>
                    treasureNum(a.type_) - treasureNum(b.type_)
                  );
                switch (treasures) {
                | [t1, t2, t3, t4] => (t1, t2, t3, t4)
                | _ =>
                  let t = {
                    type_: Rat,
                    votes: [],
                    voteDone: true,
                    authentic: false,
                  };
                  (t, t, t, t);
                };
              },
            )
          | _ => state.treasures
          },
      }
  | Dramatic(a, b) => {...state, phase: VoteRole, playerCount: a * b}
  | IdentifyTreasure(_) => state
  };

let authorized = (index, state, action) =>
  switch (action) {
  | Init(_) => index == 0
  | StartTurn => index == 0
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
