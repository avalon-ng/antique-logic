const { toJs, reduce } = require('./Reducer.bs.js')

function makeGame(playerCount) {
  let state = null

  return function transition(playerIndex, action) {
    state = reduce(state, {...action, meta: {playerIndex}})
    const jsState = toJs(state)
    console.log(JSON.stringify(jsState, null, 2))
    return jsState
  }
}

const reducer = makeGame(6)

reducer(0, {
  type: 'init',
  playerCount: 6,
})

reducer(0, { type: 'prepare_turn' })
reducer(0, { type: 'start_turn' })

for(let i = 0; i < 6; i ++) {
  for(let j = 0; j < 4; j++) {
    reducer(i, {
      type: 'turn_action',
      action: 'identify_treasure',
      index: j,
    });
  }
}

