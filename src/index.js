const { toJs, reduce } = require('./Reducer.bs.js')

function makeGame(playerCount) {
  let state = null

  return function transition(playerIndex, action) {
    state = reduce(state, {...action, meta: {playerIndex}})
    const jsState = toJs(state)
    console.log(jsState)
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

