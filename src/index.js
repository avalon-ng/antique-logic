const { toJs, reduce } = require('./Reducer.bs.js')

function makeGame(playerCount) {
  let state = null

  return function transform(action) {
    state = reduce(state, action)
    const jsState = toJs(state)
    console.log(jsState)
    return jsState
  }
}

const reducer = makeGame(6)

reducer({
  type: 'init',
  playerCount: 6,
})

reducer({
  type: 'dramatic_action',
  a: 1, b: 2,
})

