// const { gameStepToJs, gameStepsFromJs, gameReducer } = require('./Demo.bs.js');
const { toJs, reduce } = require('./Reducer.bs.js');
// require('./JsonExample.bs.js');

// let state = gameReducer(null, { type: 'Init' });
// console.log(state);

// state = gameReducer(state, { type: 'End' });
// console.log(state);

// state = gameReducer(state, { type: 'End' });
// console.log(state);

// state = gameReducer(state, { type: 'End' });
// console.log(state);

// state = gameReducer(null, { type: 'Init' });
// console.log(state);


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
});

reducer({
  type: 'dramatic_action',
  a: 1, b: 2,
});

reducer({
  type: 'update_name',
  name: 'updated name',
});
// console.log(toJs(state));
