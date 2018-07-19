// const { gameStepToJs, gameStepsFromJs, gameReducer } = require('./Demo.bs.js');
const { fromJs, toJs, reduce } = require('./Reducer.bs.js');
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





let state = reduce(null, {
  type: 'init',
  playerCount: 8,
});
state = reduce(state, {
  type: 'dramatic_action',
  a: 1, b: 2,
});
state = reduce(state, {
  type: 'update_name',
  name: 'updated name',
});
// console.log(toJs(state));
