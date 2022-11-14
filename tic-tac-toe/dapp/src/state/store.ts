import { createStore, compose, applyMiddleware } from 'redux';
import { createLogger } from 'redux-logger';
import { install } from 'redux-loop';
import reducer, { initialState } from "./reducer";

const logger = createLogger({
    collapsed: true
});

const enhancer = compose(
    install(),
    applyMiddleware(logger),
);

const store = createStore(reducer as any, initialState, enhancer);

export default store;
