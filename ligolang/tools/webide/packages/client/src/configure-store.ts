import { applyMiddleware, createStore, Middleware } from 'redux';
import ReduxThunk from 'redux-thunk';

import rootReducer, { AppState } from './redux/app';

declare var defaultServerState: AppState | undefined;
 

export default function configureStore() {
  const store = createStore(
    rootReducer,
    applyMiddleware(ReduxThunk, cleanRouteOnAction)
  );

  return store;
}

const cleanRouteOnAction: Middleware = store => next => action => {
  const { share } = store.getState();
  next(action);
  const state = store.getState();
  if (
    share && share.link !== undefined &&
    state.share.link === undefined &&
    window.location.pathname !== '/'
  ) {
    window.history.replaceState({}, document.title, '/');
  }
};
