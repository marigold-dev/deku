import { Dispatch } from 'redux';

import { evaluateValue, getErrorMessage } from '../../services/api';
import { AppState } from '../app';
import { DoneLoadingAction, UpdateLoadingAction } from '../loading';
import { ChangeOutputAction } from '../result';
import { CommandType } from '../types';
import { CancellableAction } from './cancellable';

export class EvaluateValueAction extends CancellableAction {
  getAction() {
    return async (dispatch: Dispatch, getState: () => AppState) => {
      const { editor, evaluateValue: evaluateValueState } = getState();

      dispatch({
        ...new UpdateLoadingAction(
          `Evaluating "${evaluateValueState.entrypoint}" entrypoint...`
        ),
      });

      try {
        const result = await evaluateValue(
          editor.language,
          editor.code,
          evaluateValueState.entrypoint
        );

        if (this.isCancelled()) {
          return;
        }

        dispatch({
          ...new ChangeOutputAction(
            result.code,
            CommandType.EvaluateValue,
            false
          ),
        });
      } catch (ex) {
        if (this.isCancelled()) {
          return;
        }
        dispatch({
          ...new ChangeOutputAction(
            `Error: ${getErrorMessage(ex)}`,
            CommandType.EvaluateValue,
            true
          ),
        });
      }

      dispatch({ ...new DoneLoadingAction() });
    };
  }
}
