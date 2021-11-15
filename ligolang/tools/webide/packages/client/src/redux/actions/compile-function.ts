import { Dispatch } from 'redux';

import { compileExpression, getErrorMessage } from '../../services/api';
import { AppState } from '../app';
import { DoneLoadingAction, UpdateLoadingAction } from '../loading';
import { ChangeOutputAction } from '../result';
import { CommandType } from '../types';
import { CancellableAction } from './cancellable';

export class CompileFunctionAction extends CancellableAction {
  getAction() {
    return async (dispatch: Dispatch, getState: () => AppState) => {
      dispatch({ ...new UpdateLoadingAction('Compiling contract...') });

      try {
        const { editor, compileFunction } = getState();
        const michelsonCode = await compileExpression(
          editor.language,
          editor.code,
          compileFunction.functionName
        );

        if (this.isCancelled()) {
          return;
        }

        dispatch({
          ...new ChangeOutputAction(
            michelsonCode.result,
            CommandType.Compile,
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
            CommandType.Compile,
            true
          ),
        });
      }

      dispatch({ ...new DoneLoadingAction() });
    };
  }
}
