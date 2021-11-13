import { Dispatch } from 'redux';

import { compileContract, getErrorMessage } from '../../services/api';
import { AppState } from '../app';
import { DoneLoadingAction, UpdateLoadingAction } from '../loading';
import { ChangeOutputAction } from '../result';
import { CommandType } from '../types';
import { CancellableAction } from './cancellable';

export class CompileAction extends CancellableAction {
  getAction() {
    return async (dispatch: Dispatch, getState: () => AppState) => {
      dispatch({ ...new UpdateLoadingAction('Compiling contract...') });

      try {
        const { editor, compile: compileState } = getState();
        const michelsonCode = await compileContract(
          editor.language,
          editor.code,
          compileState.entrypoint,
          compileState.michelsonFormat
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
