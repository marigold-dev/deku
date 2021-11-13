import { Dispatch } from 'redux';

import { dryRun, getErrorMessage } from '../../services/api';
import { AppState } from '../app';
import { DoneLoadingAction, UpdateLoadingAction } from '../loading';
import { ChangeOutputAction } from '../result';
import { CommandType } from '../types';
import { CancellableAction } from './cancellable';

export class DryRunAction extends CancellableAction {
  getAction() {
    return async (dispatch: Dispatch, getState: () => AppState) => {
      dispatch({
        ...new UpdateLoadingAction('Waiting for dry run results...'),
      });

      try {
        const { editor, dryRun: dryRunState } = getState();
        const result = await dryRun(
          editor.language,
          editor.code,
          dryRunState.entrypoint,
          dryRunState.parameters,
          dryRunState.storage
        );
        if (this.isCancelled()) {
          return;
        }
        dispatch({
          ...new ChangeOutputAction(result.output, CommandType.DryRun, false),
        });
      } catch (ex) {
        if (this.isCancelled()) {
          return;
        }
        dispatch({
          ...new ChangeOutputAction(
            `Error: ${getErrorMessage(ex)}`,
            CommandType.DryRun,
            true
          ),
        });
      }

      dispatch({ ...new DoneLoadingAction() });
    };
  }
}
