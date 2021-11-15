import { getSharedFile } from '../../services/api';
import { ActionType } from '../share';

export const GcpFileAction = (hash) => {
  return (dispatch) =>
    new Promise((resolve) => {
      getSharedFile(hash).then((response) => {
        dispatch({ type: ActionType.GetSharedFile, value: response });
        resolve(response);
      });
    });
};
