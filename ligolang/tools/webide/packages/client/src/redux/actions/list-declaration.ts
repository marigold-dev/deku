import { getListDeclaration } from '../../services/api';
import { ActionType } from '../list-declaration';

export const ListDeclarationAction = (syntax, code) => {
  return (dispatch) =>
    new Promise((resolve) => {
      getListDeclaration(syntax, code)
        .then((response) => {
          dispatch({ type: ActionType.SetDefaultList, value: response });
          resolve(response);
        })
        .catch((error) => {
          resolve(error);
        });
    });
};
