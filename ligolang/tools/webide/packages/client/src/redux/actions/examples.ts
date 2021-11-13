import { getExampleList, getExample } from '../../services/api';
import { ActionType, ChangeSelectedAction } from '../examples';

export const ExampleListAction = () => {
  return (dispatch) =>
    new Promise((resolve) => {
      getExampleList().then((response) => {
        dispatch({ type: ActionType.SetDefaultList, value: response });
        resolve(response);
      });
    });
};

export const ExampleAction = (exampleId) => {
  return (dispatch) =>
    new Promise((resolve) => {
      getExample(exampleId).then((response) => {
        dispatch({ type: ChangeSelectedAction, value: response });
        resolve(response);
      });
    });
};
